library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#### ingest national bracket from ESPN and clean up
espn <- read_html("http://games.espn.com/tournament-challenge-bracket/2017/en/whopickedwhom")

crowd_picks_raw <- espn %>%
  html_nodes(".seed , .percentage, .teamName") %>%
  html_text() %>% 
  matrix(ncol=18, byrow=TRUE) %>% 
  data.frame()

colnames(crowd_picks_raw) <- c("seed", "team", "R32", "seed2", "team2", "R16", "seed3", "team3", "R8", "seed4", "team4", "R4", "seed5", "team5", "NCG", "seed6", "team6", "title")

df1 <- crowd_picks_raw %>% select(seed, team, R32)
df2 <- crowd_picks_raw %>% select(team2, R16)
df3 <- crowd_picks_raw %>% select(team3, R8)
df4 <- crowd_picks_raw %>% select(team4, R4)
df5 <- crowd_picks_raw %>% select(team5, NCG)
df6 <- crowd_picks_raw %>% select(team6, title)

crowd_picks <- df1 %>% 
  left_join(df2, by = c("team" = "team2")) %>% 
  left_join(df3, by = c("team" = "team3")) %>% 
  left_join(df4, by = c("team" = "team4")) %>% 
  left_join(df5, by = c("team" = "team5")) %>% 
  left_join(df6, by = c("team" = "team6"))

crowd_picks[,3:8] <- as.data.frame(apply(crowd_picks[,3:8], 2, function(y) as.numeric(gsub("%", "", y))))

#### ingest fivethirtyeight forecast and clean up
# fte_forecast <- read.csv(url("http://projects.fivethirtyeight.com/march-madness-api/2016/fivethirtyeight_ncaa_forecasts.csv"))
download.file("http://projects.fivethirtyeight.com/march-madness-api/2017/fivethirtyeight_ncaa_forecasts.csv", destfile = "data/fte2017.csv")
fte_forecast <- read.csv("data/fte2017.csv")

fte_forecast <- fte_forecast %>% 
  filter(gender == "mens") %>% 
  mutate(forecast_date = ymd(forecast_date)) %>% 
  filter(forecast_date == max(forecast_date)) %>%
  # filter(rd1_win == 1) %>% 
  select(team_name, team_region, rd2_win, rd3_win, rd4_win, rd5_win, rd6_win, rd7_win)

colnames(fte_forecast) <- c("team", "team_region", "R32", "R16", "R8", "R4", "NCG", "title")

#### fuzzy match team names from espn to fivethirtyeight
fte_names <- as.character(unique(fte_forecast$team))
espn_names <- as.character(unique(crowd_picks$team))

# https://gist.github.com/goldingn/80c82f2886debeb927a5
fuzzyMatch <- function (a, b) {
  
  # no-frills fuzzy matching of strings between character vectors
  # `a` and `b` (essentially a wrapper around a stringdist function)
  # The function returns a two column matrix giving the matching index
  # (as `match` would return) and a matrix giving the distances, so you
  # can check how well it did on the hardest words.
  # Warning - this uses all of your cores.
  
  # load the stringdist package
  require (stringdist)
  
  # calculate a jaccard dissimilarity matrix 
  distance <- stringdistmatrix(a,
                               b,
                               method = 'jaccard')
  
  # find the closest match for each
  match <- apply(distance, 1, which.min)
  
  # find how far away these were
  dists <- apply(distance, 1, min)
  
  # return these as a two-column matrix
  return (cbind(match = match,
                distance = dists))
  
}

match_results <- fuzzyMatch(fte_names, espn_names)

match_names <- data.frame(fte_names, espn_names = espn_names[as.data.frame(match_results)$match], distance = as.data.frame(match_results)$distance)

# check which names need to be changed in the crowd_picks data structure in order to match
match_names %>% filter(distance > 0) %>% arrange(desc(distance))

# make a few changes, then convert team back to factor
crowd_picks$team <- as.character(crowd_picks$team)
crowd_picks[crowd_picks$team == "Pitt", "team"] <- "Pittsburgh"
crowd_picks[crowd_picks$team == "FGCU", "team"] <- "Florida Gulf Coast"
crowd_picks[crowd_picks$team == "VCU", "team"] <- "Virginia Commonwealth"
crowd_picks[crowd_picks$team == "UVA", "team"] <- "Virginia"
crowd_picks[crowd_picks$team == "UConn", "team"] <- "Connecticut"
crowd_picks[crowd_picks$team == "Cal", "team"] <- "California"
crowd_picks[crowd_picks$team == "USC", "team"] <- "Southern California"
crowd_picks[crowd_picks$team == "UNC", "team"] <- "North Carolina"
crowd_picks[crowd_picks$team == "UNC Asheville", "team"] <- "North Carolina-Asheville"
crowd_picks[crowd_picks$team == "UNC Wilmington", "team"] <- "North Carolina-Wilmington"
crowd_picks$team <- as.factor(crowd_picks$team)

# check again
fte_names <- as.character(unique(fte_forecast$team))
espn_names <- as.character(unique(crowd_picks$team))
match_results <- fuzzyMatch(fte_names, espn_names)
match_names <- data.frame(fte_names, espn_names = espn_names[as.data.frame(match_results)$match], distance = as.data.frame(match_results)$distance)
match_names %>% filter(distance > 0) %>% arrange(desc(distance))

# when satisfied, settle on one set to be the final name (in this case, ESPN)
match_names$final_name <- match_names$espn_names

fte_forecast <- fte_forecast %>% 
  left_join(match_names[, c("fte_names", "final_name")], by = c("team" = "fte_names")) %>% 
  select(final_name, team_region, R32, R16, R8, R4, NCG, title) %>% 
  rename(team = final_name)

# tidy both data frames
crowd_picks_tidy <- crowd_picks %>% gather(round, prob, 3:8)
crowd_picks_tidy$round <- ordered(crowd_picks_tidy$round, levels = c("R32", "R16", "R8", "R4", "NCG", "title"))
# crowd_picks_tidy %>% ggplot(aes(x = round, y = prob)) + geom_line(aes(group = team))

fte_tidy <- fte_forecast %>% gather(round, prob, 2:8, -team_region)
fte_tidy$round <- ordered(fte_tidy$round, levels = c("R32", "R16", "R8", "R4", "NCG", "title"))

# combine
combined_df <- fte_tidy %>% 
  left_join(crowd_picks_tidy, by = c("team", "round")) %>% 
  rename(fte_prob = prob.x, crowd_prob = prob.y) %>% 
  mutate(crowd_prob = crowd_prob/100) %>% 
  mutate(fte_ep = ifelse(round == "R32", 10*fte_prob, 
                         ifelse(round == "R16", (10+20)*fte_prob, 
                                ifelse(round == "R8", (10+20+40)*fte_prob, 
                                       ifelse(round == "R4", (10+20+40+80)*fte_prob, 
                                              ifelse(round == "NCG", (10+20+40+80+160)*fte_prob, 
                                                     (10+20+40+80+160+320)*fte_prob)))))) %>% 
  mutate(crowd_ep = ifelse(round == "R32", 10*crowd_prob, 
                           ifelse(round == "R16", (10+20)*crowd_prob, 
                                  ifelse(round == "R8", (10+20+40)*crowd_prob, 
                                         ifelse(round == "R4", (10+20+40+80)*crowd_prob, 
                                                ifelse(round == "NCG", (10+20+40+80+160)*crowd_prob, 
                                                       (10+20+40+80+160+320)*crowd_prob)))))) %>% 
  mutate(delta = fte_ep - crowd_ep) %>% 
  mutate(evoc = delta * fte_prob) %>% 
  arrange(desc(evoc))

write.csv(combined_df, paste0("data/bracket16_", Sys.Date(), ".csv"), row.names = FALSE)




# wins above crowd expectation
combined_df %>% 
  group_by(team) %>% 
  summarize(WACE = sum(fte_prob-crowd_prob)) %>% 
  ggplot(aes(x = reorder(team, WACE), y = WACE)) + 
  geom_point() + 
  geom_segment(aes(x = team, y = WACE, xend = team, yend = 0)) + 
  coord_flip() + 
  theme_minimal() +
  labs(x = NULL, y = "Wins Above Crowd Expectation")
