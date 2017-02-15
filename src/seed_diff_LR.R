library(dplyr)
library(modelr)
library(ggplot2)

# read in seeds
seeds <- read.csv("data/TourneySeeds.csv")
seeds$Seed <- as.character(seeds$Seed)
seeds <- seeds %>% separate(Seed, into = c("region", "seed"), sep = 1)

# read in tournament results
t_results <- read.csv("data/TourneyCompactResults.csv")

# add seeds to tournament results
df <- t_results %>% 
  left_join(seeds, by = c("Wteam" = "Team", "Season")) %>% 
  rename(Wseed = seed) %>% 
  select(-region) %>% 
  left_join(seeds, by = c("Lteam" = "Team", "Season")) %>% 
  rename(Lseed = seed) %>% 
  select(-region, -Daynum, -Wloc, -Numot)

# remove a and b from play-in seeds
df$Wseed <- as.numeric(gsub("a|b", "", df$Wseed))
df$Lseed <- as.numeric(gsub("a|b", "", df$Lseed))

# make col names generic
colnames(df) <- c("season", "t1", "t1score", "t2", "t2score", "t1seed", "t2seed")

# duplicate df, swap order of columns, and rename
df2 <- df %>% select(season, t2, t2score, t1, t1score, t2seed, t1seed)
colnames(df2) <- c("season", "t1", "t1score", "t2", "t2score", "t1seed", "t2seed")

# bind together
df_all <- rbind(df, df2)
df_all <- df_all %>% mutate(seed_diff = t2seed - t1seed, 
                            t1_win = ifelse(t1score > t2score, 1, 0))

# fit logistic regression and plot
model <- glm(t1_win ~ seed_diff, data = df_all, family = binomial)

grid <- df_all %>% data_grid(seed_diff) %>% mutate(pred = predict(model, newdata = ., type = "response"))

ggplot(df_all, aes(x = seed_diff)) + 
  geom_jitter(aes(y = t1_win), alpha = .1, height = .1) + 
  geom_line(data = grid, aes(y = pred)) + 
  scale_y_continuous("P(win|seed_diff)", breaks = 0:1) +
  labs(x = NULL, title = "Seed Difference Model", subtitle = "Probability of winning a matchup given seed difference alone") +
  theme_minimal()

ggsave("seed_diff_LR.png")