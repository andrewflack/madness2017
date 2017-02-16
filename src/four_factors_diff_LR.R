# "Four Factors" model

library(dplyr)
library(modelr)
library(ggplot2)
library(broom)

rsd <- read.csv("data/RegularSeasonDetailedResults.csv")

rsd_ff <- rsd %>% 
  mutate(Wposs = (Wfga - Wor + Wto + (4.75*Wfta))/2,
         Lposs = (Lfga - Lor + Lto + (4.75*Lfta))/2,
         Wfgeff = (Wfgm + (0.5*Wfgm3))/Wfga,
         Lfgeff = (Lfgm + (0.5*Lfgm3))/Lfga,
         Wto_pct = Wto/Wposs,
         Lto_pct = Lto/Lposs,
         Wor_pct = Wor/(Wor + Ldr),
         Lor_pct = Lor/(Lor + Wdr),
         Wft_pct = Wfta/Wfga,
         Lft_pct = Lfta/Lfga) %>% 
  select(Wteam, Wscore, 
         Lteam, Lscore, 
         Wposs, Lposs, 
         Wfgeff, Lfgeff, 
         Wto_pct, Lto_pct, 
         Wor_pct, Lor_pct, 
         Wft_pct, Lft_pct)

colnames(rsd_ff) <- c("t1", "t1score", 
                      "t2", "t2score", 
                      "t1poss", "t2poss", 
                      "t1fgeff", "t2fgeff", 
                      "t1to_pct", "t2to_pct", 
                      "t1or_pct", "t2or_pct", 
                      "t1ft_pct", "t2ft_pct")

rsd_ff2 <- rsd_ff %>% select(t2, t2score,
                             t1, t1score,
                             t2poss, t1poss,
                             t2fgeff, t1fgeff,
                             t2to_pct, t1to_pct,
                             t2or_pct, t1or_pct,
                             t2ft_pct, t1ft_pct)

colnames(rsd_ff2) <- c("t1", "t1score", 
                      "t2", "t2score", 
                      "t1poss", "t2poss", 
                      "t1fgeff", "t2fgeff", 
                      "t1to_pct", "t2to_pct", 
                      "t1or_pct", "t2or_pct", 
                      "t1ft_pct", "t2ft_pct")

rsd_ff_all <- rbind(rsd_ff, rsd_ff2)

rsd_ff_all <- rsd_ff_all %>% 
  mutate(t1win = ifelse(t1score > t2score, 1, 0),
                                    fgeff_diff = t1fgeff - t2fgeff,
                                    to_pct_diff = t1to_pct - t2to_pct,
                                    or_pct_diff = t1or_pct - t2or_pct,
                                    ft_pct_diff = t1ft_pct - t2ft_pct) %>% 
  select(fgeff_diff, to_pct_diff, or_pct_diff, ft_pct_diff, t1win)

model <- glm(t1win ~ ., data = rsd_ff_all, family = binomial)

model %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = reorder(term, estimate), y = estimate)) + 
  geom_point() + 
  geom_errorbar(aes(x = term, ymax = conf.high, ymin = conf.low)) + 
  coord_flip() + 
  theme_minimal() + 
  labs(y = "Estimate", x = NULL, 
       title = "Regression Coefficients", 
       subtitle = "Change in log odds of winning matchup for one unit change in variable")

fgeff_diff_model <- glm(t1win ~ fgeff_diff, data = rsd_ff_all, family = binomial)
to_pct_diff_model <- glm(t1win ~ to_pct_diff, data = rsd_ff_all, family = binomial)
or_pct_diff_model <- glm(t1win ~ or_pct_diff, data = rsd_ff_all, family = binomial)
ft_pct_diff_model <- glm(t1win ~ ft_pct_diff, data = rsd_ff_all, family = binomial)

ggsave("plots/fourfactors_regression_coefs.png")

fgeff_diff_grid <- rsd_ff_all %>% data_grid(fgeff_diff) %>% mutate(pred = predict(fgeff_diff_model, newdata = ., type = "response"))
to_pct_diff_grid <- rsd_ff_all %>% data_grid(to_pct_diff) %>% mutate(pred = predict(to_pct_diff_model, newdata = ., type = "response"))
or_pct_diff_grid <- rsd_ff_all %>% data_grid(or_pct_diff) %>% mutate(pred = predict(or_pct_diff_model, newdata = ., type = "response"))
ft_pct_diff_grid <- rsd_ff_all %>% data_grid(ft_pct_diff) %>% mutate(pred = predict(ft_pct_diff_model, newdata = ., type = "response"))

ggplot(rsd_ff_all, aes(x = fgeff_diff)) + 
  geom_jitter(aes(y = t1win), alpha = .01, height = .1) + 
  geom_line(data = fgeff_diff_grid, aes(y = pred)) + 
  scale_y_continuous("P(win|fg_eff_diff)", breaks = 0:1) +
  labs(x = NULL, title = "Four Factors Difference Model", subtitle = "Probability of winning a matchup given difference in effective fg %") +
  theme_minimal()

ggsave("plots/fourfactors_fgeff.png")

ggplot(rsd_ff_all, aes(x = to_pct_diff)) + 
  geom_jitter(aes(y = t1win), alpha = .01, height = .1) + 
  geom_line(data = to_pct_diff_grid, aes(y = pred)) + 
  scale_y_continuous("P(win|to_pct_diff)", breaks = 0:1) +
  labs(x = NULL, title = "Four Factors Difference Model", subtitle = "Probability of winning a matchup given difference in turnover rate") +
  theme_minimal()

ggsave("plots/fourfactors_to_pct.png")

ggplot(rsd_ff_all, aes(x = or_pct_diff)) + 
  geom_jitter(aes(y = t1win), alpha = .01, height = .1) + 
  geom_line(data = or_pct_diff_grid, aes(y = pred)) + 
  scale_y_continuous("P(win|or_pct_diff)", breaks = 0:1) +
  labs(x = NULL, title = "Four Factors Difference Model", subtitle = "Probability of winning a matchup given difference in offensive rebound %") +
  theme_minimal()

ggsave("plots/fourfactors_or_pct.png")

ggplot(rsd_ff_all, aes(x = ft_pct_diff)) + 
  geom_jitter(aes(y = t1win), alpha = .01, height = .1) + 
  geom_line(data = ft_pct_diff_grid, aes(y = pred)) + 
  scale_y_continuous("P(win|ft_pct_diff)", breaks = 0:1) +
  labs(x = NULL, title = "Four Factors Difference Model", subtitle = "Probability of winning a matchup given difference in free throw rate") +
  theme_minimal()

ggsave("plots/fourfactors_ft_pct.png")