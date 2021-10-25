# Tutorial 1: Introduction to nflfastR: https://www.youtube.com/watch?v=uw6PH1YiCtI&t=155s
# Tutorial 2: Data Viz With nflfastr: https://www.youtube.com/watch?v=tGaXtAAt6-4&t=255s
# Tutorial 3 (Today): https://github.com/tejseth/nfl-r-tutorials/blob/master/modeling.R

# Load needed packages
library(tidyverse)
library(ranger)
library(nflfastR)
library(ggthemes)
library(vip)

# Make our custom package
theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 16),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

# Load in play-by-play data
pbp <- load_pbp(2016:2021)

# Getting just field goals
field_goals <- pbp %>%
  filter(field_goal_attempt == 1) %>%
  mutate(made_fg = ifelse(field_goal_result == "made", 1, 0))

# Making the logistic regression model
log_fg <- glm(made_fg ~ kick_distance,
                  data = field_goals,
                  family = "binomial")

summary(log_fg)

# Checking the prediction probabilities
field_goals %>%
  mutate(pred_prob = log_fg$fitted.values) %>%
  ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred_prob), color = "black", size = 2) +
  geom_point(aes(y = made_fg, color = ifelse(made_fg == 1, "darkgreen", "darkred")), 
             alpha = 0.3) +
  scale_color_identity() +
  theme_reach() +
  labs(x = "Kick Distance",
       y = "Chance to Make Field Goal (0-1)")

# Getting field goals over expected
field_goals <- field_goals %>%
  mutate(pred_prob = log_fg$fitted.values) %>%
  mutate(fg_oe = made_fg - pred_prob)

fg_oe_stats <- field_goals %>%
  group_by(kicker_player_name) %>%
  summarize(kicks = n(),
            exp_fg_perc = mean(pred_prob),
            actual_fg_perc = mean(made_fg),
            fg_oe = 100*mean(fg_oe)) %>%
  filter(kicks >= 50) %>%
  arrange(-fg_oe)

# Plotting the kickers
fg_oe_stats %>%
  mutate(`Field Goal % Over Expected` = fg_oe) %>%
  ggplot(aes(x = exp_fg_perc, y = actual_fg_perc)) +
  geom_point(aes(size = kicks, fill = `Field Goal % Over Expected`), shape = 21, color = "black") +
  scale_fill_viridis_c() +
  ggrepel::geom_text_repel(aes(label = kicker_player_name), size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 1.5) +
  theme_reach() +
  labs(x = "Expected Field Goal Percentage",
       y = "Actual Field Goal Percentage",
       title = "Actual and Expected Field Goal Percentage, 2016-2021",
       subtitle = "Minimum of 50 kicks") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  guides(size = FALSE)
  
# Now we can make a linear model
pass_plays <- pbp %>%
  filter(pass == 1) %>%
  filter(!is.na(air_yards), !is.na(down), !is.na(score_differential),
         !is.na(ydstogo), !is.na(half_seconds_remaining)) 
  
# Select just the columns we want
pass_play_model <- pass_plays %>%
  select(air_yards, down, score_differential, ydstogo, half_seconds_remaining) %>%
  mutate(down = as.factor(down))

str(pass_play_model)
colSums(is.na(pass_play_model))

# Make the linear model
air_yards_lm <- lm(air_yards ~ down + score_differential + ydstogo + half_seconds_remaining, 
                   data = pass_play_model)

summary(air_yards_lm)

vip(air_yards_lm, num_features = 7) + theme_fivethirtyeight()

# Get predictions 

air_yard_preds <- data.frame(predict.lm(air_yards_lm, newdata = pass_play_model)) %>%
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_play_model.)

air_yards_projs <- cbind(pass_plays, air_yard_preds)

# Leaders in air yards over expected in 2021
ayoe_21 <- air_yards_projs %>%
  mutate(ayoe = air_yards - exp_air_yards) %>%
  filter(season == 2021) %>%
  group_by(passer) %>%
  summarize(passes = n(),
            avg_ayoe = mean(ayoe)) %>%
  filter(passes >= 80)




