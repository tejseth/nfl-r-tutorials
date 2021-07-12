library(tidyverse)

future::plan("multisession")
pbp <- nflfastR::load_pbp(2009:2020)
games <- espnscrapeR::get_sharpe_data(dataset = "games")

games <- games %>%
  filter(season >= 2009) %>%
  mutate(home_wins = case_when(
    home_score > away_score ~ 1,
    home_score == away_score ~ 0.5, 
    home_score < away_score ~ 0
  ),
  away_wins = 1 - home_wins)

home_qbs <- games %>%
  select(game_id, season, week, home_qb_name, home_team, home_wins) %>%
  rename(qb_name = home_qb_name,
         wins = home_wins,
         team = home_team)

away_qbs <- games %>%
  select(game_id, season, week, away_qb_name, away_team, away_wins) %>%
  rename(qb_name = away_qb_name,
         wins = away_wins,
         team = away_team)

all_qbs <- rbind(home_qbs, away_qbs)

qb_win_perc <- all_qbs %>%
  filter(!is.na(qb_name)) %>%
  group_by(qb_name) %>%
  summarize(games = n(),
            win_perc = mean(wins)) %>%
  filter(games >= 50) %>%
  arrange(-win_perc)

rushing_stats <- pbp %>%
  filter(rush == 1) %>%
  group_by(game_id, posteam) %>%
  summarize(rush_epa = mean(epa)) 

def_stats <- pbp %>%
  group_by(game_id, defteam) %>%
  summarize(def_epa = mean(epa))

all_stats <- rushing_stats %>%
  left_join(def_stats, by = c("game_id", "posteam" = "defteam"))

all_stats <- all_stats %>%
  mutate(total_help = rush_epa - def_epa) %>%
  left_join(all_qbs, by = c("game_id", "posteam" = "team"))

total_help_stats <- all_stats %>%
  filter(!is.na(total_help)) %>%
  filter(!is.na(qb_name)) %>%
  group_by(qb_name) %>%
  summarize(avg_total_help = mean(total_help, na.rm = T))

qb_win_perc <- qb_win_perc %>%
  left_join(total_help_stats, by = c("qb_name"))

qb_win_perc <- qb_win_perc %>%
  mutate(color = case_when(
    qb_name == "Matthew Stafford" ~ "navy",
    qb_name == "Jared Goff" ~ "#FFA300",
    TRUE ~ "gray"
  ))

qb_win_perc %>%
  ggplot(aes(x = avg_total_help, y = win_perc)) +
  geom_point(aes(fill = color), color = "black", shape = 21, size = 5) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.6, se = FALSE) +
  ggrepel::geom_text_repel(aes(label = qb_name), size = 5, box.padding = 0.3) +
  theme_reach() +
  labs(x = "Average Total Help (Rush EPA/Play + Defensive EPA/Play)",
       y = "Quarterback's Win Percentage",
       title = "How Much Help Each Quarterback Has Gotten and How Often They Win",
       subtitle = "2009-2020, total help doesn't account for pass blocking and receiving play but only rushing and defense",
       caption = "By Tej Seth | @mfbanalytics")
ggsave('stafford-goff.png', width = 15, height = 10, dpi = "retina")  
  
  
