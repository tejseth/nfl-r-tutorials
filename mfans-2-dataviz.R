#install.packages("ggthemes")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)

# Not to tej: hold command + to zoom in so it's easier to see the code

# Loading in the play-by-play data
future::plan("multisession")
pbp <- nflfastR::load_pbp(2014:2021)

# cleaning the data
pbp_rp <- pbp %>%
  filter(pass == 1 | rush == 1) 

# getting the pass rate over expected by season
proe_season <- pbp_rp %>%
  group_by(posteam, season) %>%
  summarize(proe = mean(pass_oe, na.rm = T)) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# inputting a team
team = "DAL"

team_proe <- proe_season %>%
  filter(posteam == team)
league_proe <- proe_season %>%
  filter(posteam != team)

# making the graph
ggplot() +
  geom_jitter(data = league_proe, aes(x = season, y = proe, fill = team_color, color = team_color2), 
              alpha = 0.3, width = 0.05, size = 4, shape = 21) + #setting the league pass rate over expected
  geom_line(data = team_proe, aes(x = season, y = proe, color = team_color2), size = 2) +
  geom_image(data = team_proe, aes(x = season, y = proe, image = team_logo_espn), asp = 16/9, size = 0.06) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_fivethirtyeight() +
  geom_hline(yintercept = 0) +
  labs(x = "Season",
       y = "Pass Rate Over Expected",
       title = paste0(team, "'s Pass Rate Over Expected, 2014-2021"),
       subtitle = "Other teams in the NFL shown for reference",
       caption = "By Tej Seth | @tejfbanalytics | M-FANS") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.text = element_text(size = 13))

# save the plot
ggsave('proe-1.png', width = 15, height = 10, dpi = "retina")

#################################################################################

# Picking a list of teams
teams <- c("GB", "MIN", "DET", "CHI")

# getting dataset for the teams
teams_proe <- proe_season %>%
  filter(posteam %in% teams)

# making the facet plot
teams_proe %>%
  ggplot(aes(x = season, y = proe)) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.9) +
  geom_image(aes(image = team_logo_espn, y = ifelse(proe > 0, proe + 1, proe - 1)), asp = 16/9, size = 0.06) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(~posteam) +
  theme_fivethirtyeight() +
  labs(x = "Season",
       y = "Pass Rate Over Expected",
       title = "Pass Rate Over Expected in the NFC North, 2014-2021",
       caption = "By Tej Seth | @tejfbanalytics | M-FANS") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.text = element_text(size = 13),
        panel.grid.major.x = element_line(size = 0.1),
        strip.text.x = element_text(size = 14))
ggsave('nfc-north-proe.png', wkdth = 15, height = 10, dpi = "retina")

#################################################################################

# getting passing play-by-play
pbp_p <- pbp %>%
  filter(season_type == "REG") %>%
  filter(qb_dropback == 1) %>%
  filter(!is.na(epa)) %>%
  group_by(passer) %>%
  mutate(total_passes = n()) %>%
  ungroup() %>%
  filter(total_passes >= 200)

# Getting cumulative dropback numbers and EPA
pbp_p$dropback_num <- ave(pbp_p$epa, pbp_p$passer, FUN = seq_along)
pbp_p$csum <- ave(pbp_p$epa, pbp_p$passer, FUN=cumsum)

# filtering for just the passer we need
mahomes_stats <- pbp_p %>%
  filter(passer == "P.Mahomes")
rosen_stats <- pbp_p %>%
  filter(passer == "J.Rosen")

# Getting the specific color for the chiefs
teams_colors_logos %>% 
  filter(team_abbr %in% c("KC", "MIA")) %>% 
  dplyr::select(team_abbr, team_color)

pbp_p %>% 
  ggplot(aes(x = dropback_num, y = csum)) +
  geom_smooth(aes(group = passer), color = "gray", se = FALSE, size = 1.5) +
  geom_smooth(data = mahomes_stats, aes(group = passer), color = "#e31837", se = FALSE, size = 3) +
  geom_smooth(data = rosen_stats, aes(group = passer), color = "#008e97", se = FALSE, size = 3) +
  theme_fivethirtyeight() +
  labs(x = "Dropback Number",
       y = "Cumulative EPA",
       title = "Each Passer's Cumulative EPA, 2014-2021",
       subtitle = "All other passers with at least 200 dropbacks in gray",
       caption = "By Tej Sethh | @tejfbanalytics | M-FANS") +
  annotate("text", x = 2250, y = 620, label = "P.Mahomes", size = 5) +
  annotate("text", x = 750, y = -200, label = "J.Rosen", size = 5) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.text = element_text(size = 13))
ggsave('mahomes-rosen.png', width = 15, height = 10, dpi = "retina")



