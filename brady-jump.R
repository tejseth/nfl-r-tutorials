library(tidyverse)
library(nflfastR)
library(ggthemes)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text = element_text(size = 12)
    )
}

pbp <- nflfastR::load_pbp(2020)

pbp_p <- pbp %>%
  filter(qb_dropback == 1) %>%
  filter(!is.na(epa)) %>%
  group_by(passer) %>%
  mutate(total_passes = n()) %>%
  ungroup() %>%
  filter(total_passes >= 200)

pbp_p$dropback_num <- ave(pbp_p$epa, pbp_p$passer, FUN = seq_along)

pbp_p$csum <- ave(pbp_p$epa, pbp_p$passer, FUN=cumsum)

brady <- pbp_p %>%
  filter(passer == "T.Brady")

pbp_p %>% 
  ggplot(aes(x = dropback_num, y = csum)) +
  geom_smooth(aes(group = passer), color = "gray", se = FALSE, size = 1.5) +
  geom_smooth(data = brady, aes(group = passer), color = "#d50a0a", se = FALSE, size = 4) +
  theme_reach() +
  geom_vline(xintercept = 362, linetype = "dashed") +
  labs(x = "Dropback Number",
       y = "Cumulative EPA",
       title = "Brady's Cumulative EPA Peaked Towards the End of the Season",
       subtitle = "All other passers with at least 200 dropbacks in gray",
       caption = "By Tej Sethh | @mfbanaltyics | nflfastR") +
  annotate("text", x = 350, y = 180, label = "Antonio Brown joins", size = 5, angle = 90)
ggsave('brady-jump.png', width = 15, height = 10, dpi = "retina")
  
