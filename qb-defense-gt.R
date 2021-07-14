library(tidyverse)
library(nflfastR)
library(gt)

future::plan("multisession")
pbp <- nflfastR::load_pbp(1999:2020)

pbp_dp <- pbp %>%
  filter(!is.na(passer)) %>%
  filter(!is.na(epa)) %>%
  filter(qb_dropback == 1) 

qb_stats <- pbp_dp %>%
  group_by(passer, defteam) %>%
  summarize(plays = n(),
            epa_per_play = round(mean(epa), 3)) %>%
  filter(plays >= 500) %>%
  arrange(-epa_per_play) %>%
  ungroup() %>%
  mutate(rank = row_number()) %>%
  filter(rank < 16) %>%
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>%
  mutate(vs = "vs.") %>%
  select(rank, passer, vs, team_wordmark, plays, epa_per_play)

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    )}

qb_games <- qb_stats %>% gt() %>%
  text_transform(
    locations = cells_body(c(team_wordmark)),
    fn = function(x){
      web_image(
        url = x,
        height = px(35)
      )
    }
  ) %>% 
  cols_label(
    rank = "Rank",
    passer = "Quarterback",
    vs = "",
    team_wordmark = "Defense",
    plays = "Plays",
    epa_per_play = "EPA/Play") %>%
  data_color(
    columns = c(epa_per_play),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  tab_source_note(
    source_note = md("By Tej Seth | @mfbanalytics <br>Inspiration: @thomas_mock")
  ) %>% 
  opt_align_table_header(align = "center") %>%
  tab_header(
    title = md("**The Top Quarterback Performances Against a Certain Defense**"),
    subtitle = md("1999-2020, minimum of 500 plays against the same team")) %>% 
  opt_row_striping() %>%
  gt_theme_538(table.width = px(550))
gtsave(qb_games, "qb_games.png")
