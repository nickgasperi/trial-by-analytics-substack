# load packages
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(gt)
library(webshot2)

# load data
nfldata2 = load_pbp(2018:2024)

# filter data
kcplayoffs = nfldata2 %>%
  filter(season_type == "POST",
         penalty_team == "KC",
         season <= 2023) %>%
  group_by(season) %>%
  summarize(penalties = n(),
            pen_yds = sum(penalty_yards)) %>%
  print(n = Inf)

# add 'games' since not all teams play equal # of games
# allows us to calculate per game stats
kcplayoffs$games = c(2, 3, 3, 3, 3, 4)

# calculate per game columns
kcplayoffs$pen_pg = kcplayoffs$penalties/kcplayoffs$games
kcplayoffs$pen_yds_pg = kcplayoffs$pen_yds/kcplayoffs$games

# view updated tibble
kcplayoffs

# add league postseason averages for each year for penalties and yds
kcplayoffs$post_avg_pen = (c(5.64, 5.45, 4.35, 4.73, 4.69, 4.81))
kcplayoffs$post_avg_yds = (c(44.23, 44.32, 34.46, 37.73, 34.54, 39.12))

# filter to drop original total columns
kcplayoffs = kcplayoffs %>%
  select(season, games, pen_pg, post_avg_pen, pen_yds_pg, post_avg_yds) %>%
  print(width = Inf)

# create postseason penalty table
tblkcplayoffs = gt(kcplayoffs) %>%
  fmt_number(columns = c(pen_pg, pen_yds_pg),
             decimals = 2) %>%
  cols_label(pen_pg = "penalties",
             post_avg_pen = "nfl_avg_pen",
             pen_yds_pg = "pen_yds",
             post_avg_yds = "nfl_avg_yds",
             .fn = md) %>%
  tab_header(title = md("**Postseason Penalties & Penalty Yards by Season**"),
             subtitle = md("**KC vs. NFL Averages**")) %>%
  tab_spanner(md("*per game*"), c("pen_pg", "post_avg_pen", "pen_yds_pg", "post_avg_yds")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = season == 2023 |
                                          season == 2022 |
                                          season == 2019)) %>%
  tab_footnote(footnote = md("*bold = won Super Bowl*")) %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | Data @nflfastR"))

# view table
tblkcplayoffs

# save table
tblkcplayoffs %>%gtsave("SubSt1.3 - gt_post_pens.png")