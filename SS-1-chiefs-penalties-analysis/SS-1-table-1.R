# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(gt)
library(webshot2)

# load data
nfldata2 = load_pbp(2018:2024)

# filter data
chiefsgr = nfldata2 %>%
  filter(penalty_team == "KC",
         season_type == "REG") %>%
  group_by(season) %>%
  summarize(penalties = n(),
            pen_yds = sum(penalty_yards)) %>%
  print(n = Inf)

# add league averages for each year for penalties and yds
chiefsgr$nfl_avg_p = (c(108, 108, 90, 101, 95, 97, 110))
chiefsgr$nfl_avg_y = (c(920, 917, 779, 867, 781, 801, 888))

# add diff columns for both varibales
chiefsgr$diff_pen = chiefsgr$penalties-chiefsgr$nfl_avg_p
chiefsgr$diff_yds = chiefsgr$pen_yds-chiefsgr$nfl_avg_y

# view updated tibble
chiefsgr

# reorder columns
chiefsgr = chiefsgr %>%
  relocate(nfl_avg_p, .after = penalties) %>%
  relocate(diff_pen, .after = nfl_avg_p) %>%
  relocate(nfl_avg_y, .after = pen_yds) %>%
  relocate(diff_yds, .after = nfl_avg_y) %>%
  print(n = Inf)


# create table
# use md() to add bold with '**' and italics with '*'
tblchiefsgr = gt(chiefsgr) %>%
  tab_header(title = md("**Penalties & Penalty Yards by Season**"),
             subtitle = md("**KC vs. NFL Averages**")) %>%
  tab_spanner("KC", c("season", "penalties", "nfl_avg_p", "diff_pen",
                      "pen_yds", "nfl_avg_y", "diff_yds")) %>%
  tab_footnote(footnote = md("*By Nick Gasperi | @tbanalysis | Data @nflfastR*")) %>%
  gt_nfl_wordmarks(locations = cells_column_spanners("KC"))

# save table
tblchiefsgr %>%gtsave("SubSt1.2 - gt_pens.png")