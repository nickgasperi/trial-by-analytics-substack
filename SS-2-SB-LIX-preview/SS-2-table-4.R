# load packages
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(gt)
library(webshot2)
library(data.table)
library(gtExtras)

# load data
nfldata = load_pbp(2024)

# filter data
sbtdsdata = nfldata %>%
  filter(week < 22,
         !is.na(touchdown),
         !is.na(posteam),
         td_player_id == "00-0034844" | td_player_id == "00-0036389" |
         td_player_id == "00-0030506" | td_player_id == "00-0033923" |
         td_player_id == "00-0035676" | td_player_id == "00-0036912" |
         td_player_id == "00-0037197") %>%
  group_by(td_player_id, posteam) %>%
  summarize(td = sum(touchdown == 1 &
                     td_team == posteam)) %>%
  print(n = Inf)

# create column for reg. season games played
sbtdsdata$gpreg = c(16, 13, 16, 13, 15, 13, 7)
# create column for postseason games played
sbtdsdata$gppost = c(2, 2, 3, 3, 3, 3, 2)
# create column for td per game
sbtdsdata$tdperg = sbtdsdata$td/(sbtdsdata$gpreg + sbtdsdata$gppost)
# create column for anytime TD scorer odds
sbtdsdata$tdodds = c("+125", "+130", "-210", "+175", "-115", "+230", "+270")

# view updated tibble
sbtdsdata

# drop columns not needed in table, then view updated tibble
sbtdsdata = sbtdsdata %>%
  select(td_player_id, td, tdperg, tdodds) %>%
  print(n = Inf)

# ungroup data & order by most tot. tds
sbtdsdata = sbtdsdata %>%
  ungroup() %>%
  arrange(-td)

# create table
sbtdsplot1 = gt(sbtdsdata) %>%
  cols_label(td_player_id = "",
             td = "Tot. TDs",
             tdperg = "TD/Game",
             tdodds = "TD Odds") %>%
  cols_align(align = "center") %>%
  fmt_number(columns = "tdperg",
             decimals = 2) %>%
  tab_header(title = "2025 SUPER BOWL",
             subtitle = "ANYTIME TD ODDS") %>%
  tab_spanner(label = "'24 Reg & Post Season",
              columns = c("td", "tdperg")) %>%
  tab_spanner(label = "@ Caesars",
              columns = "tdodds") %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | Data @nflfastR")) %>%
  tab_options(footnotes.font.size = 12) %>%
  gt_nfl_headshots(columns = "td_player_id", height = 65) %>%
  gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = "tdodds"))

# view table
sbtdsplot1

# save table
# use 'expand' to prevent sides of table from being cut out of .png export
sbtdsplot1 %>% gtsave("SubSt2.6 - gt_sb_td_odds.png", expand = 12)
