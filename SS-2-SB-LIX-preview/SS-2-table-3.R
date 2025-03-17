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

# create tibble 1 - regular season targets
sbwrdata1 = nfldata %>%
  filter(season_type == "REG",
         !is.na(receiver_player_id),
         posteam == "KC" | posteam == "PHI",
         receiver_player_name == "T.Kelce" | receiver_player_name == "X.Worthy" |
         receiver_player_name == "D.Hopkins" | receiver_player_name == "N.Gray" |
         receiver_player_name == "A.Brown" | receiver_player_name == "D.Smith" |
         receiver_player_name == "D.Goedert" | receiver_player_name == "S.Barkley") %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(targets_reg = n()) %>%
  print(n = Inf)

# create tibble 2 - regular season receptions
sbwrdata2 = nfldata %>%
  filter(season_type == "REG",
         !is.na(receiver_player_id),
         !is.na(receiving_yards),
         posteam == "KC" | posteam == "PHI",
         receiver_player_name == "T.Kelce" | receiver_player_name == "X.Worthy" |
           receiver_player_name == "D.Hopkins" | receiver_player_name == "N.Gray" |
           receiver_player_name == "A.Brown" | receiver_player_name == "D.Smith" |
           receiver_player_name == "D.Goedert" | receiver_player_name == "S.Barkley") %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(rec_reg = n()) %>%
  print(n = Inf)

# join the regular season tibbles
sbwrdata3 = sbwrdata1 %>%
  left_join(sbwrdata2, by = c("receiver_player_id", "receiver_player_name")) %>%
  print(n = Inf)

# add games played for each player, then calc per game stats
sbwrdata3$gp_reg = c(16, 10, 10, 16, 13, 17, 13, 17)
sbwrdata3$targets_per_reg = sbwrdata3$targets_reg/sbwrdata3$gp_reg
sbwrdata3$rec_per_reg = sbwrdata3$rec_reg/sbwrdata3$gp_reg

# filter out unwanted columns
sbwrdata3 = sbwrdata3 %>%
  select(receiver_player_id, targets_per_reg, rec_per_reg) %>%
  print(n = Inf)

# repeat the same tibbles, but include only data since week 15
# create tibble 4 -  targets since wk 15
sbwrdata4 = nfldata %>%
  filter(week >= 15,
         !is.na(receiver_player_id),
         posteam == "KC" | posteam == "PHI",
         receiver_player_name == "T.Kelce" | receiver_player_name == "X.Worthy" |
         receiver_player_name == "D.Hopkins" | receiver_player_name == "N.Gray" |
         receiver_player_name == "A.Brown" | receiver_player_name == "D.Smith" |
         receiver_player_name == "D.Goedert" | receiver_player_name == "S.Barkley") %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(targets_late = n()) %>%
  print(n = Inf)

# create tibble 5 - receptions since wk 15
sbwrdata5 = nfldata %>%
  filter(week >15,
         !is.na(receiver_player_id),
         !is.na(receiving_yards),
         posteam == "KC" | posteam == "PHI",
         receiver_player_name == "T.Kelce" | receiver_player_name == "X.Worthy" |
           receiver_player_name == "D.Hopkins" | receiver_player_name == "N.Gray" |
           receiver_player_name == "A.Brown" | receiver_player_name == "D.Smith" |
           receiver_player_name == "D.Goedert" | receiver_player_name == "S.Barkley") %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(rec_late = n()) %>%
  print(n = Inf)

# join the regular season tibbles
sbwrdata6 = sbwrdata4 %>%
  left_join(sbwrdata5, by = c("receiver_player_id", "receiver_player_name")) %>%
  print(n = Inf)

# add games played for each player, then calc per game stats
sbwrdata6$gp_late = c(5, 5, 4, 6, 6, 6, 6, 5)
sbwrdata6$targets_per_late = sbwrdata6$targets_late/sbwrdata6$gp_late
sbwrdata6$rec_per_late = sbwrdata6$rec_late/sbwrdata6$gp_late

# filter out unwanted columns
sbwrdata6 = sbwrdata6 %>%
  select(receiver_player_id, targets_per_late, rec_per_late) %>%
  print(n = Inf)

# join tibbles 3 and 6
sbwrdata7 = sbwrdata3 %>%
  left_join(sbwrdata6, by = "receiver_player_id")

# add O/U and odds columns for receptions before creating table
sbwrdata7$rec_ou = c(6, 2, 5, 3, 6, 2, 5, 6)
sbwrdata7$rec_odds = c(-157, "+101", -125, "+139", "+110", "+103", -118, -109)

# ungroup data & order by highest o/u
sbwrdata7 = sbwrdata7 %>%
  ungroup() %>%
  arrange(-rec_ou)

# create table
sbwrplot1 = gt(sbwrdata7) %>%
  cols_label(receiver_player_id = "",
             targets_per_reg = "Targets/Game",
             rec_per_reg = "Rec/Game",
             targets_per_late = "Targets/Game",
             rec_per_late = "Rec/Game",
             rec_ou = "Rec O/U",
             rec_odds = "Odds") %>%
  cols_align(align = "center") %>%
  fmt_number(columns = c("targets_per_reg", "rec_per_reg", "targets_per_late", "rec_per_late"),
             decimals = 0) %>%
  tab_header(title = "2025 SUPER BOWL",
             subtitle = "RECEPTIONS O/U") %>%
  tab_spanner(label = "'24 Reg. Season",
              columns = c("targets_per_reg", "rec_per_reg")) %>%
  tab_spanner(label = "Since Wk 15",
              columns = c("targets_per_late", "rec_per_late")) %>%
  tab_spanner(label = "@ DraftKings",
              columns = c("rec_ou", "rec_odds")) %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | Data @nflfastR")) %>%
  tab_options(footnotes.font.size = 11) %>%
  gt_nfl_headshots("receiver_player_id", height = 60) %>%
  gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = "rec_ou"))
  
# view table
sbwrplot1
  
# save table
sbwrplot1 %>%gtsave("SubSt2.5 - gt_sb_rec_ou.png")