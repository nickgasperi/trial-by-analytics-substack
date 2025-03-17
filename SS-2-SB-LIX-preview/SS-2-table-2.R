# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(gt)
library(data.table)
library(gtExtras)

# load data
nfldata = load_pbp(2024)
nfldata4 = load_pbp(2018:2024)

# create tibble 1
sbqbdata1 = nfldata %>%
  filter(week >= 15,
         play_type == "pass",
         passer_player_name == "P.Mahomes" | passer_player_name == "J.Hurts",
         qb_spike == 0,
         !is.na(passing_yards)) %>%
  group_by(passer_player_id, passer_player_name, posteam) %>%
  summarize(avgyds_reg = sum(passing_yards)/5) %>%
  print(n = Inf)

# create tibble 2
sbqbdata2 = nfldata4 %>%
  filter(season_type == "POST",
         play_type == "pass",
         passer_player_name == "P.Mahomes" | passer_player_name == "J.Hurts",
         qb_spike == 0,
         !is.na(passing_yards)) %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(avgyds = sum(passing_yards)) %>%
  print(n = Inf)

# add games played, then calc average per game
sbqbdata2$post_gp = c(20, 8)
sbqbdata2$avgyds_post = sbqbdata2$avgyds/sbqbdata2$post_gp

# view updated tibble
sbqbdata2

# join tables
sbqbdata3 = sbqbdata1 %>%
  left_join(sbqbdata2, by = c("passer_player_id", "passer_player_name"))

# view new tibble
sbqbdata3

# add vegas props
sbqbdata3$passyd_prop = c(250, 200)
sbqbdata3$passyd_odds = c(-123, -166)

# filter out unwanted columns
sbqbdata3 = sbqbdata3 %>%
  ungroup() %>%
  select(passer_player_id, avgyds_reg, avgyds_post, passyd_prop, passyd_odds)

# create one more tibble to get tot. reg. season passing yds
# then add games played , then calc avg yd per game
sbqbdata0 = nfldata %>%
  filter(week <= 18,
         play_type == "pass",
         passer_player_name == "P.Mahomes" | passer_player_name == "J.Hurts",
         qb_spike == 0,
         !is.na(passing_yards)) %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(totyds_reg = sum(passing_yards)) %>%
  print(n = Inf)

sbqbdata0$gp_reg = c(16, 15)
sbqbdata0$avgyds_totreg = sbqbdata0$totyds_reg/sbqbdata0$gp_reg

# remove passer name, then view updated tibble
sbqbdata0 = sbqbdata0 %>%
  select(passer_player_id, gp_reg, avgyds_totreg) %>%
  print(n = Inf)

# join last column
sbqbdata3 = sbqbdata3 %>%
  left_join(sbqbdata0, by = "passer_player_id")

# reorder columns
# view tibble - use this for gt()
sbqbdata3 = sbqbdata3 %>%
  relocate(gp_reg, .after = passer_player_id) %>%
  relocate(avgyds_totreg, .after = gp_reg) %>%
  print(n = Inf)

# create table
sbqbtbl1 = gt(sbqbdata3) %>%
  cols_label(passer_player_id = "",
             gp_reg = "Games",
             avgyds_totreg = "Yd/Game",
             avgyds_reg = "Yd/Game",
             avgyds_post = "Yd/Game",
             passyd_prop = "Yards O/U",
             passyd_odds = "Odds") %>%
  cols_align(align = "center") %>%
  fmt_number(columns = c("avgyds_totreg", "avgyds_reg", "avgyds_post"),
             decimals = 0) %>%
  tab_header(title = "2025 SUPER BOWL",
             subtitle = "PASSING YARDS O/U") %>%
  tab_spanner(label = "'24 Reg. Season",
              columns = c("gp_reg", "avgyds_totreg")) %>%
  tab_spanner(label = "Since Wk 15",
              columns = "avgyds_reg") %>%
  tab_spanner(label = "Postseason",
              columns = "avgyds_post") %>%
  tab_spanner(label = "@ DraftKings",
              columns = c("passyd_prop", "passyd_odds")) %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | Data @nflfastR")) %>%
  tab_options(footnotes.font.size = 9) %>%
  gt_nfl_headshots(columns = passer_player_id, height = 50) %>%
  gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = "passyd_prop"))

# view table
sbqbtbl1

# save table
sbqbtbl1 %>%gtsave("SubSt2.4 - gt_sb_passyd.png")