# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load play-by-play data from 2024-2026 seasons (thru '26 wk 3)
nfldata3z = load_pbp(2024:2026)

# create new tibble containing 1st half runs from Chase Brown, CIN RB
# call rush attempts and rush yards grouped by game
nfldata3x = nfldata3z %>%
  filter(play_type == "run",
         aborted_play == 0,
         rusher_player_id == "00-0038597",
         game_half == "Half1") %>%
  group_by(posteam,
           game_id) %>%
  summarize(att = n(),
            first_half_yds = sum(yards_gained)
           ) %>%
  print(n = Inf)


# filter to include games with relevant rushing volume AKA games with 4 or more 1st half rush attempts
nfldata4x = nfldata3x %>%
  filter(att >= 4) %>%
  arrange(-first_half_yds) %>%
  print(n = Inf)

# export dataset as csv to local files
write.csv(nfldata4x,
          "C:/Users/Nick Gasperi/Documents/GitHub/trial-by-analytics-substack/SS-5-nfl-player-prop-odds-framework/data/cb_1h_rush_data.csv",
          row.names = FALSE)
