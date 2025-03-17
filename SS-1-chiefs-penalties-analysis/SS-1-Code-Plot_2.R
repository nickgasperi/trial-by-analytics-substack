# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data
nfldata = load_pbp(2024)

# summarize regular season penalty yards by team
# make sure to arrange by most pen yards to least for next step
regpen = nfldata %>%
  mutate(penalty = ifelse(is.na(penalty), 0, penalty)) %>%
  mutate(penalty_yards = ifelse(is.na(penalty_yards), 0, penalty_yards)) %>%
  filter(season_type == "REG",
         !is.na(penalty_team)) %>%
  group_by(penalty_team) %>%
  summarize(penyd = sum(penalty_yards)) %>%
  arrange(-penyd) %>%
  print(n = Inf)

# add rank to tibble
regpen1 = regpen %>%
  mutate(rankpenyd = row_number())

# view updated table
regpen1 %>% print(n = Inf)

# summarize regular season 4th Quarter penalty yards by team
regpen4q = nfldata %>%
  mutate(penalty = ifelse(is.na(penalty), 0, penalty)) %>%
  mutate(penalty_yards = ifelse(is.na(penalty_yards), 0, penalty_yards)) %>%
  filter(season_type == "REG",
         qtr == 4,
         !is.na(penalty_team)) %>%
  group_by(penalty_team) %>%
  summarize(penyd4 = sum(penalty_yards)) %>%
  arrange(-penyd4) %>%
  print(n = Inf)

# add rank to tibble2
regpen4q2 = regpen4q %>%
  mutate(rank4penyd = row_number())

# view updated table
regpen4q2 %>% print(n = Inf)

# combine tibbles to create one dataset
allpenyd = regpen1 %>%
  left_join(regpen4q2, by = "penalty_team") %>%
  print(n = Inf)

# plot regular season data
plotregpen5 = ggplot(data = allpenyd) +
  geom_nfl_logos(mapping = aes(team_abbr = penalty_team, x = rankpenyd, y = reorder(penalty_team, -rankpenyd)),
                               width = 0.04) +
  geom_segment(aes(x =rankpenyd, xend = rank4penyd,
                   y = penalty_team, yend = penalty_team,
                   color = penalty_team),
               linewidth = 1, alpha = 0.85) +
  geom_point(mapping = aes(x = rank4penyd, y = penalty_team, color = penalty_team), size = 2) +
  scale_color_nfl(type = "primary") +
  scale_x_reverse(breaks = seq(1, 32, 1)) +
  labs(title = "2024 NFL Reg. Season Total Penalty Yards",
       subtitle = "Logo = Season Total | Point = 4th Quarter Only",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Rank", y = "Penalty Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(face = "bold", size = 15),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic", size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank())

# view plot 1
plotregpen5

# save plot
ggsave("X post 46 - reg_penyd.png",
       width = 10.5, height = 7, dpi = "retina")