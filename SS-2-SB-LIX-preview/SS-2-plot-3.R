# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# pull 2024 NFL game results
schedules1 = fast_scraper_schedules(2024)

# filter to only see KC games
# select only necessary columns
teamstats2 = schedules1 %>%
  filter(week < 22,
         home_team == "KC" | away_team == "KC") %>%
  select(game_type, week, away_team, away_score, home_team, home_score, result) %>%
  print(n = Inf)

# add kc cum_result column
teamstats2$kc_result = ifelse(teamstats2$home_team == "KC", teamstats2$result, teamstats2$result*(-1))

# add point diff cumulative sum column
teamstats2$cum_result = ave(teamstats2$kc_result, FUN = cumsum)

# filter to necessary columns
teamstats2 = teamstats2 %>%
  select(week, cum_result) %>%
  print(n = Inf)

# add filler column for bye week
# add column for team
teamstatskc = teamstats2 %>%
  add_row(week = 6, cum_result = 33, .after = 5) %>%
  mutate(team = "KC") %>%
  print(n = Inf)

# repeat for PHI games
teamstats3 = schedules1 %>%
  filter(week < 22,
         home_team == "PHI" | away_team == "PHI") %>%
  select(game_type, week, away_team, away_score, home_team, home_score, result) %>%
  print(n = Inf)

# add phi cum_result column
teamstats3$phi_result = ifelse(teamstats3$home_team == "PHI", teamstats3$result, teamstats3$result*(-1))

# add point diff cumulative sum column
teamstats3$cum_result = ave(teamstats3$phi_result, FUN = cumsum)

# filter to necessary columns
teamstats3 = teamstats3 %>%
  select(week, cum_result) %>%
  print(n = Inf)

# add filler column for bye week
# add column for team
teamstatsphi = teamstats3 %>%
  add_row(week = 5, cum_result = -10, .after = 4) %>%
  mutate(team = "PHI") %>%
  print(n = Inf)

# combine tibbles for ggplot
teamstats5 = bind_rows(teamstatskc, teamstatsphi)

# view new tibble
teamstats5 %>%
  print(n = Inf)

# define last week for geom_point() and geom_text_repel
frameteams5 = teamstats5 %>%
  filter(week == 21)

# plot data
scorediff1 = ggplot(data = teamstats5, aes(x = week, y = cum_result)) +
  geom_line(aes(color = team),
            linewidth = 1.5) +
  geom_point(data = frameteams5,
             aes(color = team),
             size = 2.5) +
  geom_text_repel(box.padding = 0.6,
                  min.segment.length = 1.5,
                  data = frameteams5,
                  aes(label = cum_result,
                      color = team,
                      fontface = "bold.italic"),
                  size = 10) +
  scale_color_nfl(type = "primary") +
  scale_x_continuous(n.breaks = 8) +
  labs(title = "Cumulative Point Differential - KC vs. PHI",
       subtitle = "2024 Regular & Post Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Week", y = "Point Differential") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(face = "bold", size = 22),
        plot.subtitle = element_text(face = "bold", size = 20),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 15))

# view plot
scorediff1

# save plot
ggsave("SubSt2.7 - point_diff.png",
       width = 10.5, height = 7.5, dpi = "retina")
  