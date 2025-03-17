# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data
nfldata2 = load_pbp(2018:2024)

# summarize penalties and penalty yards by team in reg season games
pendata1 = nfldata2 %>%
  filter(season_type == "REG",
         !is.na(penalty_yards),
         !is.na(penalty_team)) %>%
  group_by(penalty_team) %>%
  summarize(penalties = n(),
            penyd = sum(penalty_yards)) %>%
  arrange(-penyd) %>%
  print(n = Inf)

# plot data
penplot1 = pendata1 %>%
  mutate(pencolor1 = ifelse(penalty_team == "KC", NA, "b/w")) %>%
  mutate(penwidth = ifelse(penalty_team == "KC", 0.065, 0.05)) %>%
  ggplot(aes(x = penalties, y = penyd)) +
  geom_hline(yintercept = mean(pendata1$penyd), linetype = "dashed") +
  geom_vline(xintercept = mean(pendata1$penalties), linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_nfl_logos(aes(team_abbr = penalty_team, color = pencolor1, width = penwidth),
                 alpha = 0.8) +
  scale_color_identity() +
  labs(title = "Cumulative Penalties & Penalty Yards By Team",
       subtitle = "2018-2024 Regular Seasons",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "# of Penalties", y = "Total Penalty Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(face = "bold", size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 15))

# view plot
penplot1

# save plot
ggsave("SubSt1.1 - reg_szn_pens.png",
       width = 10.5, height = 7, dpi = "retina")