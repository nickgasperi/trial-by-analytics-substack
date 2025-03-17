# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data
nfldata = load_teams(2024)

# create tibble for all time super bowl win leaders
sbteams = nfldata %>%
  filter(team_abbr == "NE" | team_abbr == "PIT" | team_abbr == "SF" | team_abbr == "DAL" |
         team_abbr == "KC" | team_abbr == "GB" | team_abbr == "NYG") %>%
  print(n = Inf)

# add sb wins column
sbteams$sbwins = c(5, 4, 4, 6, 4, 6, 5)

# create plot 1
sbwinplot = ggplot(data = sbteams, aes(x = reorder(team_abbr, -sbwins), y = sbwins)) +
  geom_col(aes(fill = team_abbr, color = team_abbr), linewidth = 1.25) +
  scale_fill_nfl(type = "primary") +
  scale_color_nfl(type = "secondary") +
  geom_text(label = sbteams$sbwins,
            position = position_stack(vjust = 0.85),
            color = sbteams$team_color2, fontface = "bold.italic", size = 12) +
  labs(title = "All-Time NFL Super Bowl Wins",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
  theme_void() +
  theme(plot.background = element_rect(fill = "cornsilk"),
        plot.title = element_text(hjust = 0.5, face = "bold.italic", size = 20),
        plot.caption = element_text(size = 11),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_nfl_logo(size = 2))

# view plot 1
sbwinplot

# save plot 1
ggsave("SubSt2.1 - all_time_sb.png",
       width = 10.5, height = 7, dpi = "retina")

#######

# create tibble for 21st century super bowl win leaders
sbteams21c = nfldata %>%
  filter(team_abbr == "NE" | team_abbr == "PIT" | team_abbr == "LAR" | team_abbr == "BAL" |
         team_abbr == "KC" | team_abbr == "TB" | team_abbr == "NYG") %>%
  print(n = Inf)

# add sb wins column
sbteams21c$sbwins = c(2, 3, 2, 6, 2, 2, 2)

# create plot 2
sbwin21cplot = ggplot(data = sbteams21c, aes(x = reorder(team_abbr, -sbwins), y = sbwins)) +
  geom_col(aes(fill = team_abbr, color = team_abbr), linewidth = 1.25) +
  scale_fill_nfl(type = "primary") +
  scale_color_nfl(type = "secondary") +
  geom_text(label = sbteams21c$sbwins,
            position = position_stack(vjust = 0.85),
            color = sbteams21c$team_color2, fontface = "bold.italic", size = 12) +
  labs(title = "NFL Super Bowl Wins - since 2000",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR") +
  theme_void() +
  theme(plot.background = element_rect(fill = "cornsilk"),
        plot.title = element_text(hjust = 0.5, face = "bold.italic", size = 20),
        plot.caption = element_text(size = 11),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_nfl_logo(size = 2))

# view plot 2
sbwin21cplot

# save plot 2
ggsave("SubSt2.2 - 21st_cen_sb.png",
       width = 10.5, height = 7, dpi = "retina")