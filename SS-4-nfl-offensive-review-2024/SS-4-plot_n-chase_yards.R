# load packages
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggplot2)
library(gghighlight)
library(ggrepel)

# load data
nfldata = load_pbp(2024)

# filter data
wrdata6 = nfldata %>%
  filter(week < 19,
         !is.na(yards_gained),
         !is.na(receiver_player_id)) %>%
  group_by(play_id, receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgts = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# add cumulative rec yds and targets columns to existing tibble
wrdata6$cumyds = ave(wrdata6$yds, wrdata6$receiver_player_id, FUN = cumsum)
wrdata6$cumtgts = ave(wrdata6$tgts, wrdata6$receiver_player_id, FUN = cumsum)

# frame last targets of dataset for later geom_point()
framewr6 = wrdata6 %>%
  filter(play_id == "5028") %>%
  print(n = Inf)

# plot data
wrplot6 = ggplot(data = wrdata6, aes(x = play_id, y = cumyds, group = receiver_player_id)) +
  geom_line(aes(color = posteam),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0036900",
              label_key = receiver_player_name,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  geom_point(data = framewr6,
             aes(color = posteam),
             size = 2.5) +
  geom_text_repel(data = framewr6,
                  aes(label = cumyds,
                      color = posteam,
                      fontface = "bold.italic"),
                  size = 6.5) +
  scale_y_continuous(breaks = c(0, 400, 800, 1200, 1600)) +
  labs(title = "Ja'Marr Chase Triple Crown - Receiving Yards",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       y = "Receiving Yards") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(face = "bold", size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15))

# view plot
wrplot6

# save plot to device's local files
ggsave("SubSt4.n - chase_yards.png",
       width = 10.5, height = 7, dpi = "retina")
