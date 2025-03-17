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
wrdata8 = nfldata %>%
  filter(week < 19,
         play_type == "pass",
         !is.na(receiver_player_id)) %>%
  group_by(week, receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgt = n(),
            td = sum(touchdown == 1 &
                       td_team == posteam &
                       td_player_id == receiver_player_id),
            .groups = "drop")

# create cumulative rec tds and targets columns
wrdata8$cumtgt = ave(wrdata8$tgt, wrdata8$receiver_player_id, FUN = cumsum)
wrdata8$cumtd = ave(wrdata8$td, wrdata8$receiver_player_id, FUN = cumsum)

# frame last week for later future labeling
framewr8 = wrdata8 %>%
  filter(week == 18,
         receiver_player_id == "00-0036900") %>%
  print(n = Inf)

# plot data
# use gghighlight to emphasize J.Chase data point
# use 'use_direct_label' to allow you to create your own label with 'geom_point' & geom_text_repel'
wrplot8 = ggplot(data = wrdata8, aes(x = week, y = cumtd, group = receiver_player_id)) +
  geom_line(aes(color = posteam),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0036900",
              label_key = receiver_player_name,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  geom_point(data = framewr8,
             aes(color = posteam),
             size = 2.5) +
  geom_text_repel(box.padding = 0.2,
                  data = framewr8,
                  aes(label = cumtd,
                      color = posteam,
                      fontface = "bold.italic"),
                  size = 6.5) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  labs(title = "Ja'Marr Chase Triple Crown - Receiving TDs",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Week", y = "Receiving Touchdowns") +
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
wrplot8

# save plot
ggsave("SubSt4.n - chase_rec_tds.png",
       width = 10.5, height = 7, dpi = "retina")
