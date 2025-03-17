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
wrdata7 = nfldata %>%
  filter(week < 19,
         !is.na(receiver_player_id)) %>%
  group_by(play_id, receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgt = n(),
            rec = complete_pass,
            .groups = "drop")

# create cumulative rec yds and targets columns
wrdata7$cumtgt = ave(wrdata7$tgt, wrdata7$receiver_player_id, FUN = cumsum)
wrdata7$cumrec = ave(wrdata7$rec, wrdata7$receiver_player_id, FUN = cumsum)

# frame last play for later future labeling
framewr7 = wrdata7 %>%
  filter(play_id == "5028") %>%
  print(n = Inf)

# plot data
# use gghighlight to emphasize J.Chase data point
# use 'use_direct_label' to allow you to create your own label with 'geom_point' & geom_text_repel'
wrplot7 = ggplot(data = wrdata7, aes(x = play_id, y = cumrec, group = receiver_player_id)) +
  geom_line(aes(color = posteam),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0036900",
              label_key = receiver_player_name,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  geom_point(data = framewr7,
             aes(color = posteam),
             size = 2.5) +
  geom_text_repel(data = framewr7,
            aes(label = cumrec,
                color = posteam,
                fontface = "bold.italic"),
            size = 6.5) +
  scale_y_continuous(breaks = c(0, 30, 60, 90, 120)) +
  labs(title = "Ja'Marr Chase Triple Crown - Receptions",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       y = "Receptions") +
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
wrplot7
  
# save plot
ggsave("SubSt4.n - chase_rec.png",
       width = 10.5, height = 7, dpi = "retina")
