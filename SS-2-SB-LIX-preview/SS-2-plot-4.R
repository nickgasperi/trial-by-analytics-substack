# load packages
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggplot2)

# load data
nfldata = load_pbp(2024)

# filter data
sbeparush = nfldata %>%
  filter(week < 22,
         qb_kneel == 0,
         sack == 0,
         posteam == "PHI" | posteam == "KC",
         !is.na(epa),
         !is.na(rusher_player_id)) %>%
  group_by(rusher_player_id, rusher_player_name, posteam) %>%
  summarize(att = n(),
            epaper = sum(epa)/sum(att),
            .groups = "drop") %>%
  filter(att >= 50) %>%
  print(n = Inf)

# plot data
eparushplot2 = ggplot(data = sbeparush, aes(x = epaper,
                                            xend = 0,
                                            y = reorder(rusher_player_id, epaper),
                                            yend = rusher_player_id)) +
  geom_vline(xintercept = 0,
             linetype = "dashed", color = "darkgrey") +
  geom_segment(linewidth = 4.0, aes(color = posteam)) +
  scale_color_nfl(type = "primary") +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.07) +
  labs(title = "EPA Per Rush Attempt - KC & PHI",
       subtitle = "2024 NFL Regular & Post Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Rush", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill= "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.text.y = element_nfl_headshot(size = 2.85),
        axis.title.x = element_text(face = "bold.italic", size = 15),
        axis.text.x = element_text(size = 15))

# view plot
eparushplot2

# save plot
ggsave("SubSt2.8 - epa_per_rush.png",
       width = 10.5, height = 7.5, dpi = "retina")
