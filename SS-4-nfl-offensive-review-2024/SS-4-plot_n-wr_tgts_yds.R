# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# load data
nfldata = load_pbp(2024)

# filter data
wrdata5 = nfldata %>%
  filter(week < 19,
         !is.na(receiver_player_id)) %>%
  group_by(receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgts = n(),
            recyd = sum(yards_gained)) %>%
  filter(tgts > 110) %>%
  arrange(-tgts) %>%
  print(n = Inf)

# plot targets and receiving yds by receiver
wrplot5 = ggplot(data = wrdata5, aes(x = tgts, y = recyd)) +
  geom_hline(yintercept = mean(wrdata5$recyd),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(wrdata5$tgts),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.04, alpha = 0.8) +
  geom_text_repel(box.padding = 0.3,
                  aes(label = receiver_player_name,
                      color = posteam)) +
  scale_color_nfl(type = "primary") +
  scale_x_continuous(breaks = c(110, 130, 150, 170)) +
  scale_y_continuous(breaks = c(900, 1100, 1300, 1500, 1700)) +
  labs(title = "Targets vs. Receiving Yards",
       subtitle = "2024 NFL Regular Season (min 110 targets)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Targets", y = "Receiving Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 15))

# view plot
wrplot5

# save plot to device's local files
ggsave("SubSt4.n - reg_passers.png",
       width = 10.5, height = 7, dpi = "retina")
