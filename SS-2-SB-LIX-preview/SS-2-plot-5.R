# load packages
library(tidyverse)      # data wrangling
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data to call receiving plays from KC and Philly
# incldues regular and postseason
# no need to filter out qb spikes since there is no receiver targeted on those plays
sbeparec = nfldata %>%
  filter(week < 22,
         posteam == "KC" | posteam == "PHI",
         !is.na(receiver_player_id),
         !is.na(epa)) %>%
  group_by(receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgts = n(),
            epaper = sum(epa)/sum(tgts)) %>%
  arrange(-tgts) %>%
  filter(tgts >= 50) %>%      # include players with at least 50 targets this season
  print(n = Inf)

# plot data
eparecplot2 = ggplot(data = sbeparec, aes(x = epaper,
                                            xend = 0,
                                            y = reorder(receiver_player_id, epaper),
                                            yend = receiver_player_id)) +
  geom_vline(xintercept = 0,
             linetype = "dashed", color = "darkgrey") +
  geom_segment(linewidth = 4.0, aes(color = posteam)) +
  scale_color_nfl(type = "primary") +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.07) +
  labs(title = "EPA Per Target - KC & PHI",
       subtitle = "2024 NFL Regular & Post Season (min. 50 targets)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Target", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill= "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.caption = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.text.y = element_nfl_headshot(size = 2.5),
        axis.title.x = element_text(face = "bold.italic", size = 15),
        axis.text.x = element_text(size = 15))

# view plot
eparecplot2

# save plot to device's local files
ggsave("SubSt2.9 - epa_per_target.png",
       width = 10.5, height = 7.5, dpi = "retina")
