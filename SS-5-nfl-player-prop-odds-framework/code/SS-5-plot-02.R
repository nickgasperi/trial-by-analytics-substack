# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# generate column chart
colchart_cb = nfldata4x %>%
  mutate(ydcolor = ifelse(first_half_yds >= 45,
                          "purple4",
                          ifelse(first_half_yds >= 35,
                                 "purple1",
                                 ifelse(first_half_yds >= 25,
                                        "#CF9FFF",
                                        "grey")))) %>%
  ggplot(aes(x = game_id,
             y = first_half_yds)) +
  geom_hline(yintercept = c(25, 35, 45),
             linetype = "dashed",
             linewidth = 0.8) +
  geom_col(aes(fill = ydcolor),
           alpha = 0.9) +
  scale_fill_identity(name = NULL,
                      breaks = c("grey", "#CF9FFF", "purple1", "purple4"),
                      labels = c("< 25", "25+", "35+", "45+"),
                      guide = "legend") +
  scale_y_continuous(breaks = c(0, 20, 40, 60)) +
  labs(title = "Chase Brown 1st Half Rushing Yards",
       subtitle = "2024 Wk 1 - 2026 Wk 3 | min 4 att.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "game_id (chronological)",
       y = "Rush Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 16),
        plot.subtitle = element_text(face = "bold",
                                     size = 13),
        plot.caption = element_text(size = 9),
        axis.title = element_text(face = "bold",
                                  size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = c(0.64, 1.01),
        legend.justification = c(0.00, 0.00),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "grey35",
                                   size = 10))

# view plot
colchart_cb

# save to local files
ggsave("SS-5.2-cb_colchart.png",
       width = 8, height = 6.5,
       dpi = "retina")
