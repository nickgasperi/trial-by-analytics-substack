# load packages
library(tidyverse)        # data wrangling
library(readxl)           # to import data
library(ggimage)

# add column to dataset
kenbart1$SQUAD = paste(kenbart1$TEAM, kenbart1$YEAR, sep = ", ")

# plot data
# for the mutate color and size lines - you can use the or function ( | ) or use a nested ifelse. the only difference is how your code is organized/looks
champs2 = kenbart1 %>%
  mutate(color6 = ifelse(ROUND == 1, "purple", ifelse(SEED == 1 & YEAR == 2025, "purple", "lightgrey"))) %>%
  mutate(size6 = ifelse(ROUND == 1, 7, ifelse(SEED == 1 & YEAR == 2025, 7, 1))) %>%
  mutate(label6 = ifelse(SEED == 1 & YEAR == 2025, SQUAD, "")) %>%
  ggplot(aes(x = SEED, y = `WIN%`)) +
  geom_point(aes(color = color6,
                 size = size6)) +
  geom_text_repel(box.padding = 2.6,
                  max.overlaps = 500,
                  aes(label = label6,
                      fontface = "bold.italic",
                      size = 7)) +
  scale_color_identity() +
  scale_x_continuous(n.breaks = 16) +
  labs(title = "Seed vs. Win %",
       subtitle = "National Champions | '08-'25 Tournament Teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold.italic",
                                  size = 24),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold.italic",
                                     size = 22),
        plot.caption = element_text(size = 12),
        axis.title = element_text(face = "bold",
                                  size = 18),
        axis.text = element_text(size = 16))

# view plot
champs2

# save the plot to the device's local files
ggsave("SS3-plot13-champs_bio.png",
       width = 14, height = 10, dpi = "retina")
