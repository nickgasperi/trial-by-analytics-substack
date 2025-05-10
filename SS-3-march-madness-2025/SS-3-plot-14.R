# load packages
library(tidyverse)        # data wrangling
library(ggimage)          # add images to ggplot

# load data
kenbart1

# insert blank column for logos
kenbart1[, "logo"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart1$logo[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart1$logo[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart1$logo[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart1$logo[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# plot data
tempoplot2 = kenbart1 %>%
  mutate(color27 = ifelse(ROUND == 1, "purple", ifelse(SEED == 1 & YEAR == 2025, "white", "lightgrey"))) %>%
  mutate(size27 = ifelse(ROUND == 1, 2, 1)) %>%
  ggplot(aes(x = `K TEMPO`, y = BARTHAG)) +
  geom_point(aes(color = color27,
                 size = size27)) +
  geom_image(image = kenbart1$logo,
             size = 0.065) +
  scale_color_identity() +
  labs(title = "Tempo vs. Barttorvik Power Rating",
       subtitle = "National Champions | '08-'25 Tournament Teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "TEMPO") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  size = 24, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 22, face = "bold.italic"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16))

# view plot
tempoplot2

# save plot to device's local files
ggsave("SS3-plot14-tempo.png",
       width = 12, height = 9, dpi = "retina")
