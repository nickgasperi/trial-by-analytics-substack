# load packages
library(tidyverse)    # data wrangling
library(readxl)       # to import data
library(ggrepel)      # replaces geom_text()
library(ggimage)      # replaces geom_text_repel() for 2025 data points

# load data
kenbart1

# wrangle data into new tibble with only national champions since 2008 and the 2025 final 4 teams
kbchamps = kenbart1 %>%
  filter(ROUND == 1 | YEAR == 2025 & SEED == 1) %>%
  print(n = Inf)

# insert blank column where we can add logos
kbchamps[, "logo"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kbchamps$logo[kbchamps$TEAM == "Auburn" & kbchamps$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kbchamps$logo[kbchamps$TEAM == "Florida" & kbchamps$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kbchamps$logo[kbchamps$TEAM == "Houston" & kbchamps$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kbchamps$logo[kbchamps$TEAM == "Duke" & kbchamps$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# insert column that combines year and team name
kbchamps$teamyear = paste(kbchamps$TEAM, kbchamps$YEAR, sep = ", ")

# plot power rating against adjusted efficiency
champsplot1 = kbchamps %>%
  mutate(pointcolor4 = ifelse(YEAR == 2025, "white", "purple")) %>%
  mutate(label4 = ifelse(YEAR == 2025, "", teamyear)) %>%
  ggplot(aes(x = BARTHAG, y = `KADJ EM`)) +
  geom_point(aes(color = pointcolor4),
             size = 4) +
  scale_color_identity() +
  geom_text_repel(box.padding = 0.85,
                  aes(label = label4,
                      fontface = "bold"),
                  size = 5.5) +
  geom_image(aes(image = kbchamps$logo),
             size = 0.065) +
  labs(title = "Barttorvik Power Rating vs. Kenpom Adj. Efficiency",
       subtitle = "'08-'24 National Champions",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold.italic", size = 24),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold.italic", size = 22),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 16))

# view plot
champsplot1

# save the plot to the device's local files
ggsave("SS3-plot16-barthag_eff_champs.png",
       width = 14, height = 10, dpi = "retina")
