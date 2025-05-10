# load packages
library(tidyverse)    # data wrangling
library(ggrepel)      # replaces geom_text

# load data
kenbart1

# wrangle data into new tibble that includes all teams from 2016 to 2025
kenbart2 = kenbart1 %>%
  filter(YEAR >= 2016)

# insert blank column for logos
kenbart2[, "logo"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart2$logo[kenbart2$TEAM == "Auburn" & kenbart2$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart2$logo[kenbart2$TEAM == "Florida" & kenbart2$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart2$logo[kenbart2$TEAM == "Houston" & kenbart2$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart2$logo[kenbart2$TEAM == "Duke" & kenbart2$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# plot Power Rating vs. WAB
# set point color to grey for all teams except 2025 1-seeds
# match point color for 2025 teams to plot background color & add logos with geom_image
power2 = kenbart2 %>%
  mutate(pointcolor3 = ifelse(SEED == 1 & YEAR == 2025, "white", "darkgrey")) %>%
  ggplot(aes(x = BARTHAG, y = WAB)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "grey27") +
  geom_point(aes(color = pointcolor3),
             size = 2.5) +
  geom_image(aes(image = kenbart2$logo),
             size = 0.03) +
  scale_color_identity() +
  labs(title = "Barttorvik Power Rating vs. Wins Above Bubble",
       subtitle = "'16-'25 Tournament Teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5, size = 21, face = "bold.italic"),
        plot.caption = element_text(size = 11),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16))

# view plot
power2

# save the plot to the device's local files
ggsave("SS3.3-barthag_wab_multi.png",
       width = 14, height = 10, dpi = "retina")
