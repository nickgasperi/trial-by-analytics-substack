# load packages
library(tidyverse)        # data wrangling
library(readxl)           # to import data
library(ggimage)

# convert dataframe to tibble
kenbart1 = as_tibble(kenbart)

# select only 2025 tournament
kenbart2025 = kenbart1 %>%
  filter(YEAR == 2025)

# insert blank column where we can add logos
kenbart2025[, "logo"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart2025$logo[kenbart2025$TEAM == "Auburn"] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart2025$logo[kenbart2025$TEAM == "Florida"] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart2025$logo[kenbart2025$TEAM == "Houston"] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart2025$logo[kenbart2025$TEAM == "Duke"] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# plot power rating vs. wins above bubble
# set point color to grey for seeds 2-16 -- match point color to plot background color for teams with logos
power1 = kenbart2025 %>%
  mutate(pointcolor2 = ifelse(SEED > 1, "darkgrey", "white")) %>%
  ggplot(aes(x = BARTHAG, y = WAB)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "grey27") +
  geom_point(aes(color = pointcolor2),
             size = 3) +
  geom_image(aes(image = kenbart2025$logo),
             size = 0.06) +
  scale_color_identity() +
  labs(title = "Power Rating vs. Wins Above Bubble",
       subtitle = "2025 March Madness",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  size = 24, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 21, face = "bold.italic"),
        plot.caption = element_text(size = 11),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16))

# view plot
power1

# save the plot to the device's local files
ggsave("SubSt3.1-barthag_wab.png",
       width = 14, height = 10, dpi = "retina")
