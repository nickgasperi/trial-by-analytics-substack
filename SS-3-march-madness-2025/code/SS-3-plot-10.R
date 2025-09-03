# load packages
library(tidyverse)
library(ggimage)

# load data
kenbart1

# insert blank column for logos
kenbart1[, "logo"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart1$logo[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart1$logo[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart1$logo[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart1$logo[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# plot experience vs. height
# set color to purple if prev. champion, grey if not, and match color to plot background for '25 1-seeds since we will insert logos to replace points
heightplot = kenbart1 %>%
  mutate(color5 = ifelse(ROUND == 1, "purple", ifelse(YEAR == 2025 & SEED == 1, "white", "#F0F0F0"))) %>%
  mutate(size5 = ifelse(ROUND == 1, 2, 1)) %>%
  ggplot(aes(x = EXP, y = `AVG HGT`)) +
  geom_point(aes(color = color5,
                 size = size5)) +
  scale_color_identity() +
  geom_image(aes(image = kenbart1$logo),
             size = 0.065) +
  labs(title = "Avg. Experience vs. Avg. Height",
       subtitle = "National Champions | '08-'25 Tournament Teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "EXPERIENCE",
       y = "HEIGHT") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  size = 24,
                                  face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 22,
                                     face = "bold.italic"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 16,
                                  face = "bold"),
        axis.text = element_text(size = 16))

# view plot
heightplot

# save plot to device's local files
ggsave("SS3-plot10-exp_height.png",
       width = 14,
       height = 10,
       dpi = "retina")
