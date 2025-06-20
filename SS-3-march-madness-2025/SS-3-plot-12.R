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

# plot data
sosplot = kenbart1 %>%
  mutate(color7 = ifelse(ROUND == 1, "purple", ifelse(YEAR == 2025 & SEED == 1, "white", "lightgrey"))) %>%
  mutate(size7 = ifelse(ROUND == 1, 7, 1)) %>%
  ggplot(aes(x = YEAR, y = `ELITE SOS RANK`)) +
  geom_hline(yintercept = 28.25,
             linetype = "dashed",
             color = "red",
             linewidth = 1.25) +
  geom_point(aes(color = color7,
                 size = size7)) +
  scale_color_identity() +
  geom_image(aes(image = kenbart1$logo),
             size = 0.04) +
  scale_x_continuous(n.breaks = 17) +
  labs(title = "Strength of Schedule Rank",
       subtitle = "National Champions | '08-'25 Tournament Teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       y = "SOS RANK") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  size = 22,
                                  face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 20,
                                     face = "bold.italic"),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 16,
                                  face = "bold"),
        axis.text = element_text(size = 16))

# view plot
sosplot  

# save plot to local files
ggsave("SS3-plot12-sos_plot.png",
       width = 12,
       height = 9,
       dpi = "retina")
