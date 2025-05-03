# load packages
library(tidyverse)        # data wrangling
library(ggimage)          # add images to ggplot

# load data
kenbart1

# insert blank columns to can add logos, primary team color, and secondary team color
kbchamps[, "logo"] = NA
kenbart1[, "colorpri"] = NA
kenbart1[, "colorsec"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart1$logo[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart1$logo[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart1$logo[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart1$logo[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# insert column that combines year and team name
kenbart1$SQUAD = paste(kenbart1$TEAM, kenbart1$YEAR, sep = ", ")

# insert school primary and secondary colors
# link to hex codes for all sports: 
kenbart1$colorpri[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "#0C2340"
kenbart1$colorsec[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "#E87722"
kenbart1$colorpri[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "#0021A5"
kenbart1$colorsec[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "#FA4616"
kenbart1$colorpri[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "#C8102E"
kenbart1$colorsec[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "#B2B4B2"
kenbart1$colorpri[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "#003087"
kenbart1$colorsec[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "#000000"

# create density plot
gamesplot = ggplot(data = kenbart1, aes(GAMES)) +
  geom_density(aes(fill = "density"),
               kernel = "cosine",
               alpha = 0.3,
               fill = "#009CDE") +
  geom_vline(xintercept = 32.9,
             linetype = "dashed",
             color = "purple",
             linewidth = 0.85) +
  geom_vline(xintercept = 33,
             linewidth = 0.75) +
  geom_vline(xintercept = 34,
             linewidth = 0.75) +
  geom_image(image = kenbart1$logo,
             size = 0.055,
             x = kenbart1$GAMES, y = ifelse(kenbart1$TEAM == "Auburn" | kenbart1$TEAM == "Duke", 0.01,
                                            ifelse(kenbart1$TEAM == "Houston", 0.028,
                                                   ifelse(kenbart1$TEAM == "Florida", 0.043, 0)))) +
  labs(title = "Density of Pre-Tournament Games Played",
       subtitle = "'08-'25 Tournament Teams",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "GAMES", y = "DENSITY") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5, size = 20, face = "bold.italic"),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16))

# view plot
gamesplot

# save the plot to the device's local files
ggsave("SS3-plot11-games-champs.png",
       width = 12.5, height = 9, dpi = "retina")
