# load packages
library(tidyverse)        # data wrangling
library(readxl)           # to import data
library(ggimage)          # replaces geom_point() for 2025 teams

# load data
kenbart1

# calculate Composite Rank
kenbart1$comprank = (kenbart1$`KADJ EM RANK` + kenbart1$`BARTHAG RANK`)/2

# add column to combine team and year
kenbart1$SQUAD = paste(kenbart1$TEAM, kenbart1$YEAR, sep = ", ")

# add 3 blank columns
kenbart1[, "logo"] = NA
kenbart1[, "colorpri"] = NA
kenbart1[, "colorsec"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart1$logo[kenbart1$TEAM == "Auburn"] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart1$logo[kenbart1$TEAM == "Florida"] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart1$logo[kenbart1$TEAM == "Houston"] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart1$logo[kenbart1$TEAM == "Duke"] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

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

# wrangle data into new tibble that includes only 2025 FF and prev. national champs
kenbartchamps = kenbart1 %>%
  filter(ROUND == 1 | SEED == 1 & YEAR == 2025) %>%
  print(n = Inf)

# plot data
# after reordering, the y axis labels will get stiff - scale_y_discrete is used to make sure the important y-axis labels are incldued
plot24 = kenbartchamps %>%
  mutate(color24pri = ifelse(YEAR == 2025, colorpri, "grey90")) %>%
  mutate(color24sec = ifelse(YEAR == 2025, colorsec, NA)) %>%
  mutate(label24 = ifelse(YEAR < 2025, YEAR, "")) %>%
  ggplot(aes(x = reorder(SQUAD, comprank), y = reorder(comprank, -comprank))) +
  geom_bar(stat = "identity",
           aes(fill = color24pri,
               color = color24sec),
           linewidth = 0.8) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(label = label24),
            fontface = "bold.italic",
            size = 7) +
  geom_image(aes(image = kenbartchamps$logo),
             size = 0.075,
             position = position_stack(vjust = 1.05)) +
  geom_hline(yintercept = 23,
             linetype = "dashed") +
  scale_y_discrete(breaks = c(1,2,3,4,5,24)) +
  labs(title = "Barttorvik + Kenpom Composite Power Rank",
       subtitle = "National Champions | '08-'25 Tournaments",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "TEAM", y = "COMPOSITE RANK") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  size = 24, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 22, face = "bold.italic"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank())

# view plot
plot24
    
# save the plot to the device's local files
ggsave("SS3-plot15-comp-rating-champs.png",
       width = 14, height = 10, dpi = "retina")
