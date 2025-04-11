# load packages
library(tidyverse)        # data wrangling
library(readxl)           # to import data

# 
kenbart1

# plot
kenbart1$comprank = (kenbart1$`KADJ EM RANK` + kenbart$`BARTHAG RANK`)/2

# add column to use as labels in
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

# agg data
# includes all 1 seeds 
kenbart77 = kenbart1 %>%
  select(YEAR, SEED, TEAM, SQUAD, comprank, colorpri, colorsec, logo) %>%
  filter(SEED == 1) %>%
  print(n = Inf)

mean(kenbart77$comprank)

# plot
plot77 = kenbart77 %>%
  mutate(color77pri = ifelse(YEAR == 2025, colorpri, "grey90")) %>%
  mutate(color77sec = ifelse(YEAR == 2025, colorsec, NA)) %>%
  ggplot(aes(x = reorder(SQUAD, comprank), y = reorder(comprank, -comprank))) +
  geom_bar(stat = "identity",
           aes(fill = color77pri,
               color = color77sec),
           linewidth = 0.8) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_image(aes(image = kenbart77$logo),
             size = 0.055,
             position = position_stack(vjust = 1.05)) +
  geom_hline(yintercept = max(kenbart77$comprank)-3.904412,
             linetype = "dashed") +
  labs(title = "",
       subtitle = "",
       x = "TEAM", y = "COMPOSITE RANK") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "white"),
        axis.text.x = element_blank())

# view
plot77

# save the plot to the device's local files
ggsave("SubSt3.n-comp-rating.png",
       width = 14, height = 10, dpi = "retina")


# agg data
# includes final four teams from '08-'24 and the '25 final four teams
kenbart88 = kenbart1 %>%
  select(YEAR, ROUND, SEED, TEAM, SQUAD, comprank) %>%
  filter(ROUND < 5 & YEAR < 2025 | SEED == 1 & YEAR == 2025) %>%
  print(n = Inf)

# plot