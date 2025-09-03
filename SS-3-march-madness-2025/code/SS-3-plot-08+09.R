# load packages
library(tidyverse)
library(ggimage)

# load data
kenbart1

# create new variable Composite Rank by averaging two existing variables
kenbart1$comprank = (kenbart1$`KADJ EM RANK` + kenbart1$`BARTHAG RANK`)/2

# create new column that concatenates team name and year
# there is already a unique ID in the dataset, but it is just a number assinged to each team - 'SQUAD' is created to be included in plots
kenbart1$SQUAD = paste(kenbart1$TEAM, kenbart1$YEAR, sep = ", ")

# add three blank columns for logos, primary colors, & secondary colors
kenbart1[, "logo"] = NA
kenbart1[, "colorpri"] = NA
kenbart1[, "colorsec"] = NA

# insert team logos for the four 1 seeds by conditionally replacing values
kenbart1$logo[kenbart1$TEAM == "Auburn"] = "C:/Users/Nick Gasperi/Downloads/auburn-logo.png"
kenbart1$logo[kenbart1$TEAM == "Florida"] = "C:/Users/Nick Gasperi/Downloads/florida-logo.png"
kenbart1$logo[kenbart1$TEAM == "Houston"] = "C:/Users/Nick Gasperi/Downloads/houston-logo.png"
kenbart1$logo[kenbart1$TEAM == "Duke"] = "C:/Users/Nick Gasperi/Downloads/duke-logo.png"

# insert hex codes for school primary and secondary colors
kenbart1$colorpri[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "#0C2340"
kenbart1$colorsec[kenbart1$TEAM == "Auburn" & kenbart1$YEAR == 2025] = "#E87722"
kenbart1$colorpri[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "#0021A5"
kenbart1$colorsec[kenbart1$TEAM == "Florida" & kenbart1$YEAR == 2025] = "#FA4616"
kenbart1$colorpri[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "#C8102E"
kenbart1$colorsec[kenbart1$TEAM == "Houston" & kenbart1$YEAR == 2025] = "#B2B4B2"
kenbart1$colorpri[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "#003087"
kenbart1$colorsec[kenbart1$TEAM == "Duke" & kenbart1$YEAR == 2025] = "#000000"


# Plot 8 - Final Four Teams ------------------------------------------------------------------


# wrangle data into new tibble including only selected variables for all Final Four teams in the dataset
kenbart88 = kenbart1 %>%
  select(YEAR,
         ROUND,
         SEED,
         TEAM,
         SQUAD,
         comprank,
         logo,
         colorpri,
         colorsec) %>%
  filter(ROUND < 5
         & YEAR < 2025 | SEED == 1
         & YEAR == 2025) %>%
  print(n = Inf)

# plot Composite Rank for all FF teams
# use mutate() to add primary and secondary colors to bars for only 2025 teams
# use reorder() to reverse the order of the y-axis
# add dashed horizontal line for average rank
plot88 = kenbart88 %>%
  mutate(color88pri = ifelse(YEAR == 2025, colorpri, "grey90")) %>%
  mutate(color88sec = ifelse(YEAR == 2025, colorsec, NA)) %>%
  ggplot(aes(x = reorder(SQUAD, comprank), y = reorder(comprank, -comprank))) +
  geom_bar(stat = "identity",
           aes(fill = color88pri,
               color = color88sec),
           linewidth = 0.8) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_image(aes(image = kenbart88$logo),
             size = 0.052,
             position = position_stack(vjust = 1.05)) +
  geom_hline(yintercept = 23,
             linetype = "dashed") +
  scale_y_discrete(breaks = c(1,2,3,4,5,84)) +
  labs(title = "Barttorvik + Kenpom Composite Power Rank",
       subtitle = "Final Four Teams | '08-'25 Tournaments",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "TEAM",
       y = "COMPOSITE RANK") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "white"),
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
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank())

# view plot
plot88

# save the plot to the device's local files
ggsave("SubSt3-plot8-comp-rating.png",
       width = 14,
       height = 10,
       dpi = "retina")


# Plot 9 - Final Four Teams by Year ------------------------------------------------------------------

# wrangle data into new tibble including only selected variables for all FF teams in the dataset
# group by year to change x-axis of ggplot() from individual teams in the prev. plot to average of each tournament
kenbart90 = kenbart1 %>%
  select(YEAR,
         SEED,
         ROUND,
         comprank,
         colorpri,
         colorsec,
         logo) %>%
  filter(ROUND < 5
         & YEAR < 2025 | SEED == 1
         & YEAR == 2025) %>%
  group_by(YEAR) %>%
  summarize(cumrank = mean(comprank)) %>%
  print(n = Inf)

# change YEAR column to character type for easier x-axis sorting
kenbart90$YEAR = as.character(kenbart90$YEAR)

# plot Composite Rank for each FF team, grouped by year on the x-axis
# highlight only the 2025 FF group with mutate()
# use reorder() to reverse the order of the y axis
plot90 = kenbart90 %>%
  mutate(color90 = ifelse(YEAR == 2025, "purple", "grey")) %>%
  ggplot(aes(x = YEAR, y = reorder(cumrank, -cumrank))) +
  geom_col(aes(fill = color90)) +
  scale_fill_identity() +
  labs(title = "Barttorvik + Kenpom Composite Power Rank",
       subtitle = "Final Four Groups | '08-'25 Tournaments",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin",
       x = "YEAR",
       y = "COMPOSITE RANK") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "white"),
        panel.grid.major.y = element_line(color = "grey"),
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
plot90

# save plot to local files
ggsave("SS3-plot9-comp-rating-group.png",
       width = 14,
       height = 10,
       dpi = "retina")
