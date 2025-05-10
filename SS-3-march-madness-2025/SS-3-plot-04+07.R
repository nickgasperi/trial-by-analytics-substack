# load packages
library(tidyverse)        # data wrangling


# load data
kenbart1

## Plot 4 Code - 1-Seed Groups -----------------------------------------

# create tibble 1
# include only the selected variables for 1-seeds from all tournaments before 2025
# use summarize() to get average of each variable
kbprev = kenbart1 %>%
  select(SEED, YEAR, TEAM, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR < 2025, SEED == 1) %>%
  summarize(avgWAB = mean(WAB),
            avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`),
            avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# create tibble 2
# include only the selected variables for 2025 1-seeds
# use summarize() to get average of each variable
kbnow = kenbart1 %>%
  select(SEED, YEAR, TEAM, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR == 2025,
         SEED == 1) %>%
  summarize(avgWAB = mean(WAB),
            avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`),
            avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# add labels to each tibble
kbprev$seasons = "'18-'24"
kbnow$seasons = "'25"

# combine tibbles 1 and 2 into one new tibble
kbcomp = bind_rows(kbprev, kbnow)

# view new tibble
kbcomp  # this tibble is formatted well for viewing, but needs to be reshaped to make it usable in ggplot()
        # this is not best practice, but since the tibble is very small, just manually rearrange variables

# create new tibble that manually reshapes 'kbcomp'
# 'stat' ; 'value' ; 'column' are the names of the three columns
kbcomp2 = tibble(
  stat = c("avgWAB", "avgBARTH", "avgEFF", "avgEFG",
           "avgWAB", "avgBARTH", "avgEFF", "avgEFG"),
  value = c(9.38, 0.958, 28.8, 54.2,
            11.2, 0.978, 35.8, 55.2) ,
  seasons = c("'08-'24", "'08-'24", "'08-'24", "'08-'24",
              "'25", "'25", "'25", "'25"))

# view new tibble
kbcomp2

# plot all four variables in one visual with ggplot() and facet_wrap()
# specify geom_bar() before facet_wrap() so it knows which visual to plot
# I sometimes switch to theme_bw() when using facet_wrap() for better visibility
plotkbcomp = ggplot(data = kbcomp2, aes(x = seasons, y = value,
                                        fill = seasons)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(vars(stat),
             scales = "free") +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.9),
            fontface = "bold", color = "white", size = 5) +
  theme_bw() +
  theme(legend.title = element_text(face = "bold"),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 12))

# view plot
plotkbcomp

# save plot to device's local files
ggsave("facet-comp-1-seed.png",
       width = 8, height = 5, dpi = "retina")


## Plot 7 Code - Final Four Groups -----------------------------------------

# create tibble 1
# include only the selected variables for Final Four teams from all tournaments before 2025
# use summarize() to get average of each variable
kbffone = kenbart1 %>%
  select(SEED, YEAR, TEAM, ROUND, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR < 2025, ROUND < 5) %>%
  summarize(avgWAB = mean(WAB),
            avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`),
            avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# if you already created Plot 4, you could reuse 'kbnow', but the code is included again for the sake of the exercise
# create tibble 2
# include only the selected variables for 2025 Final Four teams
# use summarize() to get average of each variable
kbffnow = kenbart1 %>%
  select(SEED, YEAR, TEAM, ROUND, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR == 2025,
         SEED == 1) %>%
  summarize(avgWAB = mean(WAB), avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`), avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# add labels to each tibble
kbffone$seasons = "'18-'24"
kbffnow$seasons = "'25"

# combine tibbles 1 and 2 into one new tibble
kbcomp99 = bind_rows(kbffone, kbffnow)

# view new tibble
kbcomp99

# create new tibble that manually reshapes 'kbcomp99'
# 'stat' ; 'value' ; 'column' are the names of the three columns
kbcomp11 = tibble(
  stat = c("avgWAB", "avgBARTH", "avgEFF", "avgEFG",
           "avgWAB", "avgBARTH", "avgEFF", "avgEFG"),
  value = c(6.79, 0.927, 24.8, 53.2,
            11.2, 0.978, 35.8, 55.2) ,
  seasons = c("'08-'24", "'08-'24", "'08-'24", "'08-'24",
              "'25", "'25", "'25", "'25"))

# plot all four variables in one visual with ggplot() and facet_wrap()
# specify geom_bar() before facet_wrap() so it knows which visual to plot
# I sometimes switch to theme_bw() when using facet_wrap() for better visibility
plotffcomp = ggplot(data = kbcomp11, aes(x = seasons, y = value,
                                        fill = seasons)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(vars(stat),
             scales = "free") +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.9),
            fontface = "bold", color = "white", size = 5) +
  theme_bw() +
  theme(legend.title = element_text(face = "bold"),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 12))

# view plot
plotffcomp

# save plot to device's local files
ggsave("facet-comp-ff-groups.png",
       width = 8, height = 5, dpi = "retina")