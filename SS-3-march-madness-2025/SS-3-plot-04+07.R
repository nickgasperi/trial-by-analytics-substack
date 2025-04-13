library(tidyverse)        # data wrangling

# load data
kenbart1

# Plot 4 Code - 1-Seed Groups -----------------------------------------

# create tibble 1
kbprev = kenbart1 %>%
  select(SEED, YEAR, TEAM, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR < 2025, SEED == 1) %>%
  summarize(avgWAB = mean(WAB), avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`), avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# create tibble 2
kbnow = kenbart1 %>%
  select(SEED, YEAR, TEAM, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR == 2025,
         SEED == 1) %>%
  summarize(avgWAB = mean(WAB), avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`), avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# add labels to each tibble
kbprev$seasons = "'18-'24"
kbnow$seasons = "'25"

# combine tibbles
kbcomp = bind_rows(kbprev, kbnow)

# view new tibble
kbcomp  # this tibble is formatted well for simply viewing, but we need to reshape it to make it usable in ggplot()
        # since the tibble is very small, just manually rearrange our variables
# start a new tibble
kbcomp2 = tibble(
  stat = c("avgWAB", "avgBARTH", "avgEFF", "avgEFG",
           "avgWAB", "avgBARTH", "avgEFF", "avgEFG"),
  value = c(9.38, 0.958, 28.8, 54.2,
            11.2, 0.978, 35.8, 55.2) ,
  seasons = c("'08-'24", "'08-'24", "'08-'24", "'08-'24",
              "'25", "'25", "'25", "'25"))

# view reworked tibble
kbcomp2

# create group of 4 barplots with ggplot() and face_wrap()
# when formatting facet_wrap use 'strip' functions
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


# Plot 7 Code - Final Four Groups -----------------------------------------

# create tibble 1
kbffone = kenbart1 %>%
  select(SEED, YEAR, TEAM, ROUND, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR < 2025, ROUND < 5) %>%
  summarize(avgWAB = mean(WAB), avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`), avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

# create tibble 2
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

# combine tibbles
kbcomp99 = bind_rows(kbffone, kbffnow)

# view new tibble
kbcomp99

# manually transpose tibble
kbcomp11 = tibble(
  stat = c("avgWAB", "avgBARTH", "avgEFF", "avgEFG",
           "avgWAB", "avgBARTH", "avgEFF", "avgEFG"),
  value = c(6.79, 0.927, 24.8, 53.2,
            11.2, 0.978, 35.8, 55.2) ,
  seasons = c("'08-'24", "'08-'24", "'08-'24", "'08-'24",
              "'25", "'25", "'25", "'25"))

# create group of 4 barplots with ggplot() and face_wrap()
# when formatting facet_wrap use 'strip' functions
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