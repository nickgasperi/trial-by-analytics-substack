library(tidyverse)        # data wrangling

# view tibble
kenbart1

kbprev = kenbart1 %>%
  select(SEED, YEAR, TEAM, WAB, BARTHAG, `BADJ EM`, `EFG%`) %>%
  filter(YEAR == 2018 | YEAR == 2019 | YEAR == 2020 |
         YEAR == 2021 | YEAR == 2022 | YEAR == 2023 |
         YEAR == 2024,
         SEED == 1) %>%
  summarize(avgWAB = mean(WAB), avgBARTH = mean(BARTHAG),
            avgEFF = mean(`BADJ EM`), avgEFG = mean(`EFG%`)) %>%
  print(n = Inf)

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
        # since the tibble is very small, it is best to just manually rearrange our variables
# start a new tibble
kbcomp2 = tibble(
  stat = c("avgWAB", "avgBARTH", "avgEFF", "avgEFG",
           "avgWAB", "avgBARTH", "avgEFF", "avgEFG"),
  value = c(9.23, 0.957, 29.0, 55.1,
            11.2, 0.978, 35.8, 55.2) ,
  seasons = c("'18-'24", "'18-'24", "'18-'24", "'18-'24",
              "'25", "'25", "'25", "'25")
  )

# view reworked tibble
kbcomp2

# create group of 4 barplots with ggplot() and face_wrap()
plotkbcomp = ggplot(data = kbcomp2, aes(x = seasons, y = value,
                                        fill = seasons)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(vars(stat),
             scales = "free") +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.9),
            fontface = "bold", color = "white", size = 5) +
  theme_bw() +
  theme(legend.title = element_text(face = "bold"))

# view plot
plotkbcomp

# save plot to device's local files
ggsave("1-seed-facet-comp.png",
       width = 14, height = 10, dpi = "retina")  
