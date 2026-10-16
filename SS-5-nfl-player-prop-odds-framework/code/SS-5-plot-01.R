# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# plot data using box plot
# use point annotate function to add green dot at each betting line yard total &  text annotate function to include odds label at each level 
boxplot_cb = ggplot(data = nfldata4x,
                  aes(x = 1,
                      y = first_half_yds)
                   ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(n.breaks = 7) +
  geom_boxplot(aes(fill = posteam,
                   alpha = 0.3)
              ) +
  scale_fill_nfl() +
  annotate("point",
           x = c(1, 1, 1), y = c(25, 35, 45),
           shape = 21,
           size = 5,
           fill = "green3",
           color = "black",
           stroke = 1.0) +
  annotate("text",
           x = c(1.10, 1.10, 1.10), y = c(25.0, 35.0, 43.5),
           label = c("-177", "+130", "+259"),
           color = "green4",
           size = 6) +
  labs(title = "Chase Brown 1st Half Rushing Yards",
       subtitle = "2024 Wk 1 - 2026 Wk 3 | min 4 att.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR | Odds via DraftKings",
       x = "",
       y = "Rush Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 13),
        plot.caption = element_text(size = 9),
        axis.title = element_text(face = "bold",
                                  size = 12),
        axis.text = element_text(size = 12)
        )

# view plot
boxplot_cb

# save plot to local files
ggsave("SS-5.1-cb_boxplot.png",
       width = 6, height = 7,
       dpi = "retina")
