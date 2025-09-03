# load packages
library(tidyverse)
library(readxl)

# convert dataframe to tibble
kenbart1 = as_tibble(kenbart)

# load data
kenbart1

# define top 5 conferences
topconf = c("B12", "SEC", "ACC", "B10", "BE")

# plot WAB by team
wabexample = kenbart1 %>%
  mutate(color5 = ifelse(CONF %in% topconf, "purple", "darkgrey")) %>%    # highlight only teams from prev. defined leagues
  ggplot(aes(x = TEAM, y = WAB)) +
  geom_hline(yintercept = 0,
             color = "red3",
             linewidth = 1.5) +
  geom_point(aes(color = color5)) +
  scale_color_identity() +
  labs(title = "Wins Against Bubble - NCAA Tournament Teams",
       subtitle = "2008-2025 | purple = top 5 conf.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nishaanamin") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold.italic",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold.italic",
                                     size = 18),
        plot.caption = element_text(size = 9),
        axis.title.y = element_text(size = 16,
                                  face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

# view plot
wabexample

# save plot to local files
ggsave("SS-3-plot1-wab_example.png",
       width = 12,
       height = 9, 
       dpi = "retina")
