# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)

# plot confusion matrix as heat map using geom_tile()
conf_heat_map = ggplot(data = conf_matrix_plot_d,
                       aes(x = predicted,
                           y = actual,
                           fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "black",
                       mid = "lightgreen",
                       high = "forestgreen") +
  geom_text(aes(label = Freq),
            color = "white",
            size = 6) +
  coord_fixed() +
  labs(title = "Confusion Matrix",
       subtitle = "values expressed as %",
       x = "Predicted",
       y = "Actual") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 14,
                                     face = "italic"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# view plot
conf_heat_map

# save plot to local files
ggsave("SS-4.1-confusion-matrix-plot.png",
       width= 7, height = 7,
       dpi = "retina")
