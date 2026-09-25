# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# view dataset from file: SS-4-model-02.R
team_results_summary

# data already ordered by team_accuracy DESC - add ranking column to use as y-axis sorting in ggplot()
team_results_plot_data = team_results_summary %>%
  mutate(accuracy_rank = row_number())

# convert team_accuracy variable from proportion to percentage
team_results_plot_data$team_accuracy = team_results_plot_data$team_accuracy * 100

# plot data
team_accuracy_plot = ggplot(data = team_results_plot_data) +
  geom_segment(aes(x = 74.03004,
                   xend = team_accuracy,
                   y = reorder(team, -accuracy_rank),
                   yend = team,
                   color = team),
               linewidth = 2.20,
               alpha = 0.90) +
  scale_color_nfl(type = "primary") +
  geom_point(aes(x = 74.03004,
                 y = reorder(team, -accuracy_rank),
                 fill = team_accuracy > 74.03004),
             shape = 21,
             size = 2.5,
             color = "black",
             stroke = 0.5) +
  scale_fill_manual(values = c("TRUE" = "forestgreen",
                               "FALSE" = "firebrick"),
                    guide = "none") +
  geom_nfl_logos(mapping = aes(team_abbr = team,
                             x = team_accuracy,
                             y = team),
                 width = 0.033,
                 alpha = 0.900) +
  labs(title = "Team-Level Model Accuracy",
       subtitle = "Green Dot > League Accuracy > Red Dot",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Model Accuracy (%)") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5,
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 13,
                                     face = "bold.italic"),
        plot.caption = element_text(size = 12),
        axis.title.x = element_text(size = 12,
                                  face = "bold"),
        axis.text.x = (element_text(size = 12)),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

# view plot
team_accuracy_plot

# save plot to local files
ggsave("SS-4.2-team_accuracy_plot.png",
       width= 10.5, height = 7,
       dpi = "retina")
