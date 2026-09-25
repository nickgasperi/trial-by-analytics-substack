# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# plot relative model accuracy vs. ppg by team
pred_ppg_plot = ggplot(data = team_plot_data_b,
                       aes(x = ppg,
                           y = diff_accuracy)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_hline(yintercept = mean(team_plot_data_b$diff_accuracy),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_vline(xintercept = mean(team_plot_data_b$ppg),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_nfl_logos(aes(team_abbr = team),
                 width = 0.06,
                 alpha = 0.80) +
  labs(title = "Relative Model Accuracy vs. Points Per Game by Team",
       subtitle = "2025 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Points Per Game",
       y = "Relative Accuracy") +
  theme_minimal() +
  theme(plot.background = element_rect("white"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     face = "bold"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 14,
                                  face = "bold"),
        axis.text = element_text(size = 12))

# view plot
pred_ppg_plot

# save plot to local files
ggsave("SS-4.4-team-accuracy-ppg-plot.png",
       width= 9, height = 6.5,
       dpi = "retina")
