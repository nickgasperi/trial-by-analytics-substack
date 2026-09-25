# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# plot relative model accuracy vs. offensive epa/play by team
pred_epa_plot = ggplot(data = team_plot_data_b,
                       aes(x = epa_play,
                           y = diff_accuracy)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_hline(yintercept = mean(team_plot_data_b$diff_accuracy),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_vline(xintercept = mean(team_plot_data_b$epa_play),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_nfl_logos(aes(team_abbr = team),
                 width = 0.06,
                 alpha = 0.80) +
  labs(title = "Relative Model Accuracy vs. Offensive EPA/Play by Team",
       subtitle = "2025 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Play",
       y = "Relative Accuracy") +
  theme_minimal() +
  theme(plot.background = element_rect("white"),
        plot.title = element_text(size = 17,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     face = "bold"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 14,
                                  face = "bold"),
        axis.text = element_text(size = 12))

# view plot
pred_epa_plot

# save plot to local files
ggsave("SS-4.5-team-accuracy-epa-plot.png",
       width= 9, height = 6.5,
       dpi = "retina")

# test relationship between off. epa/play and model accuracy
cor.test(team_plot_data_b$epa_play, team_plot_data_b$diff_accuracy)