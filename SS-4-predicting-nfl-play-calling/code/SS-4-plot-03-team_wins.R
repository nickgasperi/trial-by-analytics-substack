# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# plot relative model accuracy vs. wins by team
team_acc_wins_plot = ggplot(data = team_plot_data_b,
                        aes(x = wins,
                            y = diff_accuracy)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  geom_hline(yintercept = mean(team_plot_data_b$diff_accuracy),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_vline(xintercept = mean(team_plot_data_b$wins),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_nfl_logos(aes(team_abbr = team),
                 width = 0.06,
                 alpha = 0.80) +
  annotate("text",
           x = c(4.2, 4.2, 10.8, 10.2),
           y = c(-0.025, 0.010, -0.025, 0.010),
           label = c("Unconventional Derogatory",
                     "Conventional & Underwhelming",
                     "Unconventional Complementary",
                     "Conventional & Successful"),
           fontface = "italic") +
  labs(title = "Relative Model Accuracy vs. Wins by Team",
       subtitle = "2025 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Wins",
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
team_acc_wins_plot

# save plot to local files
ggsave("SS-4.3-team-accuracy-wins-plot.png",
       width= 9, height = 6.5,
       dpi = "retina")
