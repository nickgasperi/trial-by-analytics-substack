# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)

# ---------------------- Part 2 of 2: Team-Level Model Accuracy ----------------------

# test model at team level
team_test_obs = data.frame(team = testing_data$posteam,
                             predicted = pred_test_league,
                             actual = testing_data$play_type)

# summarize results by  team
# include difference between team level accuracy and league avg (74.0%)
team_results_summary = team_test_obs %>%
  group_by(team) %>%
  summarize(n_plays = n(),
            n_correct = sum(actual == predicted),
            team_accuracy = mean(actual == predicted),
            diff_accuracy = team_accuracy - league_accuracy) %>%
  arrange(-diff_accuracy)

# export dataset as csv to local files
write.csv(team_results_summary,
          "C:/Users/Nick Gasperi/Documents/GitHub/trial-by-analytics-substack/SS-4-predicting-nfl-play-calling/data/team_results_summary.csv",
          row.names = FALSE)


# prep team-level model results data for plotting
# calculate epa/play by team using same conditions as original 2025 testing data
team_plot_data_a = nfldata %>%
  filter(season_type == "REG",
         season == 2025,
         !is.na(epa),
         !is.na(posteam),
         play_type %in% c("run", "pass", "punt", "field_goal"),
         qb_spike == 0,
         qb_kneel == 0,
         aborted_play == 0,
         two_point_attempt == 0) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            epa_play = sum(epa)/plays) %>%
  select(-plays,
         team = posteam) %>%
  print(n = Inf)

# create data frame to use for all team-level plots
# ppg and wins variables were manually added
team_plot_data_b = team_results_summary %>%
  left_join(team_plot_data_a,
            by = "team") %>%
  select(team,
         n_plays,
         team_accuracy,
         diff_accuracy,
         epa_play) %>%
  left_join(base_stats_2025_nfl_team,
            by = "team") %>%
  print(n = Inf)
