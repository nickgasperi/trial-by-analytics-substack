# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)

# ---------------------- Part 2 of 2: Team-Level Accuracy ----------------------

# calls data from testing_data - same dataset used for league-wide testing in Part 1
# print results summary in a new data frame
team_test_obs = data.frame(team = testing_data$posteam,
                             predicted = pred_test_league,
                             actual = testing_data$play_type)

# group results by team
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

