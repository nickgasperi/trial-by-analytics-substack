# load packages
library(tidyverse)
library(randomForest)
library(ranger)
library(nflfastR)
library(nflreadr)


# Part 1 - Model Creation -------------------------------------------------



# increase the timeout limit before the data load - will require longer than the 60 second default
options(timeout = 1000)
# load 2018-2025 NFL play-by-play data
nfldata = load_pbp(2018:2025)

# Data Cleaning and Model Training ----------------------------------------------------------

# filter data to include only dependent variable and independent variables, and include data only from the regular season
# exclude irrelevant play types
all_model_data = nfldata %>%
  filter(season_type == "REG",
         play_type %in% c("run", "pass", "punt", "field_goal"),
         qb_spike == 0,
         qb_kneel == 0,
         aborted_play == 0,
         two_point_attempt == 0) %>%
  select(season,
         play_type,
         down,
         ydstogo,
         goal_to_go,
         yardline_100,
         score_differential,
         half_seconds_remaining,
         game_seconds_remaining,
         game_half,
         shotgun)

# check data structure
str(all_model_data)

# convert categorical independent variables to factors
all_model_data$play_type = as.factor(all_model_data$play_type)
all_model_data$down = as.factor(all_model_data$down)
all_model_data$goal_to_go = as.factor(all_model_data$goal_to_go)
all_model_data$game_half = as.factor(all_model_data$game_half)
all_model_data$shotgun = as.factor(all_model_data$shotgun)

# create subsets of the dataset - one for training, one for validation, and one for testing
# drop the season variable after creating each tibble, as it is not included in the model
training_data = all_model_data %>%
  filter(season %in% c(2018:2024)) %>%
  select(-season)

testing_data = all_model_data %>%
  filter(season == 2025) %>%
  select(-season)

# generate tibbles summarizing count and percentage of each play_type
# training data counts
training_data %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass"),
            run = sum(play_type == "run"),
            punt = sum(play_type == "punt"),
            fg_att = sum(play_type == "field_goal"))

# testing data counts
testing_data %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass"),
            run = sum(play_type == "run"),
            punt = sum(play_type == "punt"),
            fg_att = sum(play_type == "field_goal"))

# using the testing data here to form benchmarks
testing_data %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass")/plays,
            run = sum(play_type == "run")/plays,
            punt = sum(play_type == "punt")/plays,
            fg_att = sum(play_type == "field_goal")/plays)

# check number of rows
nrow(training_data)

# check for null values in dependent variable
training_data %>%
  filter(is.na(play_type))

# check for null values in independent variables
# define column names
predictors = c("play_type",
               "down",
               "ydstogo",
               "goal_to_go",
               "yardline_100",
               "score_differential",
               "half_seconds_remaining",
               "game_seconds_remaining",
               "game_half",
               "shotgun")

# sum null values within each independent variable
sort(colSums(is.na(training_data[, predictors])),
     decreasing = TRUE)

# since only one null value, go back to dataset to replace it with probable value
# copying the original dataset, but adding play_id, game_id, and posteam to locate exactly where the null value is
training_data_b = nfldata %>%
  filter(season_type == "REG",
         season %in% c(2018:2024),
         play_type %in% c("run", "pass", "punt", "field_goal"),
         qb_spike == 0,
         qb_kneel == 0,
         aborted_play == 0,
         two_point_attempt == 0) %>%
  select(play_id,
         game_id,
         posteam,
         play_type,
         down,
         ydstogo,
         goal_to_go,
         yardline_100,
         score_differential,
         half_seconds_remaining,
         game_seconds_remaining,
         game_half,
         shotgun) %>%
  filter(is.na(down)) %>%
  print(n = Inf)

# call the same tibble as the previous, but change the final filter  to include all plays from the game
training_data_b = nfldata %>%
  filter(season_type == "REG",
         season %in% c(2018:2024),
         play_type %in% c("run", "pass", "punt", "field_goal"),
         qb_spike == 0,
         qb_kneel == 0,
         aborted_play == 0,
         two_point_attempt == 0) %>%
  select(play_id,
         game_id,
         posteam,
         play_type,
         down,
         ydstogo,
         goal_to_go,
         yardline_100,
         score_differential,
         half_seconds_remaining,
         game_seconds_remaining,
         game_half,
         shotgun) %>%
  filter(game_id == "2019_06_CAR_TB") %>%
  arrange(play_id) %>%
  print(n = Inf)

# the field goal attempt was the last play of the half, following a punt from the opponent
# it may be a weird situation where a penalty awarded the posteam a FG oppotunity with no time on the clock, so I will exclude it from training data
dv_cols = setdiff(predictors, "play_type")

training_data_c = training_data %>%
  drop_na(all_of(dv_cols))

# check num of rows in new tibble to make sure only one column was dropped
nrow(training_data)-nrow(training_data_c)

# run model
# include importance for ability to plot later
rf_model_train = randomForest(play_type ~.,
                             ntree = 500,
                             data = training_data_c,
                             importance = TRUE)

# load model results
rf_model_train

# check independent variable importance
# type 1 calls Accuracy instead of Gini
varImpPlot(rf_model_train,
           type = 1,
           main = "Independent Variable Importance")


# Model Results with 2025 Data --------------------------------------------

# predict with 2025 data
pred_test_league = predict(rf_model_train,
                           newdata = testing_data)

# test model accuracy
league_accuracy = mean(pred_test_league == testing_data$play_type)

# produce confusion matrix
conf_matrix_a = table(predicted = pred_test_league,
                      actual = testing_data$play_type)

# Plot Confusion Matrix --------------------------------------------

# prep data
# convert values to column-wise pct of total grouped by play_type
# use 'margin = 2' to group by column & round to one decimal place
conf_matrix_plot_a = round((prop.table(conf_matrix_a,
                                margin = 2)),
                           digits = 3)
# view updated matrix
print(conf_matrix_plot_a)

# convert table to dataframe
conf_matrix_plot_d = as.data.frame(conf_matrix_plot_a)

# plot data as heatmap with geom_tile()
conf_heat_map = ggplot(data = conf_matrix_plot_d,
                       aes(x = Predicted,
                           y = Actual,
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
       x = "Predicted",
       y = "Actual") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# view plot
conf_heat_map

# save plot to local files
ggsave("SS-4.2-confusion-matrix-plot.png",
       width= 7, height = 7,
       dpi = "retina")

# Part 2 - Team-Level Testing -------------------------------------------------
# create an additional data frame that copies all_model_data plus adds in pos. team name
model_2_data = nfldata %>%
  filter(season_type == "REG",
         season == 2025,
         play_type %in% c("run", "pass", "punt", "field_goal"),
         qb_spike == 0,
         qb_kneel == 0,
         aborted_play == 0,
         two_point_attempt == 0) %>%
  select(season,
         posteam,
         play_type,
         down,
         ydstogo,
         goal_to_go,
         yardline_100,
         score_differential,
         half_seconds_remaining,
         game_seconds_remaining,
         game_half,
         shotgun) %>%
  select(-season)

# create results summary in a new data frame with pos. team added 
rf_test_results = data.frame(team = model_2_data$posteam,
                             predicted = pred_test_league,
                             actual = testing_data$play_type)

# 
team_results_summary = rf_test_results %>%
  group_by(team) %>%
  summarize(n_plays = n(),
            n_correct = sum(actual == predicted),
            team_accuracy = mean(actual == predicted),
            vs_league_avg = team_accuracy - league_accuracy)

# view results table
team_results_summary %>%
  arrange(-vs_league_avg) %>%
  print(n = Inf)

