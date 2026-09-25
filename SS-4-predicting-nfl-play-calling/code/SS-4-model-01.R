# load packages
library(tidyverse)
library(randomForest)
library(ranger)
library(caret)
library(nflfastR)
library(nflreadr)

# ---------------------- Random Forest Classification Model ----------------------

#                   ---- Data Collection & Preprocessing ----


# load 2018-2025 NFL play-by-play data
nfldata = load_pbp(2018:2025)

# filter to regular season plays and select dependent/independent variables
# posteam is retained for team-level analysis later in the project
all_model_data = nfldata %>%
  filter(season_type == "REG",
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
         shotgun)

# convert categorical independent variables to factors
all_model_data$play_type = as.factor(all_model_data$play_type)
all_model_data$down = as.factor(all_model_data$down)
all_model_data$goal_to_go = as.factor(all_model_data$goal_to_go)
all_model_data$game_half = as.factor(all_model_data$game_half)
all_model_data$shotgun = as.factor(all_model_data$shotgun)

# split into training (2018-2024) and testing (2025) datasets
# season is dropped after filtering, as it is not used in the model
# posteam is dropped from training only, since team identity should not be a predictor
training_data = all_model_data %>%
  filter(season %in% c(2018:2024)) %>%
  select(-season, -posteam)

testing_data = all_model_data %>%
  filter(season == 2025) %>%
  select(-season)

# check for null values in dependent variable
training_data %>%
  filter(is.na(play_type))

# check for null values in independent variables
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

sort(colSums(is.na(training_data[, predictors])),
     decreasing = TRUE)

# one null value found in 'down' - locate the game to investigate context
# rebuild the filtered dataset with play_id/game_id added for lookup purposes
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

# call the same dataset, but change the final filter to include all plays from the game for full context
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

# the field goal attempt was the final play of the half, following a punt from the opponent
# down is ambiguous in this context, so the row is dropped from the training dataset
dv_cols = setdiff(predictors, "play_type")

training_data_c = training_data %>%
  drop_na(all_of(dv_cols))

# check row count in new tibble to confirm only one column was dropped
nrow(training_data)-nrow(training_data_c)

# summarize play_type counts in training and testing datasets
training_data_c %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass"),
            run = sum(play_type == "run"),
            punt = sum(play_type == "punt"),
            fg_att = sum(play_type == "field_goal"))

testing_data %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass"),
            run = sum(play_type == "run"),
            punt = sum(play_type == "punt"),
            fg_att = sum(play_type == "field_goal"))

# calculate naive baseline (proportion of each play_type) in the 2025 testing data
testing_data %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass")/plays,
            run = sum(play_type == "run")/plays,
            punt = sum(play_type == "punt")/plays,
            fg_att = sum(play_type == "field_goal")/plays)


#             ---- Model Training ----

# train random forest model
rf_model_train = randomForest(play_type ~.,
                             ntree = 500,
                             data = training_data_c,
                             importance = TRUE)

# view training results (OOB error, confusion matrix)
rf_model_train

# generate variable importance plot
varImpPlot(rf_model_train,
           type = 1,
           main = "Independent Variable Importance")


#             ---- Model Testing & Results ----

# generate predictions using 2025 testing data (posteam excluded as a predictor)
pred_test_league = predict(rf_model_train,
                           newdata = testing_data %>% select(-posteam))

# build confusion matrix (predicted vs. actual)
conf_matrix_a = table(predicted = pred_test_league,
                      actual = testing_data$play_type)

# convert matrix values to % of actual plays correctly classified within each play_type (recall)
# margin = 2 normalizes by column (actual play type)
conf_matrix_plot_a = round((prop.table(conf_matrix_a,
                                margin = 2)),
                           digits = 3)

print(conf_matrix_plot_a)

# convert confusion matrix to a dataframe for use in ggplot
conf_matrix_plot_d = as.data.frame(conf_matrix_plot_a)

# convert proportions to percentage format
conf_matrix_plot_d$Freq = conf_matrix_plot_d$Freq * 100

conf_matrix_plot_d      # dataframe ready to plot

# generate summary table
confusionMatrix(pred_test_league, testing_data$play_type)

