# load packages
library(tidyverse)
library(randomForest)
library(ranger)
library(nflfastR)
library(nflreadr)

# ---------------------- Random Forest Classification Model ----------------------

#                   ---- Data Collection & Preprocessing ----


# load 2018-2025 NFL play-by-play data
nfldata = load_pbp(2018:2025)

# create new dataset that filters data to include independent and dependent variables, plus posteam (used later in the project)
# include only regular season data
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

# create two subsets of the dataset - one for training and one for testing
# drop the season variable after creating each new dataset, as it is only used to in this step and not included in the model
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
# first, define column names of IVs
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

# just one null value present - go into that game's pbp data to investigate
# copying the original dataset, but adding play_id and game_id to locate which game the null value is in
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

# call the same dataset, but change the final filter to include all plays from the game to get context
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
# down is unclear so it will be omitted from the training dataset
dv_cols = setdiff(predictors, "play_type")

training_data_c = training_data %>%
  drop_na(all_of(dv_cols))

# check row count in new tibble to make sure only one column was dropped
nrow(training_data)-nrow(training_data_c)

# generate tibbles to summarize play_type counts in training and testing data sets
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

# calculate baseline assumptions (proportion of each play_type) in testing data
testing_data %>%
  summarize(plays = n(),
            pass = sum(play_type == "pass")/plays,
            run = sum(play_type == "run")/plays,
            punt = sum(play_type == "punt")/plays,
            fg_att = sum(play_type == "field_goal")/plays)


#             ---- Model Training ----

# run model with training data
rf_model_train = randomForest(play_type ~.,
                             ntree = 500,
                             data = training_data_c,
                             importance = TRUE)

# load model results
rf_model_train

# generate variable importance plot
varImpPlot(rf_model_train,
           type = 1,
           main = "Independent Variable Importance")


#             ---- Model Testing & Results ----


# run model with league-wide 2025 testing data
pred_test_league = predict(rf_model_train,
                           newdata = testing_data %>% select(-posteam))

# calculate overall model accuracy
league_accuracy = mean(pred_test_league == testing_data$play_type)

# Generate Confusion Matrix
# create as table
conf_matrix_a = table(predicted = pred_test_league,
                      actual = testing_data$play_type)

# convert matrix values to proportion of actual plays correctly classified within each play_type (recall)
# use 'margin = 2' to group by column & round to one decimal place
conf_matrix_plot_a = round((prop.table(conf_matrix_a,
                                margin = 2)),
                           digits = 3)
# view updated matrix
print(conf_matrix_plot_a)

# convert conf. matrix from a table to dataframe
conf_matrix_plot_d = as.data.frame(conf_matrix_plot_a)

# convert proportionate values to % format
conf_matrix_plot_d$Freq = conf_matrix_plot_d$Freq * 100

# view updated table
conf_matrix_plot_d
