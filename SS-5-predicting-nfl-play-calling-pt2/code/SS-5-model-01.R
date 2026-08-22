

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

# create data frame for all plotting
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