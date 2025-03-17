# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(gt)
library(data.table)
library(gtExtras)

# load data
nfldata = load_pbp(2024)

# create tibble 1
sb1data = nfldata %>%
  filter(posteam == "KC" | posteam == "PHI",
         play_type == "run" | play_type == "pass",
         qb_kneel == 0, qb_spike == 0,
         !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            epaper = sum(epa)/sum(plays),
            passrate = sum(pass)/sum(plays)) %>%
  rename("team" = "posteam") %>%
  print(n = Inf)

# create tibble 2
sb2data = nfldata %>%
  filter(defteam == "KC" | defteam == "PHI",
         play_type == "run" | play_type == "pass",
         qb_kneel == 0, qb_spike == 0,
         !is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(playsdef = n(),
            opp_epaper = sum(epa)/sum(playsdef)) %>%
  rename("team" = "defteam") %>%
  print(n = Inf)

# join tibbles
sb1data = sb1data %>%
  left_join(sb2data, by = "team") %>%
  print(n = Inf)

# add W-L column
sb1data$record = c("17-2", "17-3")
# add PPG
sb1data$ppg = c(23.2, 28.4)
# add PPG allowed  
sb1data$opp_ppg = c(19.42, 17.9)
# add PPG diff
sb1data$point_diff = sb1data$ppg - sb1data$opp_ppg

# filter out unwanted columns by selecting just the columns you want in the upcoming table
# we will add record in later as a column header
## this will allow us to properly format our data in the gt table
sb1data = sb1data %>%
  select(team, epaper, passrate, opp_epaper, ppg, opp_ppg, point_diff) %>%
  relocate(passrate, .after = opp_epaper)

# view updated tibble
sb1data

# reduce number of decimals on both epa columns
sb1data %>%
  mutate(epaper = round(epaper, digits = 2),
         opp_epaper = round(opp_epaper, digits = 2),
         passrate = round(passrate, digits = 2),
         opp_ppg = round(opp_ppg, digits = 1),
         point_diff = round(point_diff, digits = 1)
         )

# transpose tibble before creating table with gt()
# since we are transposing, we will have to convert this back to a tibble later
t_sb1data = transpose(sb1data)

# drop first row - rename column to posteam after
t_sb1data = t_sb1data %>% slice(-1)

# restore original column names
colnames(t_sb1data) = c("Chiefs", "Eagles")

# gt does not include row titles, so create new column to serve as row titles
t_sb1data$rownames = c("EPA/Play", "Opp. EPA/Play", "Pass Rate",
                       "PPG", "Opp. PPG", "Net PPG")

# convert back to tibble
t_sb1data = as_tibble(t_sb1data)

# reclassify data types
t_sb1data$Chiefs = as.numeric(t_sb1data$Chiefs)
t_sb1data$Eagles = as.numeric(t_sb1data$Eagles)

# view tibble - should be ready to go now
t_sb1data

# create table
sbtbl1 = gt(t_sb1data) %>%
  cols_move(columns = c("Chiefs","Eagles"),
            after = rownames) %>%
  cols_label(rownames = "Record",
             Chiefs = "17-2",
             Eagles = "17-3") %>%
  cols_align(align = "center") %>%
  fmt_number(columns = c("Chiefs", "Eagles"),
             decimals = 2) %>%
  tab_header(title = md("**2025 SUPER BOWL PREVIEW**")) %>%
  tab_spanner(label = "KC",
              columns = "Chiefs") %>%
  tab_spanner(label = "PHI",
              columns = "Eagles") %>%
  gt_nfl_wordmarks(locations = cells_column_spanners()) %>%
  tab_footnote(footnote = md("**data includes reg. season & playoffs*")) %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | Data @nflfastR")) %>%
  gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = "rownames"))
  
# view table
sbtbl1

# save table
sbtbl1 %>%gtsave("SubSt2.3 - gt_sb_preview.png")
