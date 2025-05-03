# load packages
library(tidyverse)  # data wrangling
library(janitor)    # data cleaning
library(gt)         # create tables
library(gtExtras)   # table formatting

# load data
kenbart1

# wrangle data into new tibble with only final four teams
# select the columns that will appear in the table
kbfour = kenbart1 %>%
  filter(YEAR == 2025 & SEED == 1) %>%
  select(TEAM, CONF, W, L, EXP, `K TEMPO`)

# add extra columns to tibble
kbfour$PPG = c(83.2, 83.7, 85.4, 74.0)
kbfour$`Opp. PPG` = c(69.2, 62.6, 69.7, 58.3)
kbfour$Bid = c("At-Lg", "Auto", "Auto", "Auto")
kbfour$ODDS = c("+550", "-110", "+300", "+425")

# merge W and L columns into one
kbfour$`W-L` = paste(kbfour$W, kbfour$L, sep = "-")

# reorder columns
kbfour = kbfour %>%
  select(TEAM, CONF, EXP, `K TEMPO`, PPG, `Opp. PPG`, Bid, `W-L`, ODDS) %>%
  relocate(Bid, .after = CONF) %>%
  relocate(EXP, .after = Bid) %>%
  relocate(`W-L`, .after = EXP) %>%
  relocate(PPG, .after = `W-L`) %>%
  relocate(`Opp. PPG`, .after = PPG)

# standardize decimal length in PPG
# standardize deicmal length and rename column K TEMPO
kbfour = kbfour %>%
  mutate(EXP = round(EXP, digits = 2)) %>%
  mutate(`K TEMPO` = round(`K TEMPO`, digit = 1))
  
# check updated tibble
kbfour

# this table would fit the layout of the post better if it were listed vertically
# transpose tibble
t_kbfour = as_tibble(cbind(nms = names(kbfour$TEAM), t(kbfour)))

# add column to serve as row labels
t_kbfour$rownames = c("TEAM", "CONF", "BID", "EXP", "W-L", "PPG", "Opp.PPG", "TEMPO", "ODDS")

# reorder and view new tibble
t_kbfour = t_kbfour %>%
  relocate(rownames, .before = V1) %>%
  print(n = Inf)

# promote row 1 values to column names
t_kbfour = t_kbfour %>%
  row_to_names(row_number = 1) %>%
  print(n = Inf)

# create table
tbl_ffpreview = gt(t_kbfour) %>%
  gt_theme_pff() %>%
  cols_move(columns = "Duke",
            after = "TEAM") %>%
  cols_move(columns = "Houston",
            after = "Duke") %>%
  cols_move(columns = "TEAM",
            after = "Houston") %>%
  cols_align(align = "center",
             columns = c("Duke", "Houston", "TEAM", "Florida", "Auburn")) %>%
  cols_label(TEAM = "",
             Duke = md("<img src='C:/Users/Nick Gasperi/Downloads/duke-logo.png' style='height:30px;'>"),
             Florida = md("<img src='C:/Users/Nick Gasperi/Downloads/florida-logo.png' style='height:30px;'>"),
             Houston = md("<img src='C:/Users/Nick Gasperi/Downloads/houston-logo.png' style='height:30px;'>"),
             Auburn = md("<img src='C:/Users/Nick Gasperi/Downloads/auburn-logo.png' style='height:30px;'>")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = "TEAM")) %>%
  tab_footnote(footnote = md("*By Nick Gasperi | @tbanalysis | Data @nishaanamin*")) %>%
  tab_options(footnotes.font.size = 9)

# view table
tbl_ffpreview

# save table to device's local files
tbl_ffpreview %>%
  gtsave("Sub3.1-gt_ff_preview.png")
