# load packages
library(tidyverse)
library(gt)
library(gtExtras)

# load data
kenbart1

# wrangle data into new tibble to include only selected values for all teams in dataset
# use filter() to set parameters for which teams meet the criteria of the table
kball = kenbart1 %>%
  select(YEAR,
         SEED,
         TEAM,
         BARTHAG,
         WAB) %>%
  filter(BARTHAG > 0.970
         & WAB >= 9.5) %>%
  arrange(-BARTHAG)

# create table
tbloneseed = gt(kball) %>%
  gt_highlight_rows(rows = c(2, 4, 6, 11)) %>%
  tab_footnote(footnote = md("*By Nick Gasperi | @tbanalysis | Data @nishaanamin*")) %>%
  tab_options(footnotes.font.size = 9) %>%
  cols_align(align = "center",
             columns = "SEED") %>%
  gt_theme_pff() %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = "YEAR",
                                   rows = c(2, 4, 6, 11)))
    
# view table
tbloneseed

# save table to local files
tbloneseed %>%
  gtsave("Sub3.2-gt_barthag_wab.png")
