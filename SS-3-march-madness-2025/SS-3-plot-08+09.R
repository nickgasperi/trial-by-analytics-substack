# load packages
library(tidyverse)        # data wrangling
library(readxl)           # to import data

# 
kenbart1

# agg data
# includes final four teams from '08-'24 and the '25 final four teams
kenbart88 = kenbart1 %>%
  select(YEAR, ROUND, SEED, TEAM, SQUAD, comprank) %>%
  filter(ROUND < 5 & YEAR < 2025 | SEED == 1 & YEAR == 2025) %>%
  print(n = Inf)

# plot