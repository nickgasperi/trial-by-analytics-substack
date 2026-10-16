# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# ----------------- Sect. 1 - Analysis of Historical Performance -----------------

## MEAN ##
# calculate mean of 1H Chase Brown rush yards
mean(nfldata4x$first_half_yds)

## FIVE-NUMBER SUMMARY ##
# get 6 number summary (5-num summary plus mean)
summary(nfldata4x$first_half_yds)

# calculate percentile of each betting line's yards total
# define universal equation to plug in values
pctile_cb = ecdf(nfldata4x$first_half_yds)

# plug in values
pctile_1_cb = pctile_cb(25)
pctile_2_cb = pctile_cb(35)
pctile_3_cb = pctile_cb(45)

## RATE OF EXCEEDANCE ##
# calculate new percentiles to feed into exceedance rate calculation
pctile_4_cb = pctile_cb(24)
pctile_5_cb = pctile_cb(34)
pctile_6_cb = pctile_cb(44)

# translate percentile to exceedance rate
excd_rate_4_cb = 1 - pctile_4_cb
excd_rate_5_cb = 1 - pctile_5_cb
excd_rate_6_cb = 1 - pctile_6_cb

# create tibble summarizing historical performance
historical_summary_cb = tibble(yards = c("25+", "35+", "45+"),
                               odds = c("-177", "+130", "+259"),
                               excd_rate = c(excd_rate_4_cb, excd_rate_5_cb, excd_rate_6_cb) * 100
                              )

# ----------------- Sect. 2 - Calculating Implied Probability of Odds -----------------

# calculate market implied probability values at each level of yards
# define universal equation to plug in values
imp_prob = function(odds) {
  if(odds < 0) {
    abs(odds) / (abs(odds) + 100)
  } else {
    100 / (odds + 100)
  }
}

# plug in odds to calculate implied probability at each level
prob_1_cb = imp_prob(-177)
prob_2_cb = imp_prob(130)
prob_3_cb = imp_prob(259)

# 
imp_prob_cb = tibble(yards = c("25+", "35+", "45+"),
                     odds = c("-177", "+130", "+259"),
                     imp_prob = c(prob_1_cb, prob_2_cb, prob_3_cb) * 100,
                     excd_rate = c(excd_rate_4_cb, excd_rate_5_cb, excd_rate_6_cb) * 100
                    )

# ----------------- Sect. 3 - Interpreting Value of Props -----------------

# create tibble calculating summarizing historical performance AND difference between excd rates & implied market probability
odds_summary_cb = tibble(yards = c("25+", "35+", "45+"),
                         odds = c("-177", "+130", "+259"),
                         imp_prob = c(prob_1_cb, prob_2_cb, prob_3_cb) * 100,
                         excd_rate = c(excd_rate_4_cb, excd_rate_5_cb, excd_rate_6_cb) * 100,
                         diff = round(imp_prob - excd_rate, 2)
                        )
