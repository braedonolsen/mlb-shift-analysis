# Analyze and Visualize Pitcher Data to find insightful conclusions ----

## Load packages and data ----
library(tidyverse)
pitchers_qual <- read_csv("data/pitchers_qual.csv")

## See some league-wide trends ----
pitchers_qual |> 
  summarize(
    avg_k_pct = mean(k_percent),
    avg_bb_pct = mean(bb_percent),
    avg_era = mean(p_era),
    avg_hard_hit = mean(hard_hit_percent),
    .by = year
  ) |> 
  arrange(year)

# strikeout percent is relatively the same
# hard hit percent has increased steadily since 2019 though
  # this reflects a league-wide trend of increased home runs/hard hits, independent of the shift ban
  # recall that fly balls have increased since the banning of the shift as well
  # possibility: shift ban worked on the ground, but swing-for-fences approach absorbed any gains on the ground



















