# Analyze and Visualize Data to find insightful conclusions ----

## Load packages and data ----
library(tidyverse)
mlb_qual <- read_csv("data/mlb_qual.csv")

## See some league-wide trends ----

mlb_qual |> 
  filter(pa >= 300) |> 
  summarize(
    avg_ba = mean(batting_avg),
    avg_babip = mean(babip),
    avg_gb_babip = mean(gb_babip),
    avg_xba = mean(xba),
    avg_pull = mean(pull_percent),
    avg_oppo = mean(opposite_percent),
    .by = year
  )

gb_babip_by_year <- mlb_qual |> 
  filter(pa >= 300) |> 
  summarize(
    avg_gb_babip = mean(gb_babip),
    .by = year
  ) |> 
  ggplot(aes(year, avg_gb_babip)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "BABIP on ground balls has increased since the shift was banned",
    subtitle = "Average GB BABIP increased by nearly 10 points between 2022 and 2023",
    y = "Average GB BABIP"
  )

avg_by_year <- mlb_qual |> 
  filter(pa >= 300) |> 
  summarize(
    avg_ba = mean(batting_avg),
    .by = year
  ) |> 
  ggplot(aes(year, avg_ba)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Batting average has not changed significantly since 2023",
    subtitle = "Average batting average remained steady, even after the shift was banned",
    y = "Mean AVG"
  )

# GB BABIP increased slightly, but overall contact remained steady
# are hitters hitting ground balls at similar rates to before the shift was banned?
mlb_qual |> 
  filter(pa >= 300) |> 
  summarize(
    avg_gb_pct = mean(groundballs_percent),
    avg_ld_pct = mean(linedrives_percent),
    avg_fb_pct = mean(flyballs_percent),
    .by = year
  )
# ground ball rate has actually decreased; fly ball rate has increased





## Save out plots ----
ggsave(filename = "plots/gb_babip_by_year.png", plot = gb_babip_by_year)
ggsave(filename = "plots/avg_by_year.png", plot = avg_by_year)




















