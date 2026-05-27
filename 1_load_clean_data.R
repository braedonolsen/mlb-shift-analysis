# Load and clean data for easy usage ----
# This script will outline my process for acquiring and cleaning my MLB data before I delve into the more complex analysis

## Load Packages ----
library(tidyverse)
library(baseballr)

mlb_2019_2025 <- read_csv("data/mlb_all.csv")

## filter the data to only include hitters with a large enough sample size ----
mlb_2019_2025 |> 
  summarize(
    max_pa = max(pa),
    min_pa = min(pa)
  )
# we have observations with as few as 10 plate appearances--that is too few!
mlb_2019_2025 |> 
  slice_min(pa, n = 10)
# several of these are pitchers, which can explain the lack of a true sample size

# filter the data to only include qualified hiters (min. 100 plate appearances)
mlb_qual <- mlb_2019_2025 |> 
  filter(pa >= 100)

## create some useful new variables ----
# I want BABIP specific to ground balls--direct measure of shift impact, as the shift targeted grounders
# also want straight-away%, which measures percentage of balls hit up the middle of the field
# create a variable indicating if a season took place when shift was allowed or not

mlb_qual <- mlb_qual |> 
  janitor::clean_names() |> 
  mutate(
    gb_babip = b_hit_ground / (b_hit_ground + b_out_ground),
    straightaway_percent = 100 - pull_percent - opposite_percent,
    shift_allowed = factor(
      case_when(
        year < 2023 ~ "Shift Allowed",
        year >= 2023 ~ "Shift Banned"
      ),
      levels = c("Shift Allowed", "Shift Banned")
    )
  )


## Final checks before saving out batter data ----

# check for missing values
mlb_qual |> 
  summarise(across(everything(), ~sum(is.na(.))))
# no missingness issues

# check for duplicates--could be possible with in-season trades
mlb_qual |>
  count(last_name_first_name, year) |>
  filter(n > 1)
# only one name returned--Max Muncy
# there happen to be two distinct Max Muncys in the MLB--funny coincidence, but not an issue

## save out data ----
write_csv(mlb_qual, "data/mlb_qual.csv")











