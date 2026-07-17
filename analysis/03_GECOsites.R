# Download soil water potential and tree water deficit data
# from TreeNet data portal
# retrieve daily SWP and TWD

# load libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Retrieve SWP Daily data
# Read in SWP data from the Drakau site

metadata_drakau <- read_csv("~/data-raw/drakau_SWP/tn_metadata_L0_2025-01-01_2026-07-17_68c1a39f-291f-47f6-8fc6-09a2d414bf03.csv")

SWPdata_drakau <- read_csv(
  "~/data-raw/drakau_SWP/tn_timeseries_L0_2025-01-01_2026-07-17_68c1a39f-291f-47f6-8fc6-09a2d414bf03.csv"
)

# join data sets
df_drakau <- SWPdata_drakau |>
  left_join(metadata_drakau, by = "series_id")

# Aggregate to daily data

df_drakau_daily <- df_drakau |>
  mutate(date = as_date(ts)) |>
  group_by(date, series_id) |>
  summarise(
    SWP_mean = mean(value, na.rm = TRUE),
    SWP_median = median(value, na.rm = TRUE),
    SWP_min = min(value, na.rm = TRUE),
    SWP_max = max(value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# Filter daily data to months Apr.-Oct.

df_drakau_daily <- df_drakau_daily |>
  filter(month(date) >= 4, month(date) <= 10 ) |> # data for Apr.-Oct.2025
  left_join(metadata_drakau, by = "series_id")



# save csv files daily data from april to october
write_csv(
  df_drakau_daily,
  "~/data/daily_SWP_drakau.csv"
)

# Retrieve daily TWD data
# read timeseries and metadata

raw_TWD_data <- read_csv(
  "~/data-raw/drakau_TWD/tn_timeseries_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv"
)

#Read in the metadata
metadata_TWD <- read_csv("~/data-raw/drakau_TWD/tn_metadata_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv")

# Aggregate to daily data

df_daily_TWD <- raw_TWD_data |>
  mutate(date = as_date(ts)) |>
  group_by(date, series_id) |>
  summarise(

    TWD_mean = mean(twd, na.rm = TRUE),
    TWD_median = median(twd, na.rm = TRUE),
    TWD_min = min(twd, na.rm = TRUE),
    TWD_max = max(twd, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# Filter daily data to time period Apr.-Oct.

df_daily_TWD <- df_daily_TWD|>
  filter(month(date) >= 4, month(date) <= 10 ) |> # data for Apr.-Oct.2025
  left_join(metadata_TWD, by = "series_id")

write_csv(
  df_daily_TWD,
  "~/data/daily_TWD_drakau.csv"
)


