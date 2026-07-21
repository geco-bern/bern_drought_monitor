# Download soil water potential and tree water deficit data
# from TreeNet data portal
# retrieve daily SWP and TWD

# load libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(lubridate)


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

# raw_TWD_data <- read_csv(
#   "~/data-raw/drakau_TWD/tn_timeseries_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv"
# )
#
# #Read in the metadata
# metadata_TWD <- read_csv("~/data-raw/drakau_TWD/tn_metadata_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv")
#
# # Aggregate to daily data
#
# df_daily_TWD <- raw_TWD_data |>
#   mutate(date = as_date(ts)) |>
#   group_by(date, series_id) |>
#   summarise(
#
#     TWD_mean = mean(twd, na.rm = TRUE),
#     TWD_median = median(twd, na.rm = TRUE),
#     TWD_min = min(twd, na.rm = TRUE),
#     TWD_max = max(twd, na.rm = TRUE),
#     n_obs = n(),
#     .groups = "drop"
#   )
#
# # Filter daily data to time period Apr.-Oct.
#
# df_daily_TWD <- df_daily_TWD|>
#   filter(month(date) >= 4, month(date) <= 10 ) |> # data for Apr.-Oct.2025
#   left_join(metadata_TWD, by = "series_id")
#
# write_csv(
#   df_daily_TWD,
#   "~/data/daily_TWD_drakau.csv"
# )

# Read raw data and metadata
raw_TWD <- read_csv(
  "~/data-raw/drakau_TWD/tn_timeseries_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv",
  show_col_types = FALSE
)

metadata_TWD <- read_csv(
  "~/data-raw/drakau_TWD/tn_metadata_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv",
  show_col_types = FALSE
)

# Correct species metadata
metadata_clean <- metadata_TWD |>
  transmute(
    series_id,
    tree_id,
    tree_name,
    tree_genus_species_original = tree_genus_species,
    tree_genus_species = if_else(
      tree_name %in% c(
        "Fichte1_P3",
        "Fichte2_P3",
        "Fichte3_P3"
      ),
      "Abies alba",
      tree_genus_species
    ),
    site = stringr::str_extract(tree_name, "(G1|P1|P2|P3)$")
  ) |>
  distinct(series_id, .keep_all = TRUE)

safe_stat <- function(x, fun) {
  if (all(is.na(x))) NA_real_ else fun(x, na.rm = TRUE)
}

# Join metadata and calculate daily values
df_daily_TWD_clean <- raw_TWD |>
  left_join(
    metadata_clean,
    by = "series_id",
    relationship = "many-to-one"
  ) |>
  mutate(
    date = lubridate::as_date(
      lubridate::with_tz(ts, "Europe/Zurich")
    )
  ) |>
  group_by(
    date,
    series_id,
    tree_id,
    tree_name,
    site,
    tree_genus_species_original,
    tree_genus_species
  ) |>
  summarise(
    TWD_mean = if (all(is.na(twd))) NA_real_ else mean(twd, na.rm = TRUE),
    TWD_median = if (all(is.na(twd))) NA_real_ else median(twd, na.rm = TRUE),
    TWD_min = if (all(is.na(twd))) NA_real_ else min(twd, na.rm = TRUE),
    TWD_max = if (all(is.na(twd))) NA_real_ else max(twd, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) |>
  mutate(
    qc_flag = if_else(
      tree_name == "Buche1_P2" &
        date >= as.Date("2026-06-03"),
      "Excluded: sensor anomaly",
      "Valid"
    ),
    across(
      c(TWD_mean, TWD_median, TWD_min, TWD_max),
      ~ if_else(qc_flag == "Valid", .x, NA_real_)
    )
  ) |>
  filter(lubridate::month(date) %in% 4:10)

# Save cleaned daily dataset

write_csv(
  df_daily_TWD_clean,
  path.expand("~/data/daily_TWD_drakau_clean.csv")
)
