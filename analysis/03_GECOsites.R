# Download soil water potential and tree water deficit data
# from TreeNet data portal
# retrieve daily SWP and TWD

# For tree water deficit, L2 data quality was chosen
# while for soil water potential, L0 data quality was chosen

# Below code performs a quality check removing TWD outliers above 500um
# and removign SWP outliers below 6000 kPa


# load libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# A) Retrieve SWP Daily data and TWD data

# manually download the files and store them so that they can be read in by part B)


# B) Read in SWP data from the Drakau site and TWD data
# B1) Read raw data and metadata
metadata_drakau <- read_csv(
  "~/data-raw/drakau_SWP/tn_metadata_L0_2025-01-01_2026-07-17_68c1a39f-291f-47f6-8fc6-09a2d414bf03.csv"
)

SWPdata_drakau <- read_csv(
  "~/data-raw/drakau_SWP/tn_timeseries_L0_2025-01-01_2026-07-17_68c1a39f-291f-47f6-8fc6-09a2d414bf03.csv"
)

# B2) Read raw data and metadata
metadata_TWD <- read_csv(
  "~/data-raw/drakau_TWD/tn_metadata_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv",
  show_col_types = FALSE
)

raw_TWD <- read_csv(
  "~/data-raw/drakau_TWD/tn_timeseries_L2_2025-01-01_2026-07-17_1f2a1587-bc94-4b55-9ec0-a83c76c0459d.csv",
  show_col_types = FALSE
)


# C) Process SWP Daily data
SWP_threshold <- -6000 # kPa

# join data sets
df_drakau <- SWPdata_drakau |>
  left_join(metadata_drakau, by = "series_id")|>
  mutate(
    SWP_excluded = !is.na(value) & value < SWP_threshold,
    value = if_else(
      SWP_excluded,
      NA_real_,
      value
    )
  )

# Aggregate to daily data

df_drakau_daily <- df_drakau |>
  mutate(date = as_date(ts)) |>
  group_by(date, series_id) |>
  summarise(
    SWP_mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
    SWP_median = if (all(is.na(value))) NA_real_ else median(value, na.rm = TRUE),
    SWP_min = if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
    SWP_max = if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
    n_obs = n(),
    n_excluded = sum(SWP_excluded, na.rm = TRUE),
    .groups = "drop"
  )

# Filter daily data to months Apr.-Oct.

df_drakau_swp_daily <- df_drakau_daily |>
  filter(month(date) >= 4, month(date) <= 10 ) |> # data for Apr.-Oct.2025
  left_join(metadata_drakau, by = "series_id")


# save csv files daily data from april to october
write_csv(
  df_drakau_swp_daily,
  here("data", "daily_SWP_drakau.csv"),
)


# C) Process daily TWD data
TWD_threshold <- 500 # um

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
    ),
    TWD_excluded = !is.na(twd) & twd > TWD_threshold,
    twd = if_else(
      TWD_excluded,
      NA_real_,
      twd
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
    n_excluded = sum(TWD_excluded, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    qc_flag = if_else(
      n_excluded > 0,
      "Excluded: TWD > 500 µm",
      "Valid"
    )
  ) |>
  filter(lubridate::month(date) %in% 4:10)

# Save cleaned daily dataset

write_csv(
  df_daily_TWD_clean,
  here("data", "daily_TWD_drakau.csv"),
)
