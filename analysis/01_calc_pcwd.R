# Calculate PCWD from MeteoSwiss daily data for Bern/Zollikofen.
#
# Two PET series are retained:
#   1. MeteoSwiss FAO reference PET (1981-present), with missing daily PET
#      linearly interpolated between neighbouring observations.
#   2. Thornthwaite PET (1864-present), calculated from monthly temperature.

library(readr)
library(dplyr)
library(lubridate)
library(SPEI)
library(cwd)
library(tidyr)
library(here)

# ---- settings ----

output_file <- here("data", "Bern_pcwd_daily.csv")
station_lat <- 46.99
doy_reset <- yday(ymd("2000-12-01"))

dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)

# ---- read and prepare daily observations ----

read_meteo_daily <- function(file) {
  read_csv2(
    file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    select(
      station_abbr,
      reference_timestamp,
      rre150d0,
      rka150d0,
      erefaod0,
      tre200d0
    )
}

meteo_daily <- bind_rows(
  read_meteo_daily(here("data-raw", "Bern_hist_daily.csv")),
  read_meteo_daily(here("data-raw", "Bern_cur_daily.csv"))
) |>
  distinct(station_abbr, reference_timestamp, .keep_all = TRUE) |>
  mutate(
    date = as_date(dmy_hm(reference_timestamp, quiet = TRUE)),
    across(
      -c(station_abbr, reference_timestamp, date),
      ~ readr::parse_double(.x)
    )
  ) |>
  filter(!is.na(date)) |>
  arrange(date) |>
  mutate(
    year = year(date),
    doy = yday(date),
    # rre150d0 is available for the full record. Use calendar-day
    # precipitation as a fallback for its single missing recent observation.
    P = coalesce(rre150d0, rka150d0),
    P_calendar = rka150d0,
    Tmean = tre200d0,
    PET_FAO_MeteoSwiss_observed = erefaod0
  )

# ---- linear interpolation of missing MeteoSwiss FAO PET ----

pet_observed <- !is.na(meteo_daily$PET_FAO_MeteoSwiss_observed)
pet_interpolated <- approx(
  x = as.numeric(meteo_daily$date[pet_observed]),
  y = meteo_daily$PET_FAO_MeteoSwiss_observed[pet_observed],
  xout = as.numeric(meteo_daily$date),
  method = "linear",
  rule = 1,
  ties = "ordered"
)$y

meteo_daily <- meteo_daily |>
  mutate(
    PET_FAO_MeteoSwiss = coalesce(
      PET_FAO_MeteoSwiss_observed,
      pet_interpolated
    ),
    PET_FAO_MeteoSwiss_imputed =
      is.na(PET_FAO_MeteoSwiss_observed) & !is.na(PET_FAO_MeteoSwiss)
  )

if (anyNA(meteo_daily$PET_FAO_MeteoSwiss[meteo_daily$date >= as.Date("1981-01-01")])) {
  stop("MeteoSwiss FAO PET still contains missing values after interpolation.")
}

# ---- MeteoSwiss FAO PET PCWD, 1981-present ----

meteo_daily_fao <- meteo_daily |>
  filter(date >= as.Date("1981-01-01")) |>
  mutate(water_balance_FAO_MeteoSwiss = P_calendar - PET_FAO_MeteoSwiss)

pcwd_fao <- cwd::cwd(
  meteo_daily_fao,
  varname_wbal = "water_balance_FAO_MeteoSwiss",
  varname_date = "date",
  thresh_drop = 0,
  doy_reset = doy_reset
)$df |>
  transmute(
    date,
    PET_FAO_MeteoSwiss_observed,
    PET_FAO_MeteoSwiss,
    PET_FAO_MeteoSwiss_imputed,
    water_balance_FAO_MeteoSwiss,
    PCWD_FAO_MeteoSwiss = deficit
  )

# ---- Thornthwaite PET PCWD, 1864-present ----

meteo_monthly_thorn <- meteo_daily |>
  mutate(month_date = floor_date(date, "month")) |>
  group_by(month_date) |>
  summarise(
    Tmean_month = if (all(!is.na(Tmean))) mean(Tmean) else NA_real_,
    n_days_available = n_distinct(date),
    n_days_month = days_in_month(first(month_date)),
    month_complete = n_days_available == n_days_month && !is.na(Tmean_month),
    .groups = "drop"
  ) |>
  complete(
    month_date = seq(min(month_date), max(month_date), by = "month")
  ) |>
  mutate(
    n_days_month = days_in_month(month_date),
    # The final, ongoing month is allowed to use its available observations.
    month_complete = coalesce(month_complete, FALSE),
    month_status = case_when(
      month_complete ~ "complete",
      month_date == max(month_date) ~ "ongoing",
      TRUE ~ "incomplete"
    )
  ) |>
  arrange(month_date)

first_month <- min(meteo_monthly_thorn$month_date)
temperature_monthly_ts <- ts(
  meteo_monthly_thorn$Tmean_month,
  start = c(year(first_month), month(first_month)),
  frequency = 12
)

meteo_monthly_thorn <- meteo_monthly_thorn |>
  mutate(
    PET_THORN_month = as.numeric(
      SPEI::thornthwaite(
        Tave = temperature_monthly_ts,
        lat = station_lat,
        na.rm = FALSE,
        verbose = FALSE
      )
    )
  )

meteo_daily_thorn <- meteo_daily |>
  mutate(month_date = floor_date(date, "month")) |>
  left_join(
    meteo_monthly_thorn |>
      select(
        month_date,
        PET_THORN_month,
        n_days_month,
        month_complete,
        month_status
      ),
    by = "month_date"
  ) |>
  mutate(
    PET_THORN = PET_THORN_month / n_days_month,
    water_balance_THORN = P - PET_THORN
  )

pcwd_thorn <- cwd::cwd(
  meteo_daily_thorn,
  varname_wbal = "water_balance_THORN",
  varname_date = "date",
  thresh_drop = 0,
  doy_reset = doy_reset
)$df |>
  transmute(
    date,
    year = year(date),
    doy = yday(date),
    P,
    PET_THORN,
    PET_THORN_month,
    water_balance_THORN,
    PCWD_THORN = deficit,
    month_complete,
    month_status
  )

# ---- combine and save ----

meteo_pcwd <- pcwd_thorn |>
  left_join(pcwd_fao, by = "date") |>
  arrange(date)

write_csv(meteo_pcwd, output_file)
