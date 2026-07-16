# Calculate PCWD for MeteoSwiss daily data: Bern/Zollikofen

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(SPEI)
library(cwd)
library(tidyr)
library(here)

# ---- settings ----

output_file <- here("data", "Bern_pcwd_daily.csv")

station_lat <- 46.99
station_elevation <- 553

dir.create(
  here("data"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Day of year corresponding to December 1
doy_reset <- yday(ymd("2000-12-01"))

# ---- read raw data ----

cols_needed <- c(
  "station_abbr",
  "reference_timestamp",
  "rre150d0",
  "tre200d0",
  "tre200dn",
  "tre200dx",
  "fkl010d0",
  "gre000d0",
  "ure200d0",
  "oli000d0",
  "olo000d0",
  "osr000d0"
)

read_meteo_daily <- function(file) {
  read_csv2(
    file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    select(all_of(cols_needed))
}

bern_hist <- read_meteo_daily(
  here("data-raw", "Bern_hist_daily.csv")
)

bern_cur <- read_meteo_daily(
  here("data-raw", "Bern_cur_daily.csv")
)

meteo_daily_raw <- bind_rows(
  bern_hist,
  bern_cur
) |>
  distinct(
    station_abbr,
    reference_timestamp,
    .keep_all = TRUE
  )

# ---- prepare complete daily time series ----

meteo_daily_all <- meteo_daily_raw |>
  transmute(
    station_abbr,

    date = as_date(
      dmy_hm(
        reference_timestamp,
        quiet = TRUE
      )
    ),

    P = readr::parse_double(rre150d0),
    Tmean = readr::parse_double(tre200d0),
    Tmin = readr::parse_double(tre200dn),
    Tmax = readr::parse_double(tre200dx),
    RHmean = readr::parse_double(ure200d0),
    wind = readr::parse_double(fkl010d0),

    # Convert global radiation from W m-2 to MJ m-2 day-1
    Rs = readr::parse_double(gre000d0) * 86400 / 1e6,

    SW_IN = readr::parse_double(gre000d0),
    LW_IN = readr::parse_double(oli000d0),

    year = year(date),
    doy = yday(date)
  ) |>
  filter(!is.na(date)) |>
  arrange(date)

visdat::vis_miss(meteo_daily_all)

# Complete period for temperature-based Thornthwaite PET
meteo_daily_thorn <- meteo_daily_all

# Period with radiation measurements for PT and FAO56
meteo_daily_recent <- meteo_daily_all |>
  filter(date >= as.Date("2010-01-01"))

# ---- calculate Priestley Taylor PET ----
# use function cwd::pet()

meteo_daily_pt <- meteo_daily_recent |>
  mutate(
    # Approximate net radiation because outgoing shortwave
    # and outgoing longwave radiation are unavailable
    albedo = 0.23,
    emissivity = 0.98,
    sigma = 5.670374419e-8,

    SW_OUT_est = albedo * SW_IN,
    LW_OUT_est = emissivity * sigma * (Tmean + 273.15)^4,

    NETRAD_est = SW_IN - SW_OUT_est + LW_IN - LW_OUT_est,

    PA = 101325 * ((293 - 0.0065 * station_elevation) / 293)^5.26,
    PET_PT = 86400 * cwd::pet(NETRAD_est, Tmean, PA),
    pwbal = P - PET_PT
  )

# ---- FAO56 PET sensitivity analysis ----

source(here("R/calc_pet.R"))

meteo_daily_fao <- meteo_daily_recent |>
  mutate(
    PET_FAO = calc_pet_fao56_daily(
      tmean = Tmean,
      tmin = Tmin,
      tmax = Tmax,
      rhmean = RHmean,
      wind = wind,
      rs = Rs,
      doy = doy,
      lat_deg = station_lat,
      z = station_elevation
    ),
    pwbal_fao = P - PET_FAO
  )

# ---- calculate PCWD ----
# use cwd::cwd() function for potential cumulative water deficit

# Day of year corresponding to December 1 (used to reset PCWD each year)
doy_reset <- lubridate::yday(lubridate::ymd("2000-12-01"))

# PCWD with Priestley-Taylor PET from cwd::pet()
out_pcwd_pt <- cwd::cwd(
  meteo_daily_pt,
  varname_wbal = "pwbal",
  varname_date = "date",
  # thresh_terminate = 0,
  thresh_drop = 0.0,
  doy_reset = doy_reset
)

meteo_pcwd_pt <- out_pcwd_pt$df |>
  transmute(
    date,
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    P,
    PET_method = "Priestley-Taylor",
    PET = PET_PT,
    water_balance = pwbal,
    PCWD = deficit
  )

# PCWD with FAO56 PET sensitivity
out_pcwd_fao <- cwd::cwd(
  meteo_daily_fao,
  varname_wbal = "pwbal_fao",
  varname_date = "date",
  # thresh_terminate = 0,
  thresh_drop = 0.0,
  doy_reset = doy_reset
)

meteo_pcwd_fao <- out_pcwd_fao$df |>
  transmute(
    date,
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    P,
    PET_method = "FAO56",
    PET = PET_FAO,
    water_balance = pwbal_fao,
    PCWD = deficit
  )

# ---- Thorntwaite PET and PCWD ----

meteo_monthly_thorn <- meteo_daily_thorn |>
  mutate(
    month_date = floor_date(date, unit = "month")
  ) |>
  group_by(month_date) |>
  summarise(
    Tmean_month = mean(Tmean),
    n_days_available = n_distinct(date),
    n_days_month = days_in_month(first(month_date)),
    month_complete = n_days_available == n_days_month,
    .groups = "drop"
  ) |>
  tidyr::complete(
    month_date = seq(
      min(month_date),
      max(month_date),
      by = "month"
    )
  ) |>
  mutate(
    n_days_month = days_in_month(month_date),

    # Missing months introduced by complete() count as incomplete
    month_complete = coalesce(month_complete, FALSE),

    month_status = if_else(
      month_complete,
      "complete",
      "incomplete"
    )
  ) |>
  arrange(month_date)

first_month <- min(
  meteo_monthly_thorn$month_date,
  na.rm = TRUE
)

temperature_monthly_ts <- ts(
  meteo_monthly_thorn$Tmean_month,
  start = c(
    year(first_month),
    month(first_month)
  ),
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

# Join monthly Thornthwaite PET back to the complete daily time series
meteo_daily_thorn_calc <- meteo_daily_thorn |>
  mutate(
    month_date = floor_date(date, unit = "month")
  ) |>
  left_join(
    meteo_monthly_thorn |>
      select(
        month_date,
        Tmean_month,
        n_days_available,
        n_days_month,
        month_complete,
        month_status,
        PET_THORN_month
      ),
    by = "month_date"
  ) |>
  mutate(
    # Distribute monthly PET across all calendar days of the month
    PET_THORN = PET_THORN_month / n_days_month,
    water_balance_THORN = P - PET_THORN
  )

out_pcwd_thorn <- cwd::cwd(
  meteo_daily_thorn_calc,
  varname_wbal = "water_balance_THORN",
  varname_date = "date",
  thresh_drop = 0,
  doy_reset = doy_reset
)

meteo_pcwd_thorn <- out_pcwd_thorn$df |>
  transmute(
    date,
    year = year(date),
    doy = yday(date),
    P,
    PET_method = "Thornthwaite",
    PET = PET_THORN,
    PET_month = PET_THORN_month,
    water_balance = water_balance_THORN,
    PCWD = deficit,
    month_complete,
    month_status
  )

# ---- combine all PCWD methods in one wide data set ----

meteo_pcwd_wide <- meteo_pcwd_thorn |>
  transmute(
    date,
    year,
    doy,
    P,

    PET_THORN = PET,
    PET_THORN_month = PET_month,
    water_balance_THORN = water_balance,
    PCWD_THORN = PCWD,

    month_complete,
    month_status
  ) |>
  left_join(
    meteo_pcwd_pt |>
      transmute(
        date,
        PET_PT = PET,
        water_balance_PT = water_balance,
        PCWD_PT = PCWD
      ),
    by = "date"
  ) |>
  left_join(
    meteo_pcwd_fao |>
      transmute(
        date,
        PET_FAO = PET,
        water_balance_FAO = water_balance,
        PCWD_FAO = PCWD
      ),
    by = "date"
  ) |>
  arrange(date)

write_csv(
  meteo_pcwd_wide,
  output_file
)
