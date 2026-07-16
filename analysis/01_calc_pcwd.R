# Calculate PCWD for MeteoSwiss daily data: Bern/Zollikofen

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(SPEI)
library(cwd)

# ---- settings ----

input_file <- here("data-raw","Bern_daily.csv")
output_file <- here("data-dynamic","Bern_pcwd_daily.csv")

station_lat <- 46.99
current_year <- 2026
station_elevation <- 553

dir.create("data", showWarnings = FALSE)

# ---- read data ----

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

bern_hist <- read_meteo_daily(here("data-raw","Bern_hist_daily.csv"))
bern_cur  <- read_meteo_daily(here("data-raw","Bern_cur_daily.csv"))

meteo_daily_raw <- bind_rows(bern_hist, bern_cur) |>
  distinct(reference_timestamp, .keep_all = TRUE)

# Check column names once
names(meteo_daily_raw)

# ---- prepare daily data ----

meteo_daily <- meteo_daily_raw |>
  transmute(
    date = as_date(dmy_hm(reference_timestamp)),
    year = year(date),
    doy = yday(date),

    # meaning of variable described in ogd-smn_meta_parameters.csv
    P = as.numeric(rre150d0),
    Tmean = as.numeric(tre200d0),
    Tmin = as.numeric(tre200dn),
    Tmax = as.numeric(tre200dx),
    RHmean = as.numeric(ure200d0),
    wind = as.numeric(fkl010d0),
    Rs = as.numeric(gre000d0) * 86400 / 1e6,
    SW_IN  = as.numeric(gre000d0),
    LW_IN  = as.numeric(oli000d0),
  ) |>

  # xxx Achtung: Filter sind gefährlich. Versuche möglichst zu vermeiden und falls nötig gapfilling zu machen.
  # vor 2010 wurde die Strahlung nicht gemessen, daher erst Daten ab diesem Jahr beachtet.
  filter(
    date >= as.Date("2010-01-01"),
    # !is.na(date),
    # !is.na(P),
    # !is.na(Tmean),
    # !is.na(Tmin),
    # !is.na(Tmax),
    # !is.na(RHmean),
    # !is.na(wind),
    # !is.na(Rs),
    # !is.na(SW_IN),
    # !is.na(LW_IN)
  )

visdat::vis_miss(meteo_daily)

# ---- calculate PET ----
# use function cwd::pet()

meteo_daily <- meteo_daily |>
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

meteo_daily_fao <- meteo_daily |>
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
  meteo_daily,
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

# Combined output

meteo_pcwd_wide <- meteo_pcwd_pt |>
  dplyr::select(
    date, year, doy, P,
    PET_PT = PET,
    water_balance_PT = water_balance,
    PCWD_PT = PCWD
  ) |>
  dplyr::left_join(
    meteo_pcwd_fao |>
      dplyr::select(
        date,
        PET_FAO = PET,
        water_balance_FAO = water_balance,
        PCWD_FAO = PCWD
      ),
    by = "date"
  )

# ---- save processed data ----
write_csv(meteo_pcwd_wide, output_file)
