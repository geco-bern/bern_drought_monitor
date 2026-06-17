# Calculate PCWD for MeteoSwiss daily data: Bern/Zollikofen

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(SPEI)

# ---- settings ----

input_file <- "data-raw/Bern_daily.csv"
output_file <- "data/Bern_pcwd_daily.csv"

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
  "ure200d0"
)

read_meteo_daily <- function(file) {
  read_csv2(
    file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    select(all_of(cols_needed))
}

bern_hist <- read_meteo_daily("data-raw/Bern_hist_daily.csv")
bern_cur  <- read_meteo_daily("data-raw/Bern_cur_daily.csv")

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

    P = as.numeric(rre150d0),
    Tmean = as.numeric(tre200d0),
    Tmin = as.numeric(tre200dn),
    Tmax = as.numeric(tre200dx),
    RHmean = as.numeric(ure200d0),
    wind = as.numeric(fkl010d0),
    Rs = as.numeric(gre000d0) * 86400 / 1e6
  ) |>
  filter(
    date >= as.Date("1981-01-01"),
    !is.na(date),
    !is.na(P),
    !is.na(Tmean),
    !is.na(Tmin),
    !is.na(Tmax),
    !is.na(RHmean),
    !is.na(wind),
    !is.na(Rs)
  )

# ---- calculate PET ----

source("R/calc_pet.R")
meteo_daily <- meteo_daily |>
  mutate(
    PET = calc_pet_fao56_daily(
      tmean = Tmean,
      tmin = Tmin,
      tmax = Tmax,
      rhmean = RHmean,
      wind = wind,
      rs = Rs,
      doy = doy,
      lat_deg = station_lat,
      z = station_elevation
    )
  )

# ---- calculate PCWD ----

meteo_pcwd <- meteo_daily |>
  group_by(year) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    water_balance = P - PET,

    # PCWD: simple cumulative precipitation minus PET
    PCWD = cumsum(water_balance),

    # CWD: drought-stress deficit, never positive
    CWD = purrr::accumulate(
      water_balance,
      ~ min(0, .x + .y),
      .init = 0
    )[-1]
  ) |>
  ungroup()

# ---- save processed data ----

write_csv(meteo_pcwd, output_file)

# ---- plot ----
# plot yearly trajectory of PCWD
pcwd_mean <- meteo_pcwd |>
  filter(year < current_year) |>
  group_by(doy) |>
  summarise(
    PCWD_mean = mean(PCWD, na.rm = TRUE),
    .groups = "drop"
  )

pcwd_current <- meteo_pcwd |>
  filter(year == current_year)

pcwd_plot_simple <- ggplot() +
  geom_line(
    data = pcwd_mean,
    aes(x = doy, y = PCWD_mean),
    colour = "black",
    linewidth = 1
  ) +
  geom_line(
    data = pcwd_current,
    aes(x = doy, y = PCWD),
    colour = "red",
    linewidth = 1.2
  ) +
  labs(
    x = "Day of year",
    y = "PCWD = cumulative P - PET [mm]",
    title = "PCWD Bern/Zollikofen",
    subtitle = paste0(current_year, " compared to mean seasonal cycle")
  ) +
  theme_bw()

pcwd_plot_simple

# plot yearly trajectory of CWD
cwd_mean <- meteo_pcwd |>
  filter(year < current_year) |>
  group_by(doy) |>
  summarise(
    CWD_mean = mean(CWD, na.rm = TRUE),
    .groups = "drop"
  )

cwd_current <- meteo_pcwd |>
  filter(year == current_year)

cwd_plot_simple <- ggplot() +
  geom_line(
    data = cwd_mean,
    aes(x = doy, y = CWD_mean),
    colour = "black",
    linewidth = 1
  ) +
  geom_line(
    data = cwd_current,
    aes(x = doy, y = CWD),
    colour = "red",
    linewidth = 1.2
  ) +
  labs(
    x = "Day of year",
    y = "CWD = climatic water deficit [mm]",
    title = "Climatic water deficit Bern/Zollikofen",
    subtitle = paste0(current_year, " compared to mean seasonal cycle")
  ) +
  theme_bw()

cwd_plot_simple

dir.create("figures/meteo", recursive = TRUE, showWarnings = FALSE)

ggsave(
  "figures/meteo/pcwd_bern_zollikofen_mean_vs_current.png",
  pcwd_plot_simple,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/meteo/cwd_bern_zollikofen_mean_vs_current.png",
  cwd_plot_simple,
  width = 8,
  height = 5,
  dpi = 300
)


