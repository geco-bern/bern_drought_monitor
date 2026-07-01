calc_pet_fao56_daily <- function(tmean, tmin, tmax, rhmean, wind, rs, doy, lat_deg, z) {
  # FAO-56 Penman-Monteith reference ET0 [mm/day]

  Gsc <- 0.0820
  sigma <- 4.903e-9

  lat <- lat_deg * pi / 180

  # Atmospheric pressure [kPa]
  P_atm <- 101.3 * ((293 - 0.0065 * z) / 293)^5.26

  # Psychrometric constant [kPa °C-1]
  gamma <- 0.000665 * P_atm

  # Slope vapour pressure curve [kPa °C-1]
  delta <- 4098 * (0.6108 * exp((17.27 * tmean) / (tmean + 237.3))) /
    (tmean + 237.3)^2

  # Saturation vapour pressure [kPa]
  es_tmax <- 0.6108 * exp((17.27 * tmax) / (tmax + 237.3))
  es_tmin <- 0.6108 * exp((17.27 * tmin) / (tmin + 237.3))
  es <- (es_tmax + es_tmin) / 2

  # Actual vapour pressure from mean RH [kPa]
  ea <- es * rhmean / 100

  # Extraterrestrial radiation [MJ m-2 day-1]
  dr <- 1 + 0.033 * cos(2 * pi / 365 * doy)
  solar_dec <- 0.409 * sin(2 * pi / 365 * doy - 1.39)
  ws <- acos(-tan(lat) * tan(solar_dec))

  Ra <- (24 * 60 / pi) * Gsc * dr *
    (ws * sin(lat) * sin(solar_dec) +
       cos(lat) * cos(solar_dec) * sin(ws))

  # Clear-sky radiation [MJ m-2 day-1]
  Rso <- (0.75 + 2e-5 * z) * Ra

  # Net shortwave radiation
  albedo <- 0.23
  Rns <- (1 - albedo) * rs

  # Net longwave radiation
  tmax_k <- tmax + 273.16
  tmin_k <- tmin + 273.16

  Rnl <- sigma *
    ((tmax_k^4 + tmin_k^4) / 2) *
    (0.34 - 0.14 * sqrt(ea)) *
    (1.35 * pmin(rs / Rso, 1) - 0.35)

  # Net radiation
  Rn <- Rns - Rnl

  # Soil heat flux for daily timestep
  G <- 0

  # FAO-56 Penman-Monteith
  ET0 <- (
    0.408 * delta * (Rn - G) +
      gamma * (900 / (tmean + 273)) * wind * (es - ea)
  ) / (
    delta + gamma * (1 + 0.34 * wind)
  )

  pmax(ET0, 0)
}
