library(purrr)
library(here)

dir.create(here("data-raw"), showWarnings = FALSE)

station_urls <- c(
  Bern_hist = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ber/ogd-smn_ber_d_historical.csv",
  Bern_cur  = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ber/ogd-smn_ber_d_recent.csv"
)

iwalk(
  station_urls,
  ~ {
    station_name <- .y
    url <- .x

    destfile <- here("data-raw", paste0(station_name, "_daily.csv"))

    message("Lade ", station_name, " von ", url)
    download.file(url, destfile = destfile, mode = "wb")
  }
)

# MeteoSwiss station data variable names (see also data-raw/ogd-smn_meta_parameters.csv):

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `station_abbr` | MeteoSwiss station abbreviation/code | — |
# | `reference_timestamp` | Date/time to which the daily observation is assigned | timestamp |
# | `tre200d0` | Mean air temperature, 2 m above ground | °C |
# | `tre200dx` | Maximum air temperature, 2 m above ground | °C |
# | `tre200dn` | Minimum air temperature, 2 m above ground | °C |
# | `tre005d0` | Mean air temperature, 5 cm above grass | °C |
# | `tre005dx` | Maximum air temperature, 5 cm above grass | °C |
# | `tre005dn` | Minimum air temperature, 5 cm above grass | °C |
# | `ure200d0` | Mean relative humidity, 2 m above ground | % |
# | `pva200d0` | Mean water-vapour pressure, 2 m above ground | hPa |

# ### Pressure and geopotential height

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `prestad0` | Mean atmospheric pressure at barometer/station altitude—QFE | hPa |
# | `pp0qffd0` | Mean pressure reduced to sea level using actual atmospheric conditions—QFF | hPa |
# | `pp0qnhd0` | Mean pressure reduced to sea level using the standard atmosphere—QNH | hPa |
# | `ppz850d0` | Mean geopotential height of the 850 hPa pressure surface | gpm |
# | `ppz700d0` | Mean geopotential height of the 700 hPa pressure surface | gpm |

# Here, `gpm` means geopotential metres. QFE is station-level pressure; QFF and QNH are two different sea-level reductions.

# ### Wind and foehn

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `dkl010d0` | Mean wind direction | degrees |
# | `fkl010d0` | Mean scalar wind speed | m/s |
# | `fu3010d0` | Same mean scalar wind speed, expressed in km/h | km/h |
# | `fkl010d1` | Maximum 1-second gust | m/s |
# | `fu3010d1` | Maximum 1-second gust | km/h |
# | `fkl010d3` | Maximum 3-second gust | m/s |
# | `fu3010d3` | Maximum 3-second gust | km/h |
# | `wcc006d0` | Foehn index: daily duration classified as foehn | min |

# Thus, the `fkl…` and `fu30…` variables contain the same type of wind measurement in different units.

# ### Precipitation, snow, and water balance

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `rre150d0` | Precipitation total from 06:00 UTC to 06:00 UTC the following day | mm |
# | `rka150d0` | Precipitation total from 00:00 UTC to 00:00 UTC | mm |
# | `rreetsd0` | Daily hydrological water-balance quantity, designated `R−ETS` by MeteoSwiss | mm |
# | `htoautd0` | Automatically measured snow depth at 06:00 UTC | cm |
# | `erefaod0` | FAO reference evapotranspiration/evaporation, daily total | mm/day |

# The difference between `rre150d0` and `rka150d0` is important: they cover different 24-hour windows. `R−ETS` is the precipitation–evapotranspiration water-balance indicator; check which evaporation series is used before deriving it independently.

# ### Radiation and sunshine

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `gre000d0` | Mean global solar radiation | W/m² |
# | `ods000d0` | Mean diffuse shortwave radiation | W/m² |
# | `osr000d0` | Mean reflected shortwave radiation | W/m² |
# | `oli000d0` | Mean incoming longwave radiation | W/m² |
# | `olo000d0` | Mean outgoing longwave radiation | W/m² |
# | `sre000d0` | Sunshine duration, daily total | min |
# | `sremaxdv` | Sunshine duration relative to the maximum astronomically possible duration | % |

# The radiation fields are daily mean fluxes, not daily energy totals. To approximate daily energy in MJ/m², multiply W/m² by `0.0864`.

# ### Degree-day indicators

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `xcd000d0` | Cooling Degree Day, US definition | °C |
# | `xno000d0` | SIA heating-degree value, HGT 12/20 | °C |
# | `xno012d0` | Heating-degree value, ATD 12/12 | °C |

# `HGT 12/20` uses a 12 °C heating threshold and a 20 °C reference indoor temperature. These are daily degree-day contributions; they can be summed over months or seasons.

# ### Soil temperature

# | Variable | Meaning | Unit |
# |---|---|---:|
# | `tso005d0` | Mean soil temperature at 5 cm depth | °C |
# | `tso010d0` | Mean soil temperature at 10 cm depth | °C |
# | `tso020d0` | Mean soil temperature at 20 cm depth | °C |

# A useful naming pattern is:

# - `tre` = air temperature
# - `tso` = soil temperature
# - `ure` = relative humidity
# - `rre`/`rka` = precipitation
# - `fkl` = wind in m/s
# - `fu3` = wind in km/h
# - `dkl` = wind direction
# - `d0` usually = daily mean or daily total
# - `dx` / `dn` = daily maximum / minimum
# - `d1` / `d3` for wind = daily maximum 1-second / 3-second gust

# Source: [official MeteoSwiss parameter-metadata CSV](https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv) and its [opendata.swiss catalogue entry](https://opendata.swiss/en/dataset/automatische-wetterstationen-messwerte/resource/b0ff8b49-06e4-41e0-a6b1-6a4daa87b53a).