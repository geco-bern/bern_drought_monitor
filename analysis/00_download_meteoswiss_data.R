library(purrr)

dir.create("data-raw", showWarnings = FALSE)

station_urls <- c(
  Bern_hist = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ber/ogd-smn_ber_d_historical.csv",
  Bern_cur  = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ber/ogd-smn_ber_d_recent.csv"
)

iwalk(
  station_urls,
  ~ {
    station_name <- .y
    url <- .x

    destfile <- file.path("data-raw", paste0(station_name, "_daily.csv"))

    message("Lade ", station_name, " von ", url)
    download.file(url, destfile = destfile, mode = "wb")
  }
)
