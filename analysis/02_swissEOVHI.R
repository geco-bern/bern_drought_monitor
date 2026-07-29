# Download and visualize swissEO VHI for the Bern region
# - finds the latest available VHI dataset
# - finds the closest dataset around 30 days before that date
# - crops the raster to the Bern region
# - creates a comparison plot

# Install packages if needed:
# install.packages(c("httr2", "jsonlite", "terra", "sf", "ggplot2", "patchwork", "dplyr", "lubridate", "scales"))

library(httr2)
library(jsonlite)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)
library(scales)
library(here)

# ---------------------------
# Settings
# ---------------------------
stac_root  <- "https://data.geo.admin.ch/api/stac/v0.9"
collection <- "ch.swisstopo.swisseo_vhi_v100"

# Region of interest
bbox_wgs84 <- c(7.1, 46.9, 7.53, 47.19)

# Output folder
out_dir <- here("data-raw","swisseo_vhi_bern")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Search window: broad enough to also find data that may be published with a delay
search_from <- Sys.Date() - days(180)
search_to   <- Sys.Date()

# ---------------------------
# Helper functions
# ---------------------------

stac_search <- function(datetime_interval, limit = 500) {
  body <- list(
    collections = list(collection),
    datetime = datetime_interval,
    limit = limit
  )

  req <- request(file.path(stac_root, "search")) |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(body, auto_unbox = TRUE)

  resp <- req_perform(req)
  txt <- resp_body_string(resp)
  fromJSON(txt, simplifyVector = FALSE)
}

item_datetime <- function(feature) {
  dt <- feature$properties$datetime

  if (is.null(dt)) {
    dt <- feature$properties$start_datetime
  }

  dt <- as.character(dt)

  lubridate::ymd_hms(dt, tz = "UTC", quiet = TRUE)
}

get_features <- function(datetime_interval) {
  js <- stac_search(datetime_interval)
  feats <- js$features
  if (length(feats) == 0) stop("No STAC items found in the selected time range.")
  feats[order(vapply(feats, item_datetime, as.POSIXct("1970-01-01", tz = "UTC")), decreasing = TRUE)]
}

pick_tif_asset <- function(feature, layer = "forest") {
  assets <- feature$assets
  keys <- names(assets)
  hrefs <- vapply(assets, function(a) a$href %||% "", character(1))

  # Select the requested 10 m VHI layer: "forest" or "vegetation"
  pattern <- paste0(layer, "-10m\\.tif$")

  idx <- grep(pattern, keys, ignore.case = TRUE)

  if (length(idx) == 0) {
    stop("No ", layer, "-10m GeoTIFF asset found in the STAC item.")
  }

  hrefs[idx[1]]
}

`%||%` <- function(x, y) if (is.null(x)) y else x

crop_to_aoi_from_url <- function(href) {
  r <- rast(href)

  # Create the bounding box as an sf polygon and transform it into the raster CRS
  aoi <- st_as_sfc(st_bbox(c(
    xmin = bbox_wgs84[1], ymin = bbox_wgs84[2],
    xmax = bbox_wgs84[3], ymax = bbox_wgs84[4]
  ), crs = 4326)) |>
    st_transform(crs(r)) |>
    vect()

  crop(r, aoi, mask = TRUE)
}

raster_to_df <- function(r, date_label) {
  if (nlyr(r) > 1) r <- r[[1]]
  names(r) <- "vhi"

  # VHI valid range is 0–100; values above 100 are invalid/special values
  r[r > 100] <- NA

  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  df$date <- date_label
  df
}

find_latest_valid_feature_by_day <- function(start_date = Sys.Date(), days_back = 180, layer = "forest") {
  dates <- seq.Date(
    from = as.Date(start_date),
    to = as.Date(start_date) - days(days_back),
    by = "-1 day"
  )

  for (i in seq_along(dates)) {
    d <- as.Date(dates[i], origin = "2026-01-01") # search valid dates in 2026

    interval <- paste0(d, "T00:00:00Z/", d, "T23:59:59Z")

    message("Searching date: ", d)

    feats <- tryCatch(
      get_features(interval),
      error = function(e) NULL
    )

    if (is.null(feats) || length(feats) == 0) next

    for (feature in feats) {
      href <- tryCatch(
        pick_tif_asset(feature, layer = layer),
        error = function(e) NULL
      )

      if (is.null(href)) next

      r <- crop_to_aoi_from_url(href)
      if (nlyr(r) > 1) r <- r[[1]]

      vals <- values(r, na.rm = FALSE)
      n_valid <- sum(!is.na(vals) & vals >= 0 & vals <= 100)
      n_special <- sum(!is.na(vals) & vals > 100)

      message("Checking: ", d, " | valid pixels: ", n_valid, " | special >100: ", n_special)

      if (n_valid > 0) {
        message("Latest valid data found: ", d)
        return(feature)
      }
    }
  }

  stop("No valid VHI data found.")
}

# ---------------------------
# 1) Find the latest five valid available datasets
# ---------------------------

vhi_layer <- "forest"
n_dates <- 5

latest_features <- vector("list", n_dates)
latest_dates <- as.Date(rep(NA, n_dates))

# Neuester gültiger Datensatz
latest_features[[1]] <- find_latest_valid_feature_by_day(
  start_date = Sys.Date(),
  days_back = 180,
  layer = vhi_layer
)

latest_dates[1] <- as.Date(
  item_datetime(latest_features[[1]])
)

message(
  "Valid VHI dataset 1 of ",
  n_dates,
  ": ",
  latest_dates[1]
)

# Vier weitere gültige Datensätze suchen
for (i in 2:n_dates) {
  latest_features[[i]] <- find_latest_valid_feature_by_day(
    start_date = latest_dates[i - 1] - 1,
    days_back = 180,
    layer = vhi_layer
  )

  latest_dates[i] <- as.Date(
    item_datetime(latest_features[[i]])
  )


  message(
    "Valid VHI dataset ",
    i,
    " of ",
    n_dates,
    ": ",
    latest_dates[i]
  )
}

# Keep aliases for code that expects the latest dataset
latest_feature <- latest_features[[1]]
latest_date <- latest_dates[1]

# ---------------------------
# 2) Read remote asset and crop it to the AOI
# ---------------------------
latest_crops <- lapply(
  latest_features,
  function(feature) {
    href <- pick_tif_asset(
      feature,
      layer = vhi_layer
    )

    crop_to_aoi_from_url(href)
  }
)


# ---------------------------
# 3) Process for plotting in vignette
# ---------------------------
raster_stack <- do.call(
  c,
  latest_crops
)

time(raster_stack) <- latest_dates

# VHI auf gültigen Bereich begrenzen
dim(raster_stack)

for (i in seq_len(nlyr(raster_stack))) {
  idx <- raster_stack[[i]] < 0 | raster_stack[[i]] > 100
  raster_stack[[i]][idx] <- NA
}
# Für leaflet nach WGS84 transformieren
raster_stack_wgs84 <- terra::project(raster_stack, "EPSG:4326", method = "near")


# ---------------------------
# 4) Save processed data for vignette
# ---------------------------

dir.create(
  here("data"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Save cropped raster stack with five dates
writeRaster(
  raster_stack_wgs84,
  here(
    "data",
    "swisseo_vhi_bern_plot_data_5dates.tif"
  ),
  overwrite = TRUE
)

# Save summary statistics for the latest valid dataset only
plot_df <- raster_to_df(
  latest_crops[[1]],
  paste0(latest_date, " (latest valid available)")
)

stats <- plot_df |>
  summarise(
    date = unique(date),
    min_vhi = min(vhi, na.rm = TRUE),
    mean_vhi = mean(vhi, na.rm = TRUE),
    median_vhi = median(vhi, na.rm = TRUE),
    max_vhi = max(vhi, na.rm = TRUE)
  )

write.csv(
  stats,
  here(
    "data",
    "swisseo_vhi_bern_stats.csv"
  ),
  row.names = FALSE
)
