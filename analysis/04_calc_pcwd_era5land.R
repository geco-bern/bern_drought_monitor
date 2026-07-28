# Visualize ERA5-Land CWD computations for the Bern region
# - downloads the latest available CWD data set from preprocessed data on UBELIX (already cropped to the Bern region)
# - (crops the raster to the Bern region)
# - creates a map

library(here)
library(httr2)
library(jsonlite)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)
library(scales)
library(tidync)

# ---------------------------
# Settings
# ---------------------------
# NOTE: below settings are outcommented since they are hardcoded in the manual steps:
# nc_root         <- "/storage/capacity/occr_geco/data_2/archive/era5land_munoz-sabater_2021/data_derived_03_daily_pcwd.narm_v2-doy-reset_netcdf/data_derived_03_daily_pcwd_v2-doy_2024_r-generated.nc"
# nc_root_cropped <- "~/data_derived_03_daily_pcwd_v2-doy_2024_r-generated_regionBern.nc"

# Region of interest, WGS84 / EPSG:4326
# Input order here: longitude (min, max), latitude (min, max)
# roi_lon <- c(6.9, 7.7)
# roi_lat <- c(46.7,47.2)

# Output folder
out_dir <- here("data","ERA5LandCWD")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# Helper functions
# ---------------------------

# -

# ---------------------------
# 1) Download preprocessed ERA5-Land PCWD data set as netCDF (Option 1 from remarks below)
# ---------------------------

# Download latest ERA5Land-T onto UBELIX:
# 1a) DO MANUALLY download new data:
# ssh ubelix
# cd ~
# sbatch ~/GitHub/fabern/download-ECMWF-data/download_era5land_munoz-sabater_2021_tp-pev-t2m-sp-ssr-str.sh
# # The above should download latest, hourly variables specified in the shell script to a
# # location specified in the shell script: e.g. /storage/capacity/occr_geco/data_2/scratch/fbernhard/ERA5Land-T_2000-2026_run2026-07-16
# # then follow up with:
# sbatch ~/GitHub/fabern/download-ECMWF-data/aggregate_ECMWF_UTCdaily.sh
# # The above should call `python3 aggregate_ECMWF_UTCdaily.py` and depending on the specified arguments
# # it should aggregate the whole provided data set (in hourly resolution) into daily mean/min/max values.
# # and store there at the specified location: e.g. /storage/capacity/occr_geco/data_2/scratch/fbernhard/ERA5Land-T_2000-2026_run2026-07-16/aggregated_Bern
# 1b) DO MANUALLY recompute the PCWD (globally or for the provided region)
# ssh ubelix
# cd ~/GitHub/geco-bern/cwd_global/;
# jid01=$(sbatch --parsable src/ERA5Land-2-BernArea/main-noNA_01tidy.sh)
# jid02=$(sbatch --dependency=afterok:$jid01 --parsable src/ERA5Land-2-BernArea/main-noNA_02pcwd.sh)
# jid03=$(sbatch --dependency=afterok:$jid02 --parsable src/ERA5Land-2-BernArea/main-noNA_03netcdf.sh)

# Use previously computed (global) PCWD:
# 1c) DO MANUALLY:

# ssh ubelix
# cd /storage/capacity/occr_geco/data_2/archive/era5land_munoz-sabater_2021/data_derived_03_daily_pcwd.narm_v2-doy-reset_netcdf/
# cd /storage/capacity/occr_geco/data_2/scratch/fbernhard/ERA5Land-T_2000-2026_run2026-07-16_derived_03_daily_pcwd.narm_v2-doy-reset_netcdf/
# srun --account=invest --qos=job_icpu-stocker --ntasks=1 --cpus-per-task=8 --mem-per-cpu=8G --job-name="crop_ERA5L_CWD" --time=1:00:00 --pty bash
# module load netCDF
# module load CDO
# ncdump -hcs data_derived_03_daily_pcwd_v2-doy_2026_r-generated.nc
# cdo sinfov data_derived_03_daily_pcwd_v2-doy_2026_r-generated.nc
# cdo sellonlatbox,6.9,7.7,46.7,47.2 data_derived_03_daily_pcwd_v2-doy_2026_r-generated.nc ~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionBern.nc
# cdo sellonlatbox,5.9,10.5,45.8,47.9 data_derived_03_daily_pcwd_v2-doy_2026_r-generated.nc ~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionCH.nc
# cdo sellonlatbox,-8.7,24.8,36.5,54.8 data_derived_03_daily_pcwd_v2-doy_2026_r-generated.nc ~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc
# ( possibly also for other years:
#   cdo sellonlatbox,6.9,7.7,46.7,47.2 data_derived_03_daily_pcwd_v2-doy_2025_r-generated.nc ~/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionBern.nc
#   cdo sellonlatbox,5.9,10.5,45.8,47.9 data_derived_03_daily_pcwd_v2-doy_2025_r-generated.nc ~/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionCH.nc
#   cdo sellonlatbox,-8.7,24.8,36.5,54.8 data_derived_03_daily_pcwd_v2-doy_2025_r-generated.nc ~/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionEUROPE.nc
# # the above takes about 10 seconds for 1 file
# ncdump -hcs ~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionBern.nc
# ncdump -hcs ~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc
# exit # srun-exit
# exit # ssh-exit

# Transfer files from UBELIX to repository:
# 1d) DO MANUALLY:
# rsync -avz --progress --no-owner --no-group fb24k097@submit04.unibe.ch:~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionBern.nc ~/GitHub/geco-bern/drought_switzerland_blog/data/ERA5LandCWD/
# rsync -avz --progress --no-owner --no-group fb24k097@submit04.unibe.ch:~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionCH.nc ~/GitHub/geco-bern/drought_switzerland_blog/data/ERA5LandCWD/
# rsync -avz --progress --no-owner --no-group fb24k097@submit04.unibe.ch:~/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc ~/GitHub/geco-bern/drought_switzerland_blog/data/ERA5LandCWD/
# ncdump -hcs data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionBern.nc
# ncdump -hcs data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionCH.nc
# ncdump -hcs data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc

# (Possibly also for other years:)
# rsync -avz --progress --no-owner --no-group fb24k097@submit04.unibe.ch:~/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionBern.nc ~/GitHub/geco-bern/drought_switzerland_blog/data/ERA5LandCWD/
# rsync -avz --progress --no-owner --no-group fb24k097@submit04.unibe.ch:~/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionCH.nc ~/GitHub/geco-bern/drought_switzerland_blog/data/ERA5LandCWD/
# rsync -avz --progress --no-owner --no-group fb24k097@submit04.unibe.ch:~/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionEUROPE.nc ~/GitHub/geco-bern/drought_switzerland_blog/data/ERA5LandCWD/


cwd_ERA5Land <- terra::rast(
  here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionCH.nc")
  # here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc")
)
cwd_ERA5Land_df <- dplyr::bind_rows(
  tidync::hyper_tibble(here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionCH.nc")),
  tidync::hyper_tibble(here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionCH.nc")),
  # tidync::hyper_tibble(here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc")),
  # tidync::hyper_tibble(here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2025_r-generated_regionEUROPE.nc")),
  # here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2026_r-generated_regionEUROPE.nc")
) |> mutate(time = lubridate::ymd(time))
# terra::plot(cwd_ERA5Land[[167]])
# terra::plot(cwd_ERA5Land[[197]])
# terra::plot(cwd_ERA5Land[[228]])
# terra::plot(cwd_ERA5Land[[259]])

# a) plot some dates:
# dates_to_plot <- c("2024-06-15","2024-07-15","2024-08-15","2024-09-15")
dates_to_plot <- c("2026-05-15","2026-06-15","2026-07-15","2026-07-16")
# which(time(cwd_ERA5Land) %in% c("2024-06-15","2024-07-15","2024-08-15","2024-09-15"))
rng <- range(values(cwd_ERA5Land), na.rm = TRUE)
plt_ERA5Land_map <- terra::plot(
  cwd_ERA5Land[[time(cwd_ERA5Land) %in% dates_to_plot]],
  range = rng)

# b) plot the average evolution for the region
cwd_ERA5Land_df_mean <- cwd_ERA5Land_df |> group_by(time) |> summarise(pcwd_mm = mean(pcwd_mm))

plt_ERA5Land_temp <- ggplot(cwd_ERA5Land_df_mean, aes(x = time, y=pcwd_mm)) +
  geom_line() + theme_bw() + labs(x=NULL, y= "Region average of PCWD (mm)")
# # individual pixels:
# ggplot(cwd_ERA5Land_df, aes(x = time, y=pcwd_mm, color = paste0(lon, ",", lat))) +
#   geom_line()


# ---------------------------
# 2) Crop to region
# ---------------------------

# NOTE: this is already perfromed on UBELIX during preprocessing


# ---------------------------
# 3) Save processed data for vignette (nothing stored above code is repeated in *.qmd)
# ---------------------------
# cwd_ERA5Land_df_mean # store into out_dir
# # cwd_ERA5Land # already stored in out_dir at here("data/ERA5LandCWD/data_derived_03_daily_pcwd_v2-doy_2024_r-generated_regionBern.nc")
#
# plt_ERA5Land_map
# plt_ERA5Land_temp


# ---------------------------
# 4) Developer notes
# ---------------------------

# NOTE: below are different options how to include ERA5Land PCWD into the blog:
# TODO: select one and develop the corresponding code

# Option 1: use the already computed PCWD for 1950 to 2024 stored here (also uses Priestley-Taylor, not `pev`):
#/storage/capacity/occr_geco/data_2/archive/era5land_munoz-sabater_2021/data_derived_03_daily_pcwd.narm_v2-doy-reset_netcdf/

# Option 2:
# download newest ERA5Land data, aggregate to daily data and then freshly compute PCWD for 2025 and ongoing 2026
# this involves steps:
# 1) download: https://github.com/fabern/download-ECMWF-data/blob/main/download_era5land_munoz-sabater_2021.sh
# 2) aggregate daily: https://github.com/fabern/aggregate-ERA5Land-daily/blob/main/main.py
# 3) run pcwd:
#     i) https://github.com/geco-bern/cwd_global/blob/main/src/ERA5Land-fullResNoNA/main-noNA_01tidy.sh
#     ii) https://github.com/geco-bern/cwd_global/blob/main/src/ERA5Land-fullResNoNA/main-noNA_02pcwd.sh
#     iii) https://github.com/geco-bern/cwd_global/blob/main/src/ERA5Land-fullResNoNA/main-noNA_03netcdf.sh
#
#   NOTE: also this code uses Priestley-Taylor, not `pev`.
#         "This might actually be a good choice since ERA5Land documentation states:
#         The Potential Evaporation field (pev, parameter Id 228251) is largely underestimated over deserts
#         and high-forested areas. This is due to a bug in the code that does not allow transpiration to
#         occur in the situation where there is no low vegetation."

# Option 3:
# adapt above code to run only for a small subset of the Bern region instead of globally

# Option 4a:
# instead of globally do it for a sub-region by
# using the newly available point-wise download (exists since 2025-06:
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land-timeseries?tab=documentation)
# - [ ] see example data set: ef16e607e4d15aefea70bd4accd6700a.zip
#
# Option 4b:
# instead of globally do it for a sub-region by
# using the newly available  “Analysis-ready cloud-optimised (ARCO)”
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=analysis_ready_data





        # # attempt 2: download rds files
        # dir.create(here("data/ERA5LandCWD/"), showWarnings = FALSE)
        # for (lon in seq(6.9, 7.7, by = 0.1)){
        #   # Try with RDS (before aggregation to nc on UBELIX)
        #   src_folder <- "/storage/capacity/occr_geco/data_2/archive/era5land_munoz-sabater_2021/data_derived_02_daily_pcwd.narm_v2-doy-reset"
        #   src_file   <- paste0(src_folder, "/ERA5Land_pcwd_LON_", sprintf("%+08.3f",lon), ".rdsallyears_onlypcwd_mm.rds")
        #   trg_folder <- here("data/ERA5LandCWD/")
        #   rsync_cmd <- sprintf("rsync -avz --no-owner --no-group fb24k097@submit04.unibe.ch:%s %s",
        #                        src_file,
        #                        trg_folder)
        #   system(rsync_cmd)
        #
        # }
        # rds_content <- lapply(
        #   list.files(trg_folder, full.names = T),
        #   readr::read_rds) |>
        #   bind_rows()
        #
        # rds_content |>
        #   dplyr::filter(lat >= 46.70, lat <= 47.2)
