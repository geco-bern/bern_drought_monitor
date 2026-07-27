library(here)

# A) Update underlying data set with freshly downloaded data
dir.create(here("data"), showWarnings = FALSE)
# MeteoSwiss data:
source(here("analysis", "00_download_meteoswiss_data.R"))
# creates: data-raw/meteoswiss_station_bern/Bern_cur_daily.csv and Bern_hist_daily.csv

source(here("analysis", "01_calc_pcwd.R"))
# creates: data/Bern_pcwd_daily.csv

# GECO-sites:
if (FALSE) { # NOTE: activate this for automatic update (TODO: automatize all manual steps)
  source(here("analysis", "03_GECOsites.R")) # TODO: this does not yet exist
}
# creates: data/daily_SWP_drakau.csv and daily_TWD_drakau.csv

# SwissEOVHI:
source(here("analysis", "02_swissEOVHI.R"))
# creates: data/swisseo_vhi_bern_plot_data_5dates.tif and swisseo_vhi_bern_stats.csv

# ERA5-Land:
if (FALSE) { # NOTE: activate this for automatic update (TODO: automatize all manual steps)
  source(here("analysis", "04_calc_pcwd_era5land.R"))
}
# creates: data/ERA5LandCWD/data_derived_03_....nc
