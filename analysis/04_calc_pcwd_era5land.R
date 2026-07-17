# TODO: develop ERA5Land PCWD based on one of the below options:

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
