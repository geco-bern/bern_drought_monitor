# Swiss Drought Monitoring Blog

## Project Overview

This repository contains the workflow used to generate the Swiss drought monitoring blog and associated figures for the Bern region.

The project combines meteorological observations from MeteoSwiss with vegetation health information from swissEO to assess current drought conditions. The workflow calculates climatic water deficit indicators and visualizes recent vegetation stress patterns.

### Main outputs

* Climatic Water Deficit (CWD) time series
* Potential Climatic Water Deficit (PCWD) time series
* Vegetation Health Index (VHI) maps derived from swissEO
* Updated drought monitoring blog post rendered from Quarto

---

## Data

### MeteoSwiss observations

Daily meteorological observations are downloaded from the MeteoSwiss Open Government Data portal:

* Bern historical station data
* Bern recent station data

Source:

https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/

The data download is automated through:

```r
analysis/download_meteoswiss_data.R
```

Downloaded files are stored in:

```text
data-raw/
```

### swissEO Vegetation Health Index (VHI)

Vegetation Health Index (VHI) raster products are retrieved from the Swiss Federal Geoportal STAC API.

Collection:

```text
ch.swisstopo.swisseo_vhi_v100
```

Source:

https://data.geo.admin.ch/

The workflow automatically searches for the most recent valid VHI dataset and downloads the required raster subset for the Bern region.

### Data availability

Large raster datasets are not intended to be stored permanently in the repository. They are downloaded automatically during execution and stored locally in:

```text
data-raw/swisseo_vhi_bern/
```

---

## Project Structure

```text
project/
│
├── analysis/
│   └── download_meteoswiss_data.R
│
├── R/
│   ├── 01_calc_pcwd.R
│   ├── 02_swissEOVHI.R
│   └── calc_pet.R
│
├── data/
│   ├── Bern_pcwd_daily.csv
│   ├── swisseo_vhi_bern_plot_data.csv
│   ├── swisseo_vhi_bern_stats.csv
│   └── swisseo_vhi_bern_crop_YYYY-MM-DD.tif
│
├── data-raw/
│
├── vignettes/
│   └── drought_2026.qmd
│
└── README.md
```

### Workflow components

#### 1. Download meteorological observations

```r
analysis/download_meteoswiss_data.R
```

Downloads daily MeteoSwiss station observations for Bern and stores them in `data-raw/`.

#### 2. Calculate climatic water deficit metrics

```r
R/01_calc_pcwd.R
```

This script:

* reads MeteoSwiss observations
* calculates daily potential evapotranspiration (PET)
* calculates:

  * PCWD (Potential Climatic Water Deficit)
  * CWD (Climatic Water Deficit)
* exports processed data to:

```text
data/Bern_pcwd_daily.csv
```

#### 3. Vegetation health assessment

```r
R/02_swissEOVHI.R
```

This script:

* downloads the latest available swissEO VHI product
* crops the raster around Bern
* converts the raster into a plot-ready data frame
* calculates summary statistics
* exports processed data to:

```text
data/swisseo_vhi_bern_plot_data.csv
data/swisseo_vhi_bern_stats.csv
data/swisseo_vhi_bern_crop_YYYY-MM-DD.tif
```

#### 4. Blog generation

```text
vignettes/drought_2026.qmd
```

The Quarto document loads the processed datasets from `data-dynamic/` and generates all figures during rendering.

This includes:

* PCWD seasonal cycle figure
* CWD seasonal cycle figure
* swissEO Vegetation Health Index map

The document also incorporates summary statistics directly into the text and provides the written drought assessment.


---

## How to Reproduce the Analysis

With below code the blog can be updated:
```
cd ~/GitHub/geco-bern/drought_switzerland_blog
quarto publish vignettes/drought_2026.qmd

# alternatively do it from R:
# renv::restore()
# quarto::quarto_render("vignettes/drought_2026.qmd")
# quarto::quarto_publish_site("vignettes/drought_2026.qmd")
```

To render the article locally without updating the blog:

```r
renv::restore()
quarto::quarto_render("vignettes/drought_2026.qmd")
```

The Quarto document automatically generates all figures from the processed datasets stored in `data-dynamic/`.


---

## Results

### Processed data

```text
data-dynamic/
```

Examples:

* Bern_pcwd_daily.csv
* swisseo_vhi_bern_plot_data.csv
* swisseo_vhi_bern_stats.csv
* swisseo_vhi_bern_crop_YYYY-MM-DD.tif

### Blog output

Rendered Quarto outputs are written to:

```text
vignettes/
```

All figures are generated dynamically during rendering from the processed datasets stored in `data-dynamic/`.

Outputs may be available as:

* HTML
* PDF

---

## Dependencies

### R packages

```r
here
readr
dplyr
ggplot2
lubridate
purrr
SPEI
httr2
jsonlite
terra
sf
patchwork
scales
quarto
```

### Environment management

```r
renv::init()
renv::snapshot()
```

Include the generated `renv.lock` file in the repository.

### System dependencies

* GDAL
* PROJ
* GEOS

Required by:

```r
sf
terra
```

A recent installation of Quarto is required for rendering the drought blog.

