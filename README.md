# Updating the Drought Blog

This repository contains the workflow used to generate the Swiss drought monitoring blog and its associated figures.

## Update Workflow

### 1. Download the latest MeteoSwiss data

Run:

```r
analysis/download_meteoswiss_data.R
```

This script downloads and updates the meteorological datasets used throughout the analysis.

### 2. Recalculate the climatic water deficit metrics

Run:

```r
R/01_calc_pcwd.R
```

This script computes the precipitation minus potential evapotranspiration metrics and updates the corresponding figures.

### 3. Update the swissEO vegetation health analysis

Run:

```r
R/02_swissEOVHI.R
```

This script downloads the latest available swissEO Vegetation Health Index (VHI) data, processes the data for the Bern region, and generates updated vegetation stress figures.

### 4. Review and update the blog content

Open:

```text
vignettes/drought_2026.qmd
```

The newly generated figures are automatically referenced in the document. Review all updated plots and adjust the text and interpretation where necessary to reflect current drought conditions.

### 5. Render the blog

Render the Quarto document:

```bash
quarto render vignettes/drought_2026.qmd
```

or from R:

```r
quarto::quarto_render("vignettes/drought_2026.qmd")
```

The rendered blog will be written to the `vignettes/` folder.

## Typical pipeline

For a regular drought update:

1. Run `analysis/download_meteoswiss_data.R`
2. Run `R/01_calc_pcwd.R`
3. Run `R/02_swissEOVHI.R`
4. Review figures in `vignettes/drought_2026.qmd`
5. Update the written interpretation
6. Render the Quarto document
7. Commit and push the updated figures and blog post

