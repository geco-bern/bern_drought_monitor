# This script creates publication-ready figures for the Bern drought monitor.

# Load packages ----------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(here)

# Read data --------------------------------------------------------------------
meteo_pcwd <- read_csv(here("data", "Bern_pcwd_daily.csv"))

# Settings ---------------------------------------------------------------------
highlight_colours <- c(
  "2026" = "#D73027",
  "2022" = "#4575B4",
  "2018" = "#E69F00",
  "2023" = "#009E73",
  "1947" = "#CC79A7",
  "1976" = "#56B4E9",
  "2003" = "#F0E442"
)

highlight_years <- as.integer(names(highlight_colours))
month_labels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

general_plot_theme <- list(
  theme_classic(base_size = 13),
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      colour = "grey30",
      margin = margin(b = 10)
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    )
  ),
  theme(
    strip.background = element_blank(),
    panel.spacing = grid::unit(1, "lines"),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    plot.caption = element_text(
      colour = "grey40",
      hjust = 0
    )
  ),
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.justification = "left"
  )
)

dir.create(here("fig"), showWarnings = FALSE, recursive = TRUE)

# Seasonal PCWD based on MeteoSwiss FAO PET ------------------------------------
meteo_pcwd_fao_meteoswiss <- meteo_pcwd |>
  filter(!is.na(PCWD_FAO_MeteoSwiss)) |>
  mutate(plot_date = make_date(2000, month(date), day(date)))

fig_pcwd_fao_meteoswiss <- ggplot() +
  geom_line(
    data = meteo_pcwd_fao_meteoswiss |>
      filter(!year %in% highlight_years),
    aes(x = plot_date, y = PCWD_FAO_MeteoSwiss, group = year),
    colour = "grey75",
    linewidth = 0.4,
    alpha = 0.5
  ) +
  geom_line(
    data = meteo_pcwd_fao_meteoswiss |>
      filter(year %in% highlight_years),
    aes(
      x = plot_date,
      y = PCWD_FAO_MeteoSwiss,
      colour = as.factor(year),
      group = year
    ),
    linewidth = 0.5
  ) +
  geom_text(
    data = meteo_pcwd_fao_meteoswiss |>
      filter(year %in% highlight_years) |>
      group_by(year) |>
      filter(PCWD_FAO_MeteoSwiss == max(PCWD_FAO_MeteoSwiss)) |>
      slice_tail(n = 1) |>
      ungroup(),
    aes(
      x = plot_date,
      y = PCWD_FAO_MeteoSwiss,
      colour = as.factor(year),
      label = year
    ),
    vjust = -0.1,
    show.legend = FALSE
  ) +
  geom_text(
    data = meteo_pcwd_fao_meteoswiss |>
      filter(!year %in% highlight_years) |>
      group_by(year) |>
      filter(PCWD_FAO_MeteoSwiss == max(PCWD_FAO_MeteoSwiss)) |>
      slice_tail(n = 1) |>
      ungroup() |>
      arrange(desc(PCWD_FAO_MeteoSwiss)) |>
      slice_head(n = 5),
    aes(x = plot_date, y = PCWD_FAO_MeteoSwiss, label = year),
    vjust = -0.1,
    colour = "grey55",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = highlight_colours,
    name = "Year"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    labels = \(x) month_labels[lubridate::month(x)]
  ) +
  labs(
    x = NULL,
    y = "PCWD (mm)",
    title = "Potential cumulative water deficit",
    subtitle = paste0(
      "Bern-Zollikofen, MeteoSwiss FAO PET (",
      min(meteo_pcwd_fao_meteoswiss$year, na.rm = TRUE),
      "-",
      max(meteo_pcwd_fao_meteoswiss$year, na.rm = TRUE),
      ")"
    )
  ) +
  general_plot_theme +
  theme(
    legend.position = "none"
  )

# Save figures -----------------------------------------------------------------
ggsave(
  filename = here("fig", "fig_pcwd_fao_meteoswiss.pdf"),
  plot = fig_pcwd_fao_meteoswiss,
  width = 8,
  height = 5
)
