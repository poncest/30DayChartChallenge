
## Challenge: #30DayChartChallenge 2026
## Prompt:    Day 23 · Timeseries | Seasons

## Topic:     The Planet Still Breathes — Keeling Curve Seasonal Waveform
## Author:    Steven Ponce
## Date:      2026-04-23

## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/ — see project structure.
##
##       Data: NOAA GML Mauna Loa CO₂ Monthly Means, 1958–present
##       URL:  https://gml.noaa.gov/aftp/ccg/co2/trends/co2_mm_mlo.csv
##


## 1. LOAD PACKAGES & SETUP ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,      
  scales, glue, camcorder     
)

camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

energy_raw <- read_csv(here("2026/data/owid-energy-data.csv"))
### |- read and parse NOAA MLO monthly CO2 ----
# Comment lines start with #; no formal header row
# Missing values coded as -99.99

co2_raw <- read_csv(
  here::here("2026/data/co2_mm_mlo.csv"),
  comment    = "#",
  col_names  = c("year", "month", "decimal_date", "average",
                 "deseasonalized", "ndays", "sdev", "unc"),
  col_types  = cols(.default = col_double())
) |>
  filter(!is.na(year))    # drop the header row that slips past comment="#"


## 3. EXAMINING THE DATA ----
glimpse(co2_raw)
range(co2_raw$year)
sum(co2_raw$average < 0)          # count missing (-99.99 sentinel)


## 4. TIDY DATA ----

### |- clean and derive seasonal anomaly ----
co2_clean <- co2_raw |>
  filter(average > 0, deseasonalized > 0) |>
  mutate(
    seasonal_anomaly = average - deseasonalized,
    decade = floor(year / 10) * 10
  ) |>
  filter(decade >= 1960)

### |- decade-averaged seasonal waveform ----
# Keep 4 key decades: 1960s (baseline), 1980s, 2000s, 2020s (now)
key_decades <- c(1960, 1980, 2000, 2020)

co2_waveform_raw <- co2_clean |>
  filter(decade %in% key_decades) |>
  group_by(decade, month) |>
  summarise(
    mean_anomaly = mean(seasonal_anomaly, na.rm = TRUE),
    .groups = "drop"
  )

# Spline-interpolate each decade curve to 120 points (10 per month interval)
# This converts the angular 12-point polygon into a smooth breathing waveform
co2_waveform <- co2_waveform_raw |>
  group_by(decade) |>
  group_modify(~ {
    spl <- splinefun(.x$month, .x$mean_anomaly, method = "periodic")
    month_seq <- seq(1, 12, length.out = 120)
    tibble(month = month_seq, mean_anomaly = spl(month_seq))
  }) |>
  ungroup() |>
  mutate(
    decade_fct = factor(decade, levels = key_decades),
    month_abb  = month.abb[round(month)]
  )

### |- annotation data: decade mean CO2 for annotation text ----
decade_means <- co2_clean |>
  group_by(decade) |>
  summarise(mean_co2 = mean(average, na.rm = TRUE), .groups = "drop")

# values for annotation:
co2_1960s <- decade_means |>
  filter(decade == 1960) |> pull(mean_co2) |> round(0)

co2_2020s <- decade_means |> 
  filter(decade == 2020) |> pull(mean_co2) |> round(0)

### |- x-axis month labels ----
month_labels <- month.abb   

### |- direct end-line labels — 1960s and 2020s only ----
label_data <- co2_waveform |>
  group_by(decade) |>
  slice_max(month, n = 1) |>
  ungroup() |>
  filter(decade %in% c(1960, 2020)) |>
  mutate(label = glue("{decade}s"))

### |- zero reference line data ----
zero_line <- tibble(month = 1:12, y = 0)

### |- split data: context (faded) vs hero (2020s) ----
co2_context <- co2_waveform |> filter(decade != 2020)
co2_hero    <- co2_waveform |> filter(decade == 2020)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "1960" = "#C2C2C2",    
    "1980" = "#9E9E9E",   
    "2000" = "#707070",   
    "2020" = "#6B0F1A",   
    "bg"   = "#F5F2EE",   
    "text" = "#2C2C2C",   
    "grid" = "#DDD9D4"    
  )
)

# extract as plain scalars 
decade_colors <- c(
  "1960" = colors$palette[["1960"]],
  "1980" = colors$palette[["1980"]],
  "2000" = colors$palette[["2000"]],
  "2020" = colors$palette[["2020"]]
)

# line widths
decade_linewidths <- c(
  "1960" = 0.7,
  "1980" = 0.7,
  "2000" = 0.7,
  "2020" = 1.5
)

col_bg   <- colors$palette[["bg"]]
col_text <- colors$palette[["text"]]
col_grid <- colors$palette[["grid"]]
col_2020 <- colors$palette[["2020"]]   

### |- titles and caption ----
title_text    <- "The Planet Still Breathes"

subtitle_text <- str_glue(
  "Decade-averaged CO₂ seasonal cycle at Mauna Loa, 1960s–2020s.<br>",
  "Each curve shows how atmospheric CO₂ rises and falls across the year — ",
  "driven by Northern Hemisphere plant growth and decay.<br>",
  "**+{co2_2020s - co2_1960s} ppm higher baseline since the 1960s** ",
  "(~{co2_1960s} ppm to ~{co2_2020s} ppm)"
)

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 23,
  source_text = "NOAA Global Monitoring Laboratory · Mauna Loa CO₂ monthly means"
)

### |- x-axis month labels ----
month_labels <- month.abb   # "Jan" … "Dec"

### |- direct end-line labels — 1960s and 2020s only ----
label_data <- co2_waveform |>
  group_by(decade) |>
  slice_max(month, n = 1) |>
  ungroup() |>
  filter(decade %in% c(1960, 2020)) |>
  mutate(label = glue("{decade}s"))

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base and weekly theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # background
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),

    # grid — horizontal reference lines only; vertical absent
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # axes
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      family = fonts$text, size = 9, color = col_text, margin = margin(t = 4)
    ),
    axis.text.y = element_text(
      family = fonts$text, size = 9, color = col_text
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = fonts$text, size = 9.5, color = col_text,
      margin = margin(r = 8), angle = 90
    ),

    # text hierarchy
    plot.title = element_text(
      family = fonts$title, size = 28, face = "bold",
      color = col_text, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 11, color = col_text,
      lineheight = 1.45, margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = "#888888",
      hjust = 0, margin = margin(t = 16)
    ),

    # margins
    plot.margin = margin(t = 20, r = 60, b = 12, l = 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot() +

  # Geoms
  geom_hline(yintercept = 0, color = col_grid, linewidth = 0.6) +
  geom_line(
    data = co2_context,
    aes(x = month, y = mean_anomaly, color = decade_fct, group = decade_fct),
    linewidth = 0.7, lineend = "round", linejoin = "round"
  ) +
  geom_line(
    data = co2_hero,
    aes(x = month, y = mean_anomaly, group = decade_fct),
    color = col_2020,
    linewidth = 1.6, lineend = "round", linejoin = "round"
  ) +
  geom_text(
    data = label_data,
    aes(x = month + 0.15, y = mean_anomaly, label = label, color = decade_fct),
    hjust = 0,
    size = 3.2,
    family = fonts$text,
    fontface = "bold",
    inherit.aes = FALSE
  ) +

  # Annotate
  annotate(
    geom = "richtext",
    x = 6.2,
    y = 3.9,
    label = glue(
      "<b>Same seasonal breath (~6–8 ppm)</b><br>",
      "Amplitude stable across all 4 decades"
    ),
    hjust = 0,
    vjust = 1,
    size = 3.3,
    family = fonts$text,
    color = col_text,
    fill = NA,
    label.color = NA,
    lineheight = 1.4
  ) +

  # Scales
  scale_x_continuous(
    breaks = 1:12,
    labels = month_labels,
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_y_continuous(
    breaks = seq(-4, 4, by = 2),
    labels = function(x) glue("{x} ppm"),
    limits = c(-4.2, 4.2)
  ) +
  scale_color_manual(values = decade_colors) +
  guides(color = "none") +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    y        = "Seasonal anomaly (CO₂ ppm, deviation from trend)",
    color    = NULL
  )
 

### |- preview ----
snap(p)


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #TidyTuesday projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #TidyTuesday attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across 50+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/30DayChartChallenge/tree/main/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/30DayChartChallenge/
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-28
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpcrxRRL/file234749b10d5". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# svglite        2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping    1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools          4.3.1    2023-06-16 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# P utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# vroom          1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────
