
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 20 · Timeseries | Global Change

## Topic:     Three Curves, One Crisis — CO₂, Temperature, Sea Level (1960–2024)
## Author:    Steven Ponce
## Date:      2026-04-20

## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/:
##       fonts.R, social_icons.R, base_theme.R
##       Key calls: get_theme_colors(), setup_fonts(), get_font_families(),
##                  create_base_theme(), extend_weekly_theme(), create_dcc_caption()

## Data Sources:
##   CO₂:         NOAA Global Monitoring Laboratory — Mauna Loa monthly mean
##                https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.csv
##   Temperature: NASA GISS Surface Temperature Analysis (GISTEMP v4) — global annual
##                https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv
##   Sea Level:   CSIRO Church & White tide gauge (1880–2013) spliced with
##                NOAA Laboratory for Satellite Altimetry (1993–present); both rebased to 1960 = 0 mm
##                CSIRO mirror: https://raw.githubusercontent.com/datasets/sea-level-rise/main/data/epa-sea-level.csv
##                NOAA LSA:     https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_ref_90.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, patchwork,
  janitor, scales, glue
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----

### |- CO₂: NOAA Mauna Loa monthly mean ----
co2_raw <- read_csv(
  here::here("2026/data/co2_mauna_loa_raw.csv"),
  show_col_types = FALSE
)

### |- Temperature: NASA GISS global annual anomaly ----
temp_raw <- read_csv(
  here::here("2026/data/nasa_gistemp_global_raw.csv"),
  show_col_types = FALSE
)

### |- Sea Level Part A: CSIRO tide gauge ----
csiro_raw <- read_csv(
  here::here("2026/data/csiro_sea_level_raw.csv"),
  show_col_types = FALSE
)

### |- Sea Level Part B: NOAA LSA satellite altimetry ----
nasa_sea_raw <- read_csv(
  here::here("2026/data/noaa_lsa_sea_level_raw.csv"),
  show_col_types = FALSE
)


## 3. EXAMINING THE DATA ----
glimpse(co2_raw)
glimpse(temp_raw)
glimpse(csiro_raw)
glimpse(nasa_sea_raw)


## 4. TIDY DATA ----

### |- CO₂: annual mean, 1960–2024 ----
co2 <- co2_raw |>
  mutate(
    year    = as.integer(year),
    co2_avg = as.numeric(co2_avg)
  ) |>
  filter(co2_avg > 0) |> # drop -99.99 fill values
  group_by(year) |>
  summarise(co2_ppm = mean(co2_avg, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1960, year <= 2024)

### |- Temperature: annual J-D anomaly, 1960–2024 ----
temp <- temp_raw |>
  clean_names() |>
  select(year, j_d) |> # J-D = Jan–Dec annual mean
  rename(temp_anomaly = j_d) |>
  mutate(
    year         = as.integer(year),
    temp_anomaly = as.numeric(temp_anomaly)
  ) |>
  filter(!is.na(temp_anomaly), year >= 1960, year <= 2024)

### |- Sea Level: splice CSIRO + NASA, rebase to 1960 = 0 mm ----
csiro <- csiro_raw |>
  clean_names() |>
  select(year, csiro_adjusted_sea_level) |>
  rename(sea_mm = csiro_adjusted_sea_level) |>
  mutate(
    year   = as.integer(year),
    sea_mm = as.numeric(sea_mm) * 25.4 # inches → mm
  ) |>
  filter(year >= 1960, year <= 1992)

# NOAA LSA structure: year (decimal) + one column per satellite mission.
nasa_sea <- nasa_sea_raw |>
  clean_names() |>
  pivot_longer(
    cols      = -year,
    names_to  = "mission",
    values_to = "sea_mm"
  ) |>
  filter(!is.na(sea_mm)) |>
  mutate(
    year_int = floor(as.numeric(year)),
    sea_mm   = as.numeric(sea_mm)
  ) |>
  filter(year_int >= 1993, year_int <= 2024) |>
  group_by(year = year_int) |>
  summarise(sea_mm = mean(sea_mm, na.rm = TRUE), .groups = "drop")

# Offset NASA to be continuous with CSIRO at the 1992/1993 boundary
csiro_1992 <- csiro |>
  filter(year == 1992) |>
  pull(sea_mm)
nasa_1993 <- nasa_sea |>
  filter(year == 1993) |>
  pull(sea_mm)
nasa_offset <- csiro_1992 - nasa_1993

sea_spliced <- bind_rows(
  csiro,
  nasa_sea |> mutate(sea_mm = sea_mm + nasa_offset)
)

# Rebase: 1960 value → 0 mm
sea_base_1960 <- sea_spliced |>
  filter(year == 1960) |>
  pull(sea_mm)

sea <- sea_spliced |>
  mutate(
    year   = as.integer(year),
    sea_mm = sea_mm - sea_base_1960
  ) |>
  filter(year >= 1960, year <= 2024)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    accent    = "#6B3FA0",  
    bg        = "#F7F7F5",
    text_main = "#1A1A1A",
    text_mute = "#4A4A4A",       
    grid      = "#EBEBEB",     
    zero_line = "#C4C4C4"
  )
)

### |- titles and caption ----
title_text    <- "Three Signals, One Direction"

subtitle_text <- "Independent indicators of planetary change move in parallel since 1960."

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 20,
  source_text = "NOAA GML (CO₂) · NASA GISS (Temperature) · CSIRO Church & White + NOAA LSA (Sea Level)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- shared panel theme ----
base_theme <- create_base_theme(colors)

panel_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(
      family = fonts$text, size = 10, color = colors$palette$text_mute 
    ),
    axis.ticks = element_blank(),
    axis.line.x = element_line(color = "#CECECE", linewidth = 0.3),
    panel.grid.major.y = element_line(color = colors$palette$grid, linewidth = 0.18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = colors$palette$bg, color = NA),
    plot.background = element_rect(fill = colors$palette$bg, color = NA),
    plot.margin = margin(4, 16, 4, 16)
  )
)

theme_set(panel_theme)

### |- Panel 1: CO₂ ----
p_co2 <- co2 |>
  ggplot(aes(x = year, y = co2_ppm)) +
  geom_line(color = colors$palette$accent, linewidth = 0.62, alpha = 0.9) +
  geom_hline(
    yintercept = 400, linetype = "22",
    color = colors$palette$text_mute, linewidth = 0.28
  ) +
  annotate(
    "text",
    x = 1963, y = 404,
    label = "400 ppm threshold — first crossed 2013",
    family = fonts$text, size = 2.55,
    color = colors$palette$text_mute, hjust = 0
  ) +
  scale_x_continuous(
    limits = c(1960, 2024), breaks = seq(1960, 2020, 20),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(310, NA),
    labels = label_number(suffix = " ppm"),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(title = "CO₂ concentration (ppm)") +
  theme(
    plot.title = element_text(
      family = fonts$text, size = 11, face = "bold",
      color = colors$palette$text_main, margin = margin(b = 6)
    )
  )

### |- Panel 2: Temperature anomaly ----
p_temp <- temp |>
  ggplot(aes(x = year, y = temp_anomaly)) +
  geom_ribbon(
    aes(ymin = pmin(temp_anomaly, 0), ymax = temp_anomaly),
    fill = colors$palette$accent, alpha = 0.12
  ) +
  geom_line(color = colors$palette$accent, linewidth = 0.62, alpha = 0.9) +
  geom_hline(yintercept = 0, color = colors$palette$zero_line, linewidth = 0.3) +
  geom_hline(
    yintercept = 1.0, linetype = "22",
    color = colors$palette$text_mute, linewidth = 0.28
  ) +
  annotate(
    "text",
    x = 1963, y = 1.045,
    label = "+1 °C anomaly — first reached 2016",
    family = fonts$text, size = 2.55,
    color = colors$palette$text_mute, hjust = 0
  ) +
  scale_x_continuous(
    limits = c(1960, 2024), breaks = seq(1960, 2020, 20),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " °C"),
    expand = expansion(mult = c(0.05, 0.14))
  ) +
  labs(title = "Global temperature anomaly (°C, relative to 1951–1980)") +
  theme(
    plot.title = element_text(
      family = fonts$text, size = 11, face = "bold",
      color = colors$palette$text_main, margin = margin(b = 6)
    )
  )

### |- Panel 3: Sea level ----
sea_end_val <- sea |> filter(year == max(year)) |> pull(sea_mm) |> round(0)

p_sea <- sea |>
  ggplot(aes(x = year, y = sea_mm)) +
  geom_ribbon(
    aes(ymin = 0, ymax = sea_mm),
    fill = colors$palette$accent, alpha = 0.10
  ) +
  geom_line(color = colors$palette$accent, linewidth = 0.62, alpha = 0.9) +
  geom_hline(yintercept = 0, color = colors$palette$zero_line, linewidth = 0.3) +
  annotate(
    "text",
    x = 1963, y = sea_end_val * 0.96,
    label = glue("+{sea_end_val} mm ({round(sea_end_val / 10, 0)} cm) rise since 1960"),
    family = fonts$text, size = 2.55,
    color = colors$palette$text_mute, hjust = 0
  ) +
  scale_x_continuous(
    limits = c(1960, 2024), breaks = seq(1960, 2020, 20),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " mm"),
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  labs(title = "Global mean sea level (mm, baseline: 1960)") +
  theme(
    plot.title = element_text(
      family = fonts$text, size = 11, face = "bold",
      color = colors$palette$text_main, margin = margin(b = 6)
    )
  )

### |- Combined plots ----
p_combined <- p_co2 / p_temp / p_sea +
  plot_layout(heights = c(1, 1, 1), axes = "collect_x") +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        family = fonts$title, face = "bold", size = 24,
        color = colors$palette$text_main, margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        family = fonts$text, size = 11,
        color = colors$palette$text_mute,
        lineheight = 1.4, margin = margin(b = 20)
      ),
      plot.caption = element_markdown(
        family = fonts$text, size = 7,
        color = colors$palette$text_mute, ,
        lineheight = 1.3,
        hjust = 1, margin = margin(t = 14)
      ),
      plot.background = element_rect(fill = colors$palette$bg, color = NA),
      plot.margin = margin(24, 28, 14, 28)
    )
  )

### |- preview ----
snap(p_combined)


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

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-27
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpuI0X6e/file86785de212e8". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
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
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────
