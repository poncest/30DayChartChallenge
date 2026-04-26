
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 26 | Uncertainties | Trend

## Topic:      We know where we've been. Where we go is a choice.
## Author:     Steven Ponce
## Date:       2026-04-26
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data:
##   Observed CO₂: NOAA Mauna Loa monthly mean — already cached from Day 20
##                 2026/data/co2_mauna_loa_raw.csv
##                 Columns: year, month, decimal_date, co2_avg, co2_deseason,
##                           ndays, std_dev, uncertainty
##                 Source: https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.csv
##
##   Projections:  IPCC AR6 WG1 (2021) — hardcoded scenario bands (no file needed)
##                 Approximate medians + likely ranges (66% CI) from SPM Figure 8
##                 Scenarios: SSP1-2.6 (strong mitigation) · SSP2-4.5 (current policies)
##                            SSP5-8.5 (high emissions)
##                 Values are rounded approximations for charting; not official IPCC data.
##                 Source: https://www.ipcc.ch/report/ar6/wg1/


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
  height = 7,
  units  = "in",
  dpi    = 320
)

source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

### |- observed CO₂: read from local cache ----
co2_raw <- read_csv(
  here::here("2026/data/co2_mauna_loa_raw.csv"),
  show_col_types = FALSE
)


## 3. EXAMINING THE DATA ----
glimpse(co2_raw)

co2_raw |>
  filter(year >= 2022) |>
  tail(12)


## 4. TIDY DATA ----

### |- annual mean CO₂, 1960–2024 ----
co2_obs <- co2_raw |>
  slice(-1) |>                    # drop row 1: duplicated header written into CSV body
  mutate(
    year    = as.integer(year),
    co2_avg = as.numeric(co2_avg)
  ) |>
  filter(co2_avg > 0) |>          # drop fill values (-99.99)
  group_by(year) |>
  summarise(co2_ppm = mean(co2_avg, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1960, year <= 2024)

### |- anchor: actual 2024 observed mean (used to seam projections) ----
co2_2024_actual <- co2_obs |> filter(year == 2024) |> pull(co2_ppm)
co2_1960_actual <- co2_obs |> filter(year == 1960) |> pull(co2_ppm)

### |- IPCC AR6 scenario projections: hardcoded tribble ----
# Approximate medians and 66% likely ranges (ppm) from IPCC AR6 WG1 SPM Fig. 8
# Anchor year 2024 set to 424 ppm (approximate); offset-corrected below
# to match the actual Mauna Loa 2024 annual mean.
# Projections run to 2075 (not 2100) to keep SSP5-8.5 upper bound legible.

proj_raw <- tribble(
  ~year,  ~scenario,    ~median,  ~lo,   ~hi,
  # SSP1-2.6: peaks ~2040, then slowly declines
  2024,   "SSP1-2.6",   424,     424,   424,
  2030,   "SSP1-2.6",   437,     430,   444,
  2040,   "SSP1-2.6",   443,     430,   456,
  2050,   "SSP1-2.6",   438,     418,   456,
  2060,   "SSP1-2.6",   428,     403,   452,
  2075,   "SSP1-2.6",   415,     385,   446,
  # SSP2-4.5: continues rising, moderates post-2060
  2024,   "SSP2-4.5",   424,     424,   424,
  2030,   "SSP2-4.5",   444,     436,   452,
  2040,   "SSP2-4.5",   467,     454,   480,
  2050,   "SSP2-4.5",   487,     469,   506,
  2060,   "SSP2-4.5",   504,     481,   528,
  2075,   "SSP2-4.5",   521,     492,   551,
  # SSP5-8.5: rapid continued rise
  2024,   "SSP5-8.5",   424,     424,   424,
  2030,   "SSP5-8.5",   453,     443,   463,
  2040,   "SSP5-8.5",   497,     480,   515,
  2050,   "SSP5-8.5",   553,     527,   580,
  2060,   "SSP5-8.5",   624,     588,   662,
  2075,   "SSP5-8.5",   736,     688,   786
) |>
  mutate(
    scenario = factor(scenario, levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"))
  )

### |- anchor correction ----
# Shift all projection values so the 2024 seam matches observed Mauna Loa mean
proj_offset <- co2_2024_actual - 424

proj <- proj_raw |>
  mutate(
    median = median + proj_offset,
    lo     = lo     + proj_offset,
    hi     = hi     + proj_offset
  )

### |- label positions at 2075 ----
proj_labels <- proj |>
  filter(year == 2075) |>
  select(scenario, median)

### |- key annotation values ----
co2_1960_label <- round(co2_1960_actual, 1)
co2_2024_label <- round(co2_2024_actual, 1)
ppm_rise       <- round(co2_2024_actual - co2_1960_actual)

### |- y positions for seam segment and seam label ----
seam_y_lo    <- 308
seam_y_hi    <- 790
seam_label_y <- 315

### |- label x position (right of 2075, inside clip = "off") ----
label_x <- 2077


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
      "bg"         = "#F5F3EE",
      "observed"   = "#8B3030",
      "ssp126"     = "#4A7C59",
      "ssp245"     = "#C28A2C",
      "ssp585"     = "#7A3020",
      "seam"       = "#6B6B6B",
      "annotation" = "#3D3D3D",
      "muted"      = "#8A8A8A",
      "grid"       = "#E4E1DB"
    )
  )
  
col_bg         <- colors$palette$bg
col_obs        <- colors$palette$observed
col_ssp126     <- colors$palette$ssp126
col_ssp245     <- colors$palette$ssp245
col_ssp585     <- colors$palette$ssp585
col_seam       <- colors$palette$seam
col_annotation <- colors$palette$annotation
col_muted      <- colors$palette$muted
col_grid       <- colors$palette$grid

### |- titles and caption ----
title_text <- "We know where we've been. Where we go is a choice."

subtitle_text <- paste0(
  "Atmospheric CO\u2082 at Mauna Loa rose from <b>", co2_1960_label, " ppm</b> (1960) ",
  "to <b>", co2_2024_label, " ppm</b> (2024) — an increase of <b>",
  ppm_rise, " ppm</b> in 65 years.<br>",
  "Beyond 2024, three IPCC AR6 scenarios show where concentrations could go. ",
  "The gap between them is not prediction error,<br>it is the consequence of different emissions choices."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 26,
  source_text = "NOAA GML Mauna Loa (observed) \u00b7 IPCC AR6 WG1 SPM Fig. 8 (projections, approximate)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.line.x = element_line(color = "#C8C4BC", linewidth = 0.4),
    axis.ticks.x = element_line(color = "#C8C4BC", linewidth = 0.3),
    axis.ticks.y = element_blank(),
    axis.text = element_text(family = fonts$text, size = 10, color = col_muted),
    axis.title.y = element_text(
      family = fonts$text, size = 10, color = col_muted,
      margin = margin(r = 8)
    ),
    axis.title.x = element_blank(),
    plot.title = element_markdown(
      family     = fonts$title,
      size       = 24,
      face       = "bold",
      color      = col_annotation,
      lineheight = 1.1,
      margin     = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      family     = fonts$text,
      size       = 11,
      color      = col_annotation,
      lineheight = 1.5,
      margin     = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = fonts$text,
      size   = 8,
      color  = col_muted,
      hjust  = 1,
      margin = margin(t = 12)
    ),
    plot.margin = margin(t = 20, r = 75, b = 15, l = 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot() +

  # Geoms
  geom_ribbon(
    data = proj |> filter(scenario == "SSP5-8.5"),
    aes(x = year, ymin = lo, ymax = hi),
    fill = col_ssp585,
    alpha = 0.14
  ) +
  geom_ribbon(
    data = proj |> filter(scenario == "SSP5-8.5", year >= 2050),
    aes(x = year, ymin = lo, ymax = hi),
    fill = col_ssp585,
    alpha = 0.10
  ) +
  geom_line(
    data = proj |> filter(scenario == "SSP5-8.5"),
    aes(x = year, y = median),
    color = col_ssp585,
    linewidth = 0.65,
    linetype = "dashed"
  ) +
  geom_ribbon(
    data = proj |> filter(scenario == "SSP2-4.5"),
    aes(x = year, ymin = lo, ymax = hi),
    fill = col_ssp245,
    alpha = 0.18
  ) +
  geom_ribbon(
    data = proj |> filter(scenario == "SSP2-4.5", year >= 2050),
    aes(x = year, ymin = lo, ymax = hi),
    fill = col_ssp245,
    alpha = 0.12
  ) +
  geom_line(
    data = proj |> filter(scenario == "SSP2-4.5"),
    aes(x = year, y = median),
    color = col_ssp245,
    linewidth = 0.65,
    linetype = "dashed"
  ) +
  geom_ribbon(
    data = proj |> filter(scenario == "SSP1-2.6"),
    aes(x = year, ymin = lo, ymax = hi),
    fill = col_ssp126,
    alpha = 0.20
  ) +
  geom_ribbon(
    data = proj |> filter(scenario == "SSP1-2.6", year >= 2050),
    aes(x = year, ymin = lo, ymax = hi),
    fill = col_ssp126,
    alpha = 0.12
  ) +
  geom_line(
    data = proj |> filter(scenario == "SSP1-2.6"),
    aes(x = year, y = median),
    color = col_ssp126,
    linewidth = 0.65,
    linetype = "dashed"
  ) +

  # Annotate
  annotate(
    "segment",
    x = 2024, xend = 2024,
    y = seam_y_lo, yend = seam_y_hi,
    color = "#4A4A4A",
    linewidth = 0.55,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 2024,
    y = seam_label_y,
    label = "Last observed (2024)",
    hjust = 0.5,
    vjust = 1,
    size = 2.9,
    color = "#4A4A4A",
    family = fonts$text,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2024,
    y = seam_label_y - 12,
    label  = "Observed \u2192 Scenarios",
    hjust = 0.5,
    vjust = 1,
    size = 2.6,
    color = col_muted,
    family = "sans"
  ) +
  geom_line(
    data = co2_obs,
    aes(x = year, y = co2_ppm),
    color = col_obs,
    linewidth = 1.1,
    lineend = "round"
  ) +
  annotate(
    "text",
    x = 1963,
    y = co2_1960_actual + 10,
    label = paste0(co2_1960_label, " ppm\n(1960)"),
    hjust = 0,
    vjust = 0,
    size = 3.1,
    color = col_annotation,
    family = fonts$text,
    lineheight = 1.3
  ) +
  annotate(
    "text",
    x = 2019,
    y = co2_2024_actual + 12,
    label = paste0(co2_2024_label, " ppm\n(2024)"),
    hjust = 1,
    vjust = 0,
    size = 3.1,
    color = col_annotation,
    family = fonts$text,
    lineheight = 1.3
  ) +
  annotate(
    "text",
    x = label_x,
    y = proj_labels |> filter(scenario == "SSP5-8.5") |> pull(median),
    label = "SSP5-8.5\nNo additional\naction",
    hjust = 0, vjust = 0.5,
    size = 2.85,
    color = col_ssp585,
    family = fonts$text,
    lineheight = 1.3
  ) +
  annotate(
    "text",
    x = label_x,
    y = proj_labels |> filter(scenario == "SSP2-4.5") |> pull(median),
    label = "SSP2-4.5\nPolicies as\nimplemented",
    hjust = 0, vjust = 0.5,
    size = 2.85,
    color = col_ssp245,
    family = fonts$text,
    lineheight = 1.3
  ) +
  annotate(
    "text",
    x = label_x,
    y = proj_labels |> filter(scenario == "SSP1-2.6") |> pull(median),
    label = "SSP1-2.6\nAggressive\ndecarbonization",
    hjust = 0, vjust = 0.5,
    size = 2.85,
    color = col_ssp126,
    family = fonts$text,
    lineheight = 1.3
  ) +

  # Scales
  scale_x_continuous(
    breaks = c(1960, 1980, 2000, 2024, 2050, 2075),
    labels = c("1960", "1980", "2000", "2024", "2050", "2075"),
    expand = expansion(mult = c(0.02, 0.0))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " ppm"),
    breaks = seq(300, 800, by = 100),
    limits = c(300, 800),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    y = "Atmospheric CO\u2082 (ppm)"
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

# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-29
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────
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
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
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
# R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.3.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.3.3)
# R.oo           1.27.0   2024-11-01 [1] CRAN (R 4.3.3)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.3.3)
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
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.3.1)
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
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.3.1)
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
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────