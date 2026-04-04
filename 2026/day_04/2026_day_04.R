
## Challenge: #30DayChartChallenge 2026 — Day 04
## Prompt:    Comparisons | Slope
##
## Title:     Whose Grid Got Cleaner?
## Data:      Our World in Data — Energy Data (OWID)
##            
## Author:    Steven Ponce
## Date:      2026-04-04

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data:      Our World in Data — Electricity Mix (Ember / Energy Institute, 2023)
##            https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,     
  janitor, scales, glue, ggrepel           
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 6,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
energy_raw <- read_csv("2026/data/owid-energy-data.csv")


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)


## 4. TIDY DATA ----

### |- define curated country list ----
countries_keep <- c(
  "United Kingdom", "Germany", "Estonia", "Poland",
  "South Africa", "Australia", "United States", "Canada",
  "France", "Sweden", "Norway", "Brazil",
  "India", "China", "Vietnam", "Indonesia",
  "Chile", "Mexico", "Japan", "South Korea"
)

### |- filter to 2020 and 2024 ----
slope_data <- energy_raw |>
  filter(
    country %in% countries_keep,
    year %in% c(2020, 2024),
    !is.na(carbon_intensity_elec)
  ) |>
  select(country, year, carbon_intensity_elec) |>
  group_by(country) |>
  filter(n() == 2) |>
  ungroup()

### |- compute delta ----
country_delta <- slope_data |>
  pivot_wider(
    names_from   = year,
    values_from  = carbon_intensity_elec,
    names_prefix = "yr_"
  ) |>
  mutate(
    delta     = yr_2024 - yr_2020,
    delta_pct = (yr_2024 - yr_2020) / yr_2020 * 100
  ) |>
  arrange(delta)

### |- identify top 3 improvers (largest absolute drop) ----
top_improvers <- country_delta |>
  slice_min(order_by = delta, n = 3) |>
  pull(country)

### |- join highlight flag back to slope data ----
slope_data <- slope_data |>
  left_join(
    country_delta |> select(country, delta, delta_pct),
    by = "country"
  ) |>
  mutate(
    highlight = country %in% top_improvers
  )

### |- endpoint data for geom_point ----
endpoints <- slope_data |>
  filter(year %in% c(2020, 2024))

### |- pre-build label data frames ----
labels_highlighted <- slope_data |> filter(highlight, year == 2024)
labels_other <- slope_data |> filter(!highlight, year == 2024)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "TRUE"  = "#007F5F",
    "FALSE" = "gray80"
  )
)

col_ref_line <- "#457B9D"
col_bg       <- colors$background

### |- titles and caption ----
title_text <- str_glue("Whose Grid Got Cleaner?")

subtitle_text <- str_glue(
  "Carbon intensity of electricity generation (gCO\u2082/kWh), <b>2020 \u2192 2024</b>.<br>",
  "Highlighted lines = the <b>3 largest absolute declines</b> in carbon intensity.<br>",
  "Lower values mean cleaner electricity."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 04,
  source_text = "Our World in Data \u2014 Energy Dataset"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Canvas
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    #
    # Grid
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Axes
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      size = 11, face = "bold", color = "gray30",
      family = fonts$text, margin = margin(t = 6)
    ),
    axis.text.y = element_text(size = 9, color = "gray50", family = fonts$text),
    axis.title = element_blank(),

    # Text hierarchy
    plot.title = element_text(
      size = 25, face = "bold", family = fonts$title,
      color = "gray10", margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 10, family = "sans", color = "gray35",
      lineheight = 1.4, margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      size = 7, color = "gray55", family = fonts$caption,
      hjust = 0, margin = margin(t = 16), lineheight = 1.3
    ),
    plot.margin = margin(t = 24, r = 25, b = 16, l = 24)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <-
  slope_data |>
  # Geoms
  ggplot(aes(
    x = year,
    y = carbon_intensity_elec,
    group = country,
    color = as.character(highlight)
  )) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = col_ref_line,
    linewidth  = 0.5,
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = 2019,
    y = 110,
    label = "~100 gCO\u2082/kWh clean grid benchmark",
    color = col_ref_line,
    size = 2.6,
    fontface = "italic",
    family = fonts$text,
    hjust = 0,
    vjust = 0
  ) +
  geom_line(
    data = slope_data |> filter(!highlight),
    linewidth = 0.45,
    alpha = 0.50
  ) +
  geom_line(
    data = slope_data |> filter(highlight),
    linewidth = 1.55,
    alpha = 0.98,
    color = colors$palette["TRUE"]
  ) +
  geom_point(
    data = endpoints |> filter(!highlight),
    size  = 1.5,
    alpha = 0.45
  ) +
  geom_point(
    data = endpoints |> filter(highlight),
    size  = 3.2,
    alpha = 0.98,
    color = colors$palette["TRUE"]
  ) +
  geom_text_repel(
    data = labels_highlighted,
    aes(label = glue("{country} (-{round(abs(delta), 0)} g)")),
    hjust = 0,
    nudge_x = 0.55,
    direction = "y",
    segment.size = 0.3,
    segment.color = "gray70",
    size = 3.5,
    lineheight = 1.15,
    family = fonts$text,
    fontface = "bold",
    color = colors$palette["TRUE"],
    box.padding = 0.30,
    point.padding = 0.20,
    min.segment.length = 0
  ) +
  geom_text_repel(
    data = labels_other,
    aes(label = country),
    hjust = 0,
    nudge_x = 0.45,
    direction = "y",
    segment.size = 0.12,
    segment.color = "gray88",
    size = 1.8,
    color = "gray70",
    family = fonts$text,
    box.padding = 0.07,
    point.padding = 0.05,
    min.segment.length = 0,
    force = 0.8,
    seed = 42
  ) +

  # Scales
  scale_color_manual(
    values = c("TRUE" = colors$palette["TRUE"], "FALSE" = colors$palette["FALSE"])
  ) +
  scale_x_continuous(
    breaks = c(2020, 2024),
    labels = c("2020", "2024"),
    limits = c(2019, 2026),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " g"),
    breaks = seq(0, 800, by = 200),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL
  ) +
  guides(color = "none")

# preview
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
# date     2026-03-20
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────
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
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
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

# ───────────────────────────────────────────────────────────────────────
