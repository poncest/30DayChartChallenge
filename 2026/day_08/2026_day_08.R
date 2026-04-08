
## Challenge: #30DayChartChallenge 2026 
## Prompt:    Day 08 | Distributions | Circular
## Topic:     The Flu Season Clock

## Author:    Steven Ponce
## Date:      2026-04-08

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data:     CDC FluView — ILINet (Influenza-like Illness Surveillance)
##           Primary:  CMU Delphi Epidata API (httr2 + jsonlite, no package needed)
##                     https://api.delphi.cmu.edu/epidata/fluview/
##           Cached:   2026/data/fluview_national_ilinet.csv (auto-saved on first run)
##
## NOTE: cdcfluview removed from CRAN 2024-01-15. Use CMU Delphi API instead.

## Visual grammar (distribution-first):
##   Layer 1 (hero)   — historical band: the typical annual distribution
##   Layer 2 (anomaly)— 2020-21 filled shape: the disrupted distribution
##   Layer 3 (guide)  — historical median: faint structural reference


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, patchwork,     
  janitor, scales, glue, httr2, jsonlite       
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 7,
  height = 7,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


### |- helper: fetch one epiweek range from Delphi Epidata ----
# fetch_fluview <- function(epiweek_start, epiweek_end) {
#   resp <- request("https://api.delphi.cmu.edu/epidata/fluview/") |>
#     req_url_query(
#       regions  = "nat",
#       epiweeks = glue("{epiweek_start}-{epiweek_end}")
#     ) |>
#     req_perform()
#   
#   content <- resp |>
#     resp_body_string() |>
#     fromJSON(simplifyDataFrame = TRUE)
#   
#   if (content$result != 1) {
#     stop(glue("Epidata API error: {content$message}"))
#   }
#   
#   content$epidata |>
#     as_tibble() |>
#     clean_names()
# }

### |- cache path ----
# cache_path <- here::here("2026/data/fluview_national_ilinet.csv")

### |- read from cache or fetch from API ----
# if (file.exists(cache_path)) {
#   message("Cache found — reading from local CSV.")
#   flu_raw <- read_csv(cache_path, show_col_types = FALSE)
# } else {
#   message("No cache — fetching from CMU Delphi Epidata API...")
#   flu_raw <- tryCatch(
#     {
#       bind_rows(
#         fetch_fluview(201540, 202039),
#         fetch_fluview(202040, 202439)
#       )
#     },
#     error = function(e) {
#       stop("API fetch failed: ", e$message)
#     }
#   )
#   dir.create(here::here("2026/data"), showWarnings = FALSE, recursive = TRUE)
#   write_csv(flu_raw, cache_path)
#   message("Data cached to: ", cache_path)
# }

## 2. READ IN THE DATA ----
flu_raw <- read_csv(
  here::here("2026/data/fluview_national_ilinet.csv"),
  show_col_types = FALSE
) |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(flu_raw)


## 4. TIDY DATA ----

### |-column parsing ----
if ("epiweek" %in% names(flu_raw)) {
  epi_year <- as.integer(flu_raw$epiweek) %/% 100L
  epi_week <- as.integer(flu_raw$epiweek) %% 100L
} else {
  epi_year <- rep(NA_integer_, nrow(flu_raw))
  epi_week <- rep(NA_integer_, nrow(flu_raw))
}

flu_parsed <- flu_raw |>
  mutate(
    year = if ("year" %in% names(flu_raw)) coalesce(as.integer(.data$year), epi_year) else epi_year,
    week = if ("week" %in% names(flu_raw)) coalesce(as.integer(.data$week), epi_week) else epi_week
  ) |>
  filter(!is.na(wili), !is.na(year), !is.na(week))

flu_tidy <- flu_parsed |>
  mutate(
    season_start = if_else(week >= 40L, year, year - 1L),
    season_label = glue("{season_start}-{str_sub(season_start + 1L, 3, 4)}"),
    week_shifted = ((week - 1L + 13L) %% 52L) + 1L
  ) |>
  filter(season_start >= 2015L, season_start <= 2023L)

### |- classify into layers ----
flu_classified <- flu_tidy |>
  mutate(
    layer = case_when(
      season_start %in% 2015:2019 ~ "historical",
      season_start == 2020L ~ "covid",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(layer))

### |- historical band (10th-90th percentile) ----
historical_band <- flu_classified |>
  filter(layer == "historical") |>
  group_by(week_shifted) |>
  summarise(
    ili_lo     = quantile(wili, 0.10, na.rm = TRUE),
    ili_median = median(wili, na.rm = TRUE),
    ili_hi     = quantile(wili, 0.90, na.rm = TRUE),
    .groups    = "drop"
  )

### |- COVID season ----
covid_line <- flu_classified |>
  filter(layer == "covid") |>
  arrange(week_shifted)

### |- close polygons at the seam ----
historical_closed <- bind_rows(
  historical_band,
  historical_band |> slice(1) |> mutate(week_shifted = 53)
)

covid_closed <- bind_rows(
  covid_line,
  covid_line |> slice(1) |> mutate(week_shifted = 53)
)

### |- historical band as explicit polygon ----
historical_band_poly <- bind_rows(
  historical_closed |> transmute(week_shifted, y = ili_hi),
  historical_closed |> transmute(week_shifted, y = ili_lo) |> arrange(desc(week_shifted))
)

### |- scale helpers ----
y_max      <- ceiling(max(historical_closed$ili_hi, na.rm = TRUE)) + 1
inner_hole <- 0.8    

### |- reference ring data ----
ring_values <- c(1, 2, 3)
ring_values <- ring_values[ring_values < y_max]

ring_data <- expand_grid(
  week_shifted = seq(1, 53, by = 0.25),
  ring_val     = ring_values
)

### |- ring % labels  ----
ring_labels <- tibble(
  x     = 27,                         
  y     = inner_hole + ring_values + 0.08,
  label = paste0(ring_values, "%")
)

### |- month labels ----
month_labels <- tibble(
  x     = c(14, 27, 40,  1),
  y     = inner_hole + y_max * 1.00,
  label = c("Jan", "Oct", "Jul", "Apr")   
)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_bg         = "#07111d",     
    col_band_fill  = "#7b8fa1",
    col_band_line  = "#a8bfcf",
    col_covid_fill = "#8fd3e8",
    col_covid_line = "#c8ecf5",
    col_text       = "#e9eef4",
    col_subtext    = "#a6b7c6",
    col_grid       = "#1f3d5c"
  )
)

### |- titles and caption ----
title_text    <- "The Flu Season Clock"

subtitle_text <- "Flu usually piles up in winter. In 2020\u201321, that seasonal pattern nearly disappeared."

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 08,
  source_text = "CDC FluView ILINet \u00b7 CMU Delphi Epidata API"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

font_body    <- fonts$text    %||% ""
font_title   <- fonts$title   %||% ""
font_caption <- fonts$caption %||% ""

### |- main clock plot ----
p_clock <- ggplot() +

  # Geoms
  geom_line(
    data = ring_data,
    aes(x = week_shifted, y = inner_hole + ring_val, group = ring_val),
    color = colors$palette$col_grid,
    linewidth = 0.4
  ) +
  geom_polygon(
    data  = historical_band_poly,
    aes(x = week_shifted, y = inner_hole + y),
    fill  = colors$palette$col_band_fill,
    alpha = 0.45
  ) +
  geom_line(
    data = historical_closed,
    aes(x = week_shifted, y = inner_hole + ili_median),
    color = colors$palette$col_band_line,
    linewidth = 0.55,
    alpha = 0.6,
    linetype = "dashed"
  ) +
  geom_polygon(
    data  = covid_closed,
    aes(x = week_shifted, y = inner_hole + wili),
    fill  = colors$palette$col_covid_fill,
    alpha = 0.35
  ) +
  geom_line(
    data = covid_closed,
    aes(x = week_shifted, y = inner_hole + wili),
    color = colors$palette$col_covid_line,
    linewidth = 0.7,
    alpha = 0.8
  ) +
  geom_text(
    data = ring_labels,
    aes(x = x, y = y, label = label),
    color = colors$palette$col_grid,
    size = 2.3,
    hjust = 0.5,
    family = font_body
  ) +
  geom_text(
    data = month_labels,
    aes(x = x, y = y, label = label),
    color = colors$palette$col_subtext,
    size = 3.8,
    hjust = 0.5,
    family = font_body
  ) +

  # Annotate
  annotate(
    "label",
    x = 40,
    y = inner_hole + 0.30,
    label  = "2020\u201321 (COVID disruption):\nflu nearly vanished",
    color = colors$palette$col_covid_fill,
    fill = alpha(colors$palette$col_bg, 0.90),
    label.size = 0,
    size  = 2.8,
    hjust  = 0.5,
    lineheight = 1.05,
    family = font_body
  ) +
  annotate(
    "label",
    x   = 14,
    y  = inner_hole + y_max * 0.82,
    label  = "Typical winter peak\n(Dec\u2013Feb)",
    color = colors$palette$col_subtext,
    fill = alpha(colors$palette$col_bg, 0.88),
    label.size = 0,
    size = 2.7,
    hjust  = 0.5,
    family = font_body
  ) +
  coord_polar(start = -pi / 2, direction = -1, clip = "off") +

  # Scales
  scale_x_continuous(limits = c(1, 53), breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, inner_hole + y_max * 1.02), breaks = NULL, expand = c(0, 0)) +

  # Labs
  labs(title = title_text, subtitle = subtitle_text) +

  # Theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = colors$palette$col_bg, color = NA),
    panel.background = element_rect(fill = colors$palette$col_bg, color = NA),
    plot.title = element_text(
      family = font_title,
      face   = "bold",
      color = colors$palette$col_text,
      size  = 20,
      hjust  = 0.5,
      margin = margin(t = 14, b = 4)
    ),
    plot.subtitle = element_text(
      family = font_body,
      color = colors$palette$col_subtext,
      size = 10,
      hjust  = 0.5,
      lineheight = 1.3,
      margin = margin(b = 2)
    ),
    plot.margin = margin(5, 5, 0, 5)
  )

### |- caption + legend panel ----
legend_line <- glue(
  "<span style='color:{colors$palette$col_band_fill};'><b>Historical band</b></span>",
  " (10th\u201390th percentile, 2015\u201316 to 2019\u201320) &nbsp;\u2022&nbsp; ",
  "<span style='color:{colors$palette$col_covid_fill};'><b>2020\u201321 disruption</b></span>"
)

p_caption <- ggplot() +
  annotate(
    "richtext",
    x = 0.5, y = 0.75,
    label = legend_line,
    color = colors$palette$col_subtext,
    fill = NA,
    label.color = NA,
    size = 2.8,
    hjust = 0.5,
    family = font_body
  ) +
  annotate(
    "richtext",
    x = 0.5, y = 0.20,
    label = caption_text,
    color = colors$palette$col_subtext,
    fill = NA,
    label.color = NA,
    size = 2.5,
    hjust = 0.5,
    family = font_caption
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = colors$palette$col_bg, color = NA),
    plot.margin     = margin(0, 10, 8, 10)
  )

### |- combined plots ----
combined_plots <- p_clock / p_caption +
  plot_layout(heights = c(10, 1.1)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = colors$palette$col_bg, color = NA))
  )

### |- preview ----
snap(combined_plots)


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

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-22
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmpwlu9SR/file7c482649dbb". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
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
# httr2        * 1.2.2    2025-12-08 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite     * 2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.3.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.3.3)
# R.oo           1.27.0   2024-11-01 [1] CRAN (R 4.3.3)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.3.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.3.1)
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
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────