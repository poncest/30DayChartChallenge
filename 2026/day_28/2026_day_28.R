
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 28 | Uncertainties | Modeling

## Topic:      One Future Does Not Exist — UN World Population Projections 2024
## Author:     Steven Ponce
## Date:       2026-04-28
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data source:
##   wpp2024 R package — United Nations World Population Prospects 2024 Revision
##   https://github.com/PPgp/wpp2024
##   License: CC BY 3.0 IGO
##   Install: devtools::install_github("PPgp/wpp2024") with options(timeout = 600)
##
## Key dataset used: popproj1dt
##   Columns: name, year, pop (median), pop_80l, pop_80u (80% CI),
##            pop_95l, pop_95u (95% CI), pop_low, pop_high (±½ child variants)
##   Units: thousands of persons


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

# Uncomment to install (large package — ~200MB; set timeout first):
# options(timeout = 600)
# devtools::install_github("PPgp/wpp2024")
#library(wpp2024)

### |- load datasets from wpp2024 package and cache (first run only) ----
# data("popproj1dt")    # annual projections 2024–2100 with uncertainty bands
# data("pop1dt")        # historical annual estimates 1950–2023
#
# pop_hist_cache <- here::here("2026/data/wpp2024_pop_historical.csv")
# pop_proj_cache <- here::here("2026/data/wpp2024_pop_projections.csv")
#
# if (!file.exists(pop_hist_cache)) {
#   write_csv(as_tibble(pop1dt),      pop_hist_cache)
#   write_csv(as_tibble(popproj1dt),  pop_proj_cache)
# }

### |- read from cache ----
pop_hist_raw <- read_csv(
  here::here("2026/data/wpp2024_pop_historical.csv"),
  show_col_types = FALSE
)
pop_proj_raw <- read_csv(
  here::here("2026/data/wpp2024_pop_projections.csv"),
  show_col_types = FALSE
)

## 3. EXAMINING THE DATA ----
glimpse(pop_hist_raw)
glimpse(pop_proj_raw)
pop_hist_raw |> filter(name == "World") |> slice_tail(n = 5)
pop_proj_raw |> filter(name == "World") |> slice_head(n = 5)
pop_proj_raw |> names()


## 4. TIDY DATA ----

### |- historical series (global, 1950–2023) ----
pop_hist <- pop_hist_raw |>
  filter(name == "World") |>
  select(year, pop_total = pop) |>
  mutate(
    pop_total = as.numeric(pop_total),
    pop_bn = pop_total / 1e6, # thousands to billions
    series_type = "historical"
  )

### |- projection series (global, 2024–2100) ----
pop_proj <- pop_proj_raw |>
  filter(name == "World") |>
  select(
    year,
    pop_med = pop,
    pop_80l, pop_80u,
    pop_95l, pop_95u,
    pop_low, pop_high
  ) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(across(c(
    pop_med, pop_80l, pop_80u, pop_95l, pop_95u,
    pop_low, pop_high
  ), \(x) x / 1e6))

### |- anchor point: last historical observation spliced to projection ----
anchor_year <- 2024L
anchor_val <- pop_hist |>
  filter(year == max(year)) |>
  pull(pop_bn)

### |- splice: add the 2023 anchor as first row of projection for clean joins ----
anchor_row <- tibble(
  year = max(pop_hist$year),
  pop_med  = anchor_val,
  pop_80l = anchor_val,
  pop_80u = anchor_val,
  pop_95l = anchor_val,
  pop_95u = anchor_val,
  pop_low = anchor_val,
  pop_high = anchor_val
)

pop_proj <- bind_rows(anchor_row, pop_proj)

### |- end-label data: values at 2100 ----
labels_2100 <- pop_proj |>
  filter(year == 2100) |>
  select(year, pop_med, pop_low, pop_high, pop_95l, pop_95u) |>
  pivot_longer(
    cols = c(pop_med, pop_low, pop_high, pop_95l, pop_95u),
    names_to = "variant",
    values_to = "pop_bn"
  ) |>
  mutate(
    label = case_when(
      variant == "pop_high" ~ glue("{round(pop_bn, 1)}B\nHigh variant"),
      variant == "pop_med" ~ glue("{round(pop_bn, 1)}B\nMedian"),
      variant == "pop_low" ~ glue("{round(pop_bn, 1)}B\nLow variant"),
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(label))

### |- seam annotation position ----
seam_year <- max(pop_hist$year)
seam_pop <- anchor_val


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "bg"          = "#F5F3EE",
    "hero"        = "#8B1A2A",        
    "band_95"     = "#D4A4AC",       
    "band_80"     = "#B85C6A",       
    "variant_lo"  = "#8B1A2A",       
    "variant_hi"  = "#8B1A2A",       
    "hist"        = "#3A3635",        
    "seam"        = "#8B1A2A",        
    "annotation"  = "#5C4033",         
    "grid"        = "#E8E4DC"          
  )
)

### |- pre-extract scalar color values  ----
col_bg         <- colors$palette$bg
col_hero       <- colors$palette$hero
col_band_95    <- colors$palette$band_95
col_band_80    <- colors$palette$band_80
col_hist       <- colors$palette$hist
col_seam       <- colors$palette$seam
col_annotation <- colors$palette$annotation
col_grid       <- colors$palette$grid

### |- titles and caption ----
title_text <- str_glue("One future does not exist")

subtitle_text <- str_glue(
  "UN population projections show how fertility assumptions shape long-run outcomes. ",
  "The **shaded bands**<br>",
  "are not noise — they are the model's uncertainty made visible."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 28,
  source_text = "United Nations, DESA, Population Division · World Population Prospects 2024"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme  <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # canvas
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),

    # grid 
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # axes
    axis.ticks = element_blank(),
    axis.text = element_text(
      family = fonts$text, size = 9,
      color = col_annotation
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = fonts$text, size = 9,
      color = col_annotation, angle = 90,
      margin = margin(r = 6)
    ),

    # text
    plot.title = element_markdown(
      family = fonts$title, size = 22,
      face = "bold", color = "#1A0A0C",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 10,
      color = col_annotation, lineheight = 1.4,
      margin = margin(b = 16)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5,
      color = col_annotation, hjust = 0,
      margin = margin(t = 12)
    ),
    plot.margin = margin(t = 20, r = 70, b = 12, l = 16),
    legend.position = "none"
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot() +
  
  # Geoms
  geom_ribbon(
    data = pop_proj,
    aes(x = year, ymin = pop_95l, ymax = pop_95u),
    fill = col_band_95,
    alpha = 0.18
  ) +
  geom_ribbon(
    data = pop_proj,
    aes(x = year, ymin = pop_80l, ymax = pop_80u),
    fill = col_band_80,
    alpha = 0.38
  ) +
  geom_line(
    data = pop_proj,
    aes(x = year, y = pop_high),
    color = col_hero,
    linetype  = "22",
    linewidth = 0.5,
    alpha = 0.65
  ) +
  geom_line(
    data = pop_proj,
    aes(x = year, y = pop_low),
    color = col_hero,
    linetype = "22",
    linewidth = 0.5,
    alpha = 0.65
  ) +
  geom_line(
    data = pop_proj,
    aes(x = year, y = pop_med),
    color = col_hero,
    linewidth = 1.3
  ) +
  geom_line(
    data = pop_hist,
    aes(x = year, y = pop_bn),
    color     = col_hist,
    linewidth = 0.85
  ) +
  geom_vline(
    xintercept = seam_year,
    color = col_seam,
    linetype = "dashed",
    linewidth  = 0.45,
    alpha = 0.7
  ) +
  geom_point(
    data = tibble(year = seam_year, pop_bn = seam_pop),
    aes(x = year, y = pop_bn),
    color = col_seam,
    fill = col_bg,
    shape = 21,
    size = 2.8,
    stroke = 1.2
  ) +
  
  # Annote
  annotate(
    "text",
    x = seam_year - 1,
    y = 11.2,
    label = "← Observed",
    hjust = 1,
    size = 3.0,
    family = 'sans',
    color = col_annotation,
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = seam_year + 1,
    y = 11.2,
    label = "Projected →",
    hjust = 0,
    size = 3.0,
    family = 'sans',
    color = col_hero,
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = 2060,
    y = 7.4,
    label = "80% uncertainty interval",
    hjust = 0.5,
    size = 2.8,
    family = fonts$text,
    color = col_annotation
  ) +
  annotate(
    "text",
    x = 2060,
    y = 7.0,
    label = "95% uncertainty interval",
    hjust = 0.5,
    size = 2.8,
    family = fonts$text,
    color = col_annotation,
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = 2047,
    y = 12.6,
    label = "Divergence driven\nby fertility assumptions",
    hjust = 0.5,
    size = 2.7,
    family = fonts$text,
    color = col_annotation,
    fontface = "italic",
    lineheight = 0.9
  ) +
  geom_text(
    data = labels_2100,
    aes(x = 2100, y = pop_bn, label = label),
    hjust = -0.08,
    size = 2.9,
    family = fonts$text,
    color = col_hero,
    lineheight = 0.9
  ) +
  
  # Scales
  scale_x_continuous(
    breaks = seq(1950, 2100, by = 25),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    name = "Global population (billions)",
    breaks = seq(2, 16, by = 2),
    labels = \(x) paste0(x, "B"),
    limits = c(2, 16.5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
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