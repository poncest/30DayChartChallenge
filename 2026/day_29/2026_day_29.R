
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 29 | Uncertainties | Monochrome

## Topic:      When warmth stops looking exceptional
## Author:     Steven Ponce
## Date:       2026-04-29
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data source:
##   NASA GISS Surface Temperature Analysis (GISTEMP v4)
##   Monthly global temperature anomalies, 1880–2024
##   URL: https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv


## 1. LOAD PACKAGES & SETUP ----

if (!require("pacman")) install.packages("pacman")         
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,      
  scales, glue, camcorder, ggridges  
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

### |- cache path ----
data_path <- here("2026/data/gistemp_global_monthly.csv")

### |- fetch or load ----
# if (!file.exists(data_path)) {
#   url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"
#   download.file(url, destfile = data_path, mode = "wb")
#   message("Downloaded GISTEMP data to: ", data_path)
# } else {
#   message("Using cached GISTEMP data.")
# }

### |- raw read ----
# NASA GISS format: first row is header info, second row is column names
# Anomalies in units of 0.01°C in older files; v4 is in °C directly
# File has a header comment row — skip = 1 handles it
raw <- read_csv(
  data_path,
  skip    = 1,
  na      = c("***", "****", ""),
  show_col_types = FALSE
)

## 3. EXAMINING THE DATA ----
glimpse(raw)


## 4. TIDY DATA ----

### |- month columns only, pivot long ----
month_cols <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

df <- raw |>
  clean_names() |>
  select(year, all_of(tolower(month_cols))) |>
  filter(year >= 1950, year <= 2024) |>
  pivot_longer(
    cols = -year,
    names_to = "month_abbr",
    values_to = "anomaly"
  ) |>
  mutate(anomaly = as.numeric(anomaly)) |>
  filter(!is.na(anomaly))

### |- assign decade labels ----
df <- df |>
  mutate(
    decade = floor(year / 10) * 10,
    decade_label = paste0(decade, "s"),
    decade_fct = factor(decade_label, levels = rev(paste0(seq(1950, 2020, 10), "s")))
  )

### |- decade summary stats (for annotation anchors) ----
decade_stats <- df |>
  group_by(decade, decade_label, decade_fct) |>
  summarise(
    mean_anom = mean(anomaly, na.rm = TRUE),
    sd_anom = sd(anomaly, na.rm = TRUE),
    .groups = "drop"
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg     = "#F5F3EE",   
    low    = "#EAD9D5",   
    mid_lo = "#C17B7B",   
    mid_hi = "#8B3030",   
    hi     = "#3D0C0C",   
    text   = "#2C2825",   
    subtle = "#7A7068",   
    grid   = "#E8E4DE"    
  )
)

# pre-extract scalar color values  
col_bg     <- colors$palette$bg
col_subtle <- colors$palette$subtle
col_mid_hi <- colors$palette$mid_hi
col_grid   <- colors$palette$grid
col_text   <- colors$palette$text

# Decade fill ramp: 8 stops, lightest (1950s) → darkest (2020s)
decade_fills <- c(
  "1950s" = "#EAD9D5",
  "1960s" = "#DDB8B1",
  "1970s" = "#CC9590",
  "1980s" = "#B86E6E",
  "1990s" = "#9E4A4A",
  "2000s" = "#8B3030",
  "2010s" = "#611818",
  "2020s" = "#3D0C0C"
)

### |- titles and caption ----
title_text <- "When warmth stops looking exceptional"

subtitle_text <- glue(
  "Each curve shows the distribution of monthly temperature anomalies by decade ",
  "(°C, vs. 1951–1980 mean).<br>",
  "**Width** = variability (uncertainty). **Position** = signal. ",
  "Early decades are wide and centered near zero — later decades are<br>",
  "narrower and shifted right. Uncertainty gives way to persistence."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 29,
  source_text = "NASA GISS Surface Temperature Analysis (GISTEMP v4)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      family = fonts$text, size = 8,
      color  = col_subtle, margin = margin(t = 4)
    ),
    axis.text.y = element_text(
      family = fonts$text, size = 9,
      color  = col_subtle, hjust = 1
    ),
    axis.title.x = element_text(
      family = fonts$text, size = 8,
      color  = col_subtle, margin = margin(t = 8)
    ),
    axis.title.y = element_blank(),
    
    plot.title = element_text(
      family = fonts$title,
      face = "bold",
      size = 20,
      color = col_text,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text,
      size = 9.5,
      color = col_subtle,
      lineheight = 1.5,
      margin = margin(b = 16)
    ),
    plot.caption = element_markdown(
      family = fonts$text,
      size = 7,
      color = col_subtle,
      hjust = 1,
      margin = margin(t = 12)
    ),
    
    legend.position = "none",
    plot.margin = margin(20, 24, 10, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- df |>
  ggplot(aes(x = anomaly, y = decade_fct, fill = decade_fct)) +

  # Geoms
  geom_vline(
    xintercept = 0,
    color = col_subtle,
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_density_ridges(
    scale = 1.6,
    rel_min_height = 0.005,
    bandwidth = 0.08,
    color = NA,
    alpha = 0.82
  ) +
  geom_point(
    data = decade_stats,
    aes(x = mean_anom, y = decade_fct),
    color = col_bg,
    fill = col_bg,
    size = 1.6,
    shape = 21,
    stroke = 0.4
  ) +

  # Scales
  scale_fill_manual(values = decade_fills, guide = "none") +
  scale_x_continuous(
    breaks = seq(-1, 1.5, by = 0.5),
    labels = function(x) ifelse(x == 0, "0", sprintf("%+.1f", x)),
    expand = expansion(add = c(0.05, 0.15))
  ) +

  # Annotate
  annotate(
    "text",
    x = -0.9, y = 1.4,
    label = "Wide = more uncertain",
    hjust = 0, vjust = 0,
    size = 2.8,
    family = fonts$text,
    color = col_subtle,
    fontface = "italic"
  ) +
  annotate(
    "segment",
    x = -0.55, xend = -0.15,
    y = 1.65,  yend = 1.65,
    arrow = arrow(length = unit(4, "pt"), type = "open"),
    color = col_subtle,
    linewidth = 0.35
  ) +
  annotate(
    "text",
    x = 0.9, y = 7.6,
    label = "Shifted right = persistent warmth",
    hjust = 0, vjust = 0,
    size = 2.8,
    family = fonts$text,
    color = col_mid_hi,
    fontface = "italic"
  ) +
  annotate(
    "segment",
    x = 0.85, xend = 0.62,
    y = 7.85, yend = 7.85,
    arrow = arrow(length = unit(4, "pt"), type = "open"),
    color = col_mid_hi,
    linewidth = 0.35
  ) +
  annotate(
    "text",
    x = 0.03, y = 8.6,
    label = "baseline\n(1951–1980)",
    hjust = 0, vjust = 1,
    size = 2.4,
    family = fonts$text,
    color = col_subtle,
    fontface = "italic",
    lineheight = 1.1
  ) +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x = "Temperature anomaly (°C)",
    y = NULL
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

# ─ Session info ────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-30
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
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
# ggridges     * 0.5.7    2025-08-27 [1] CRAN (R 4.3.1)
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
# ───────────────────────────────────────────────────────────────────────────────
