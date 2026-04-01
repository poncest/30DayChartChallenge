
## Challenge: #30DayChartChallenge 2026 — Day 01
## Prompt:   Part-to-Whole
## Topic:    Where Does $7 Trillion Go? U.S. Federal Spending FY2025

## Author:   Steven Ponce
## Date:     2026-04-01

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Source:   Congressional Budget Office (CBO) — Monthly Budget Review, FY2025
##           https://www.cbo.gov/publication/61307


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,     
  janitor, scales, glue          
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 7,
  height = 6,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))

## 2. READ IN THE DATA ----

# FY2025 actual federal outlays (billions USD)
# Source: CBO Monthly Budget Review, October 2025 (fiscal year end)
# Total outlays: ~$7.0 trillion | Deficit: $1.8 trillion
#
# Key verified figures (CBO + CRFB reconciliation):
#   Social Security: FY2024 $1,439B + $121B increase = ~$1,560B
#   Medicare:        FY2024 ~$973B  + $117B increase = ~$1,090B  (net of offsetting receipts)
#   Net Interest:    Surpassed $1T for first time; FY2024 ~$970B + $80B = ~$1,050B
#   Defense:         FY2024 ~$895B  + $38B  increase = ~$933B
#   Medicaid:        FY2024 ~$596B  + $52B  increase = ~$648B
#   Veterans Affairs:FY2024 ~$284B  + $41B  increase = ~$325B
#   Other Mandatory + Other Discretionary: residual to $7,000B total
#   Residual = 7000 - (1560+1090+1050+933+648+325) = ~$1,394B split below
spending_raw <- tibble(
  category = c(
    "Social Security",
    "Medicare",
    "Net Interest",
    "Defense",
    "Medicaid",
    "Other Mandatory",
    "Other Discretionary",
    "Veterans Affairs"
  ),
  billions = c(1560, 1090, 1050, 933, 648, 747, 647, 325)
)


## 3. EXAMINING THE DATA ----
glimpse(spending_raw)


## 4. TIDY DATA ----

spending <- spending_raw |>
  mutate(
    # Calculate percentage share of total
    pct       = billions / sum(billions),
    pct_label = percent(pct, accuracy = 0.1),
    
    # Waffle units: 1 unit = ~$70B (so 100 squares = ~$7T)
    units = round(billions / 70),
    
    # Spending type — for secondary grouping / color logic
    type = case_when(
      category %in% c("Social Security", "Medicare", "Medicaid", "Other Mandatory") ~ "Mandatory",
      category == "Net Interest" ~ "Net Interest",
      category %in% c("Defense", "Veterans Affairs", "Other Discretionary") ~ "Discretionary"
    ),
    
    category = factor(category, levels = c(
      "Social Security",
      "Medicare",
      "Net Interest",
      "Other Mandatory",
      "Medicaid",
      "Defense",
      "Other Discretionary",
      "Veterans Affairs"
    ))
  )

# Expand for waffle — each row = one square
waffle_data <- spending |>
  arrange(desc(billions)) |>
  uncount(units) |>
  mutate(
    row_id = row_number(),
    x      = (row_id - 1) %% 10 + 1,
    y      = 10 - ((row_id - 1) %/% 10)
  )

# Total squares
total_squares <- sum(spending$units)  # ~100

### |- legend labels ----
legend_labels <- spending |>
  arrange(category) |>
  mutate(
    label = glue("{category}  ${scales::comma(billions)}B")
  ) |>
  pull(label, name = category)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Social Security"     = "#2F6C8E",
    "Medicare"            = "#4B9FC0",
    "Net Interest"        = "#D97941",
    "Defense"             = "#4A6FA5",   
    "Medicaid"            = "#76B8C8",
    "Other Mandatory"     = "#9CC9D5",
    "Other Discretionary" = "#B98AA0",
    "Veterans Affairs"    = "#9A5874"
  )
)

### |- titles and caption ----
title_text <- str_glue("Where Does $7 Trillion Go?")

subtitle_text <- str_glue(
  "U.S. federal outlays, FY2025 — {total_squares} squares represent about $7.0T (≈ $70B each)<br>",
  "Total spending: **$7.0T** | Deficit: **$1.8T** | Source: **CBO**"
)

caption_text <- create_dcc_caption(
  dcc_year = 2026,
  dcc_day = 01,
  source_text = "Congressional Budget Office (CBO), Monthly Budget Review FY2025"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),

    # Grid — none needed for waffle
    panel.grid = element_blank(),

    # Legend
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(
      size = 9, face = "bold", family = fonts$text, color = colors$text
    ),
    legend.text = element_text(
      size = 8, family = fonts$text, color = colors$text
    ),
    legend.key.size = unit(0.9, "lines"),
    legend.key.spacing.y = unit(2, "pt"),
    plot.margin = margin(20, 20, 16, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <-
  ggplot(waffle_data, aes(x = x, y = y, fill = category)) +
  # Geoms
  geom_tile(
    color     = "white",
    linewidth = 0.5,
    width     = 0.92,
    height    = 0.92
  ) +
  # Scales
  scale_fill_manual(
    values = unlist(colors$palette),
    labels = legend_labels,
    name   = NULL
  ) +
  # Scales
  scale_x_continuous(expand = expansion(add = 0.5)) +
  scale_y_continuous(expand = expansion(add = 0.5)) +
  coord_equal() +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.75),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.75),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_markdown(
      size = rel(0.5),
      family = fonts$caption,
      color = colors$caption,
      lineheight = 1.1,
      hjust = 0,
      halign = 0,
      margin = margin(t = 10, b = 5)
    ),
  )

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
#   1. Clone the repo: https://github.com/poncest/0DayChartChallenge/
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

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-18
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpuGWOmg/file8f543769122d". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
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
# pillar         1.10.2   2025-04-05 [1] CRAN (R 4.3.3)
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
# ───────────────────────────────────────────────────────────────────────
