
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 09 | Distributions | Wealth
## Topic:      From Survival to Stability — Global Income Distribution (2000 vs. 2022)

## Author:     Steven Ponce
## Date:       2026-04-09

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source: World Bank Poverty and Inequality Platform (2025) via Our World in Data
##              https://ourworldindata.org/grapher/distribution-of-population-between-different-poverty-thresholds-stacke-bar
##              Full dataset (2,793 rows) | Downloaded: 2026-03-22
##              Columns are absolute population counts; shares computed in script


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
  width  = 10,
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

# Source: World Bank Poverty and Inequality Platform (2025) via Our World in Data
# Full dataset filtered to global aggregate (Entity == "World") for 2000 and 2022
# Raw columns are absolute population counts — converted to % shares in tidy step

income_raw <- read_csv(here::here("2026/data/owid_income_distribution.csv")) |>
  clean_names() |>
  filter(entity == "World", year %in% c(2000, 2022))


## 3. EXAMINING THE DATA ----
glimpse(income_raw)


## 4. TIDY DATA ----

# Bracket factor levels: low → high income 
bracket_levels <- c(
  "< $3",
  "$3 – $10",
  "$10 – $30",
  "> $30"
)

income_df <- income_raw |>
  # Convert absolute counts to % shares
  mutate(
    total        = below_3_a_day + x3_10_a_day + x10_30_a_day + above_30_a_day,
    "< $3"       = below_3_a_day / total * 100,
    "$3 – $10"   = x3_10_a_day / total * 100,
    "$10 – $30"  = x10_30_a_day / total * 100,
    "> $30"      = above_30_a_day / total * 100
  ) |>
  select(year, all_of(bracket_levels)) |>
  pivot_longer(
    cols      = all_of(bracket_levels),
    names_to  = "bracket",
    values_to = "share"
  ) |>
  mutate(
    bracket  = factor(bracket, levels = bracket_levels),
    year_lbl = factor(year, levels = c(2022, 2000), labels = c("2022", "2000"))
  ) |>
  arrange(year, bracket) |>
  group_by(year) |>
  mutate(
    xmax = cumsum(share),
    xmin = xmax - share,
    xmid = (xmin + xmax) / 2
  ) |>
  ungroup()

### |- annotation anchors (data-driven) ----
ann_poverty_x <- income_df |>
  filter(bracket == "< $3") |> summarise(xmid = mean(xmid)) |> pull(xmid)

ann_wealth_x <- income_df |>
  filter(year == 2022, bracket == "> $30") |> pull(xmid)

ann_middle_x <- income_df |>
  filter(year == 2022, bracket == "$10 – $30") |> pull(xmid)

### |- segment labels ----
income_labels <- income_df |>
  filter(share >= 9)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "< $3"      = "#6B0F1A",  
    "$3 – $10"  = "#C94A3A",  
    "$10 – $30" = "#8FAFC3",   
    "> $30"     = "#1B3A52"  
  )
)

### |- titles and caption ----
title_text    <- "From Survival to Stability"

subtitle_text <- "Share of the global population across daily income thresholds (PPP-adjusted), 2000 vs. 2022"

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 9,
  source_text = "World Bank Poverty and Inequality Platform (2025) via Our World in Data"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

font_body    <- fonts$text    %||% ""
font_title   <- fonts$title   %||% ""
font_caption <- fonts$caption %||% ""

### |- plot theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Axes
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size   = 14,
      face   = "plain",
      family = fonts$text,
      color  = colors$text,
      margin = margin(r = 8)
    ),
    axis.ticks = element_blank(),

    # Grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Legend
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(
      size   = 9.5,
      family = fonts$text,
      color  = colors$text
    ),
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.spacing.x = unit(0.6, "cm"),

    # Margins
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(
  data    = income_df,
  mapping = aes(x = share, y = year_lbl, fill = bracket)
) +
  geom_col(
    position  = position_stack(reverse = TRUE),
    width = 0.5,
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    data  = income_labels |> filter(bracket != "$10 – $30"),
    mapping  = aes(x = xmid, y = year_lbl, label = glue("{round(share)}%")),
    color = "white",
    size  = 3.8,
    fontface  = "bold",
    family  = fonts$text,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = income_labels |> filter(bracket == "$10 – $30"),
    mapping = aes(x = xmid, y = year_lbl, label = glue("{round(share)}%")),
    color = "#0B1F2A",
    size  = 3.8,
    fontface  = "bold",
    family = fonts$text,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = ann_poverty_x, y = 2.45,
    label = "Extreme poverty\nnearly halved",
    size = 2.9,
    color = colorspace::lighten(colors$text, 0.3),
    family = fonts$text,
    hjust = 0.5,
    lineheight = 1.25
  ) +
  annotate(
    "text",
    x = ann_wealth_x, y = 0.58,
    label = "Above $30/day\nalmost doubled",
    size = 2.9,
    color = colorspace::lighten(colors$text, 0.3),
    family = fonts$text,
    hjust = 0.5,
    lineheight = 1.25
  ) +
  annotate(
    "text",
    x = ann_middle_x, y = 2.45,
    label = "Largest growth:\n$10 – $30/day",
    size = 3.6,
    fontface = "bold",
    color = colors$text,
    family = fonts$text,
    hjust = 0.5,
    lineheight = 1.25
  ) +
  scale_fill_manual(
    values = unname(colors$palette),
    breaks = bracket_levels,
    guide  = guide_legend(nrow = 1, reverse = FALSE)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.01)),
    limits = c(0, 100)
  ) +
  labs(
    title  = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  theme(
    plot.title = element_text(
      size = 22,
      face   = "bold",
      family = fonts$title,
      color  = colors$title,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 11,
      family = fonts$text,
      color  = colors$text,
      lineheight = 1.3,
      margin = margin(b = 16)
    ),
    plot.caption = element_markdown(
      size  = 7,
      family = fonts$caption,
      color = colors$caption,
      hjust  = 0,
      lineheight = 1.2,
      margin = margin(t = 14)
    )
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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmp2zEZEc/file60c420a75d26". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# colorspace     2.1-1    2024-07-26 [1] CRAN (R 4.3.3)
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
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────
