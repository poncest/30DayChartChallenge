
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 21 · Timeseries | Historical

## Topic:     The Great Escape — Share of World Population in Extreme Poverty (1820–2019)
## Author:    Steven Ponce
## Date:      2026-04-21

## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/:
##       fonts.R, social_icons.R, base_theme.R
##       Key calls: get_theme_colors(), setup_fonts(), get_font_families(),
##                  create_base_theme(), extend_weekly_theme(), create_dcc_caption()

## Data Source:
##   Moatsos, M. (2021) Global extreme poverty: Present and past since 1820.
##   Published in OECD (2021), How Was Life? Volume II.
##   Accessed via Our World in Data:
##   https://ourworldindata.org/grapher/share-of-population-living-in-extreme-poverty
##   Series: 'Cost of basic needs' approach, World entity, 1820–2018
##   Definition: Living on less than $1.90/day in 2011 international-$
##   NOTE: Pre-1981 values are decadal estimates with meaningful uncertainty;
##   rendered at reduced alpha to signal this without removing historical context.


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
  width  = 10,
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

### |- Poverty data 1820-2018 ----
poverty_raw <- read_csv(here::here("2026/data/owid_extreme_poverty_historical.csv"),
                        show_col_types = FALSE) |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(poverty_raw)


## 4. TIDY DATA ----

### |- Wold poverty share 1820-2018 ----
poverty <- poverty_raw |>
  filter(entity == "World") |>
  rename(poverty_share = 4) |> 
  select(year, poverty_share) |>
  filter(!is.na(poverty_share)) |>
  arrange(year) |>
  # Flag pre-1981 data as uncertain (decadal estimates)
  mutate(uncertain = year < 1981)

### |- split into two segments for dual-alpha rendering ----
poverty_early <- poverty |> filter(uncertain)
poverty_recent <- poverty |> filter(!uncertain)

### |- annotation data ----
# Key anchors: start value, inflection point (~1980), end value
val_start  <- poverty |> filter(year == min(year)) |> pull(poverty_share)
val_end    <- poverty |> filter(year == max(year)) |> pull(poverty_share)
yr_start   <- poverty |> filter(year == min(year)) |> pull(year)
yr_end     <- poverty |> filter(year == max(year)) |> pull(year)

# Inflection: where slope steepens most sharply (visually ~1980)
val_1980   <- poverty |> filter(year == 1980 | year == 1981) |> slice(1) |> pull(poverty_share)
yr_inflect <- poverty |> filter(year == 1980 | year == 1981) |> slice(1) |> pull(year)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg         = "#F5F1EC",    
    line_main  = "#6B0F1A",     
    area_fill  = "#6B0F1A",   
    line_dim   = "#6B0F1A",    
    ref_band   = "#C8B8A2",    
    text_dark  = "#2C1A0E",    
    text_mid   = "#6B5744",     
    gridline   = "#DDD5C8"      
  )
)

### |- color scalars for use inside geom layers ----
col_line_main <- colors$palette$line_main
col_line_dim  <- colors$palette$line_dim
col_area      <- colors$palette$area_fill
col_ref_band  <- colors$palette$ref_band
col_text_dark <- colors$palette$text_dark
col_text_mid  <- colors$palette$text_mid
col_bg        <- colors$palette$bg

### |- titles and caption ----
title_text    <- "A Century of Stagnation. Four Decades of Change."

subtitle_text <- "Share of the world population living in extreme poverty (cost of basic needs approach), 1820–2018.<br>
Most of the decline occurs after 1980. Early estimates (pre-1981) carry uncertainty."

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 21,
  source_text = "Moatsos (2021) via Our World in Data · Definition: <$1.90/day, 2011 int.-$"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Background
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),

    # Grid — horizontal only, very faint
    panel.grid.major.y = element_line(color = colors$palette$gridline, linewidth = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Axes
    axis.ticks = element_blank(),
    axis.text = element_text(family = fonts$text, color = col_text_mid, size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = fonts$text, color = col_text_mid,
      size = 9, margin = margin(r = 8)
    ),

    # Titles
    plot.title = element_text(
      family = fonts$text, face = "bold", size = 16,
      color = col_text_dark, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 10, color = col_text_mid,
      lineheight = 1.4, margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7, color = col_text_mid,
      hjust = 0, margin = margin(t = 16)
    ),

    # Margins
    plot.margin = margin(t = 20, r = 30, b = 16, l = 20)
  )
)

theme_set(weekly_theme)

p <- ggplot() +

   annotate(
    "rect",
    xmin = 1820, xmax = 1960,
    ymin = 75, ymax = 90,
    fill = col_ref_band, alpha = 0.18
  ) +
  annotate(
    "text",
    x = yr_start + 5, y = 87.5,
    label = "Historical baseline\n(~75–90% of humanity)",
    hjust = 0, vjust = 0.5,
    family = fonts$text, size = 2.8, color = col_text_mid
  ) +
  geom_area(
    data = poverty_early,
    mapping = aes(x = year, y = poverty_share),
    fill = col_area, alpha = 0.10, color = NA
  ) +
  geom_area(
    data = poverty_recent,
    mapping = aes(x = year, y = poverty_share),
    fill = col_area, alpha = 0.2, color = NA
  ) +
  geom_line(
    data = poverty_early,
    mapping = aes(x = year, y = poverty_share),
    color = col_line_dim, linewidth = 0.55, alpha = 0.4,
    linetype = "solid"
  ) +
  geom_line(
    data = poverty_recent,
    mapping = aes(x = year, y = poverty_share),
    color = col_line_main, linewidth = 0.95
  ) +
  annotate(
    "point",
    x = yr_inflect, y = val_1980,
    color = col_line_main, size = 2.2, shape = 21,
    fill = col_bg, stroke = 1.2
  ) +
  annotate(
    "text",
    x = yr_inflect + 1, y = val_1980 + 2,
    label = glue("{round(val_1980)}% in {yr_inflect}"),
    hjust = 0, vjust = 0,
    family = fonts$text, size = 2.9, color = col_text_dark,
    lineheight = 1.3
  ) +
  annotate(
    "point",
    x = yr_start, y = val_start,
    color = col_line_dim, size = 2, shape = 21,
    fill = col_bg, stroke = 1
  ) +
  annotate(
    "text",
    x = yr_start + 2, y = val_start - 4,
    label = glue("{round(val_start)}%\n({yr_start})"),
    hjust = 0, vjust = 1,
    family = fonts$text, size = 2.9, color = col_text_mid,
    lineheight = 1.3
  ) +
  annotate(
    "point",
    x = yr_end, y = val_end,
    color = col_line_main, size = 2.2, shape = 21,
    fill = col_bg, stroke = 1.2
  ) +
  annotate(
    "text",
    x = yr_end + 1, y = val_end + 6,
    # label = glue("{round(val_end, 1)}%\ntoday"),
    label = glue("{round(val_end, 1)}%\n(2018)"),
    hjust = 1, vjust = 0,
    family = fonts$text, size = 2.9, color = col_text_dark,
    lineheight = 1.3
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    y        = "Share in extreme poverty (%)"
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
