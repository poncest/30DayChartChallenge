
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 24 | Timeseries | Data Day: UNICEF

## Topic:      Child Survival: The Progress That Slowed
## Author:     Steven Ponce
## Date:       2026-04-24
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data:       UN Inter-agency Group for Child Mortality Estimation (UN IGME)
##             via UNICEF. "Under-five Mortality Rates 2024" dataset.
##             Source: https://data.unicef.org/resources/dataset/child-mortality/
##             File: Under-five_Mortality_Rates_2024.xlsx
##             Last updated: March 2025 | License: CC BY 3.0 IGO
##
## File structure:
##   Sheet "U5MR Regional estimates": skip = 15, wide format
##   Columns: Region.Name | Uncertainty.Bounds* | 1990.5 | 1991.5 | ... | 2023.5
##   clean_names() converts year cols to: x1990_5, x1991_5, ..., x2023_5
##   Key regions used: "World", "Sub-Saharan Africa"
##


## 1. LOAD PACKAGES & SETUP ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,      
  scales, glue, camcorder, readxl     
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
cache_path <- here("2026", "data", "u5mr_regional_clean.csv")
df <- read_csv(cache_path)


## 3. EXAMINING THE DATA ----
glimpse(df)
df |> filter(year %in% c(1990, 2000, 2015, 2023)) |> arrange(series, year) 


## 4. TIDY DATA ----

# Data already cleaned and cached. See commented block below to re-run from raw.
# Source file: Under-five_Mortality_Rates_2024.xlsx | Sheet: "U5MR Regional estimates" | skip = 15
# Filtered to: region_name %in% c("World", "Sub-Saharan Africa"), uncertainty_bounds == "Median"
# Pivoted wide→long, year extracted from x1990_5 pattern, distinct() applied to remove duplicates.

# --- Rebuild cache (uncomment if re-downloading raw data) ---
# data_path  <- here("2026", "data", "Under-five_Mortality_Rates_2024.xlsx")
# raw_regional <- read_excel(data_path, sheet = "U5MR Regional estimates", skip = 15) |> clean_names()
# df_wide <- raw_regional |>
#   filter(region_name %in% c("World", "Sub-Saharan Africa"), uncertainty_bounds == "Median") |>
#   select(region_name, starts_with("x"))
# df <- df_wide |>
#   pivot_longer(cols = starts_with("x"), names_to = "year_raw", values_to = "u5mr") |>
#   mutate(year = as.integer(str_extract(year_raw, "\\d{4}")), u5mr = as.numeric(u5mr), series = region_name) |>
#   filter(!is.na(u5mr), year >= 1990, year <= 2023) |>
#   select(series, year, u5mr) |> distinct(series, year, .keep_all = TRUE)
# write_csv(df, cache_path)

### |- computed annotation values (from actual data) ----
world_2000 <- df |> filter(series == "World", year == 2000) |> pull(u5mr) |> first()
world_2015 <- df |> filter(series == "World", year == 2015) |> pull(u5mr) |> first()
world_2023 <- df |> filter(series == "World", year == 2023) |> pull(u5mr) |> first()

arr_mdg <- round((1 - (world_2015 / world_2000)^(1/15)) * 100, 1)  
arr_sdg <- round((1 - (world_2023 / world_2015)^(1/8))  * 100, 1)  


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(palette = list(
  "global_mdg" = "#1a1a2e",     
  "global_sdg" = "#4a5568",     
  "ssa"        = "#c0392b",
  "era_mdg"    = "#e8f4f8",
  "era_sdg"    = "#fef9e7",
  "target"     = "#27ae60",
  "text"       = "#2c3e50",
  "subtle"     = "#95a5a6"
))

col_global_mdg <- colors$palette$global_mdg
col_global_sdg <- colors$palette$global_sdg
col_ssa        <- colors$palette$ssa
col_target     <- colors$palette$target
col_subtle     <- colors$palette$subtle 

### |- titles and caption ----
title_text    <- "Child survival improved dramatically—then progress slowed"
subtitle_text <- paste0(
  "Global under-five mortality fell steeply since 1990—but the pace of progress has slowed.<br>",
  "Annual reduction dropped from **", arr_mdg, "%** (Millennium Development Goal era, 2000–2015) ",
  "to **", arr_sdg, "%** (Sustainable Development Goal era, 2015–2023),<br>",
  "putting the 2030 survival target of ≤25 deaths per 1,000 live births at risk."
)
caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 24,
  source_text = "UN IGME via UNICEF · Under-five Mortality Rates 2024 (data.unicef.org)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- helper ----
endpoint_y <- function(series_name, yr = 2023) {
  df |> filter(series == series_name, year == yr) |> pull(u5mr) |> first()
}

### |- segmented world line data ----
df_world_mdg <- df |> filter(series == "World", year >= 1990, year <= 2015)
df_world_sdg <- df |> filter(series == "World", year >= 2015, year <= 2023)

### |- annotation tibbles ----
label_mdg <- paste0("Millennium Development\nGoal era (2000–2015)\n–", arr_mdg, "% per year")
label_sdg <- paste0("Sustainable Development\nGoal era (2015–2023)\n–", arr_sdg, "% per year")

era_labels <- tibble(
  x     = c(2007.0, 2019.0),
  y     = c(64.0,   68.0),
  label = c(label_mdg, label_sdg),
  col   = c(col_global_mdg, col_global_sdg)
)

sdg_label <- tibble(
  x     = 2023.8,
  y     = 25,
  label = "SDG target (2030): ≤25 deaths per 1,000"
)

### |- base and weekly theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Remove y-axis entirely (J-style: annotations carry the values)
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fonts$text, size = 8.5, color = "#7a8a96"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    # Keep only the SDG target gridline equivalent; drop all others
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = fonts$title, face = "bold", size = 24,
      color = colors$palette$text, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 10.5, color = "#4a5568",
      margin = margin(b = 20), lineheight = 1.4
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = col_subtle,
      margin = margin(t = 12), hjust = 0
    ),
    legend.position = "none",
    plot.margin = margin(16, 110, 10, 16)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot() +
  
  # Era shaded bands
  annotate("rect",
           xmin = 2000, xmax = 2015, ymin = 0, ymax = 100,
           fill = colors$palette$era_mdg, alpha = 0.55
  ) +
  annotate("rect",
           xmin = 2015, xmax = 2023, ymin = 0, ymax = 100,
           fill = colors$palette$era_sdg, alpha = 0.65
  ) +
  
  # SDG target reference line (primary anchor — replaces y-axis)
  geom_hline(yintercept = 25, color = col_target, linewidth = 0.9, linetype = "dashed") +
  
  # Era boundary markers
  geom_vline(xintercept = 2000, color = "gray50", linewidth = 0.4, linetype = "dotted") +
  geom_vline(xintercept = 2015, color = "gray50", linewidth = 0.4, linetype = "dotted") +
  
  # Sub-Saharan Africa (contextual comparator)
  geom_line(
    data    = df |> filter(series == "Sub-Saharan Africa"),
    mapping = aes(x = year, y = u5mr),
    color = col_ssa, linewidth = 0.9, alpha = 0.65, lineend = "round"
  ) +
  
  # World line — MDG era (darker, thicker: faster progress)
  geom_line(
    data    = df_world_mdg,
    mapping = aes(x = year, y = u5mr),
    color = col_global_mdg, linewidth = 1.8, lineend = "round"
  ) +
  
  # World line — SDG era (lighter, thinner: slower progress)
  geom_line(
    data    = df_world_sdg,
    mapping = aes(x = year, y = u5mr),
    color = col_global_sdg, linewidth = 1.2, lineend = "round"
  ) +
  
  # Anchor dot — 1990 world
  annotate("point", x = 1990, y = df |> filter(series == "World", year == 1990) |> pull(u5mr) |> first(),
           color = col_global_mdg, size = 2.5) +
  
  # Endpoint dots — 2023
  annotate("point", x = 2023, y = endpoint_y("World"),
           color = col_global_sdg, size = 3.5) +
  annotate("point", x = 2023, y = endpoint_y("Sub-Saharan Africa"),
           color = col_ssa, size = 2.8) +
  
  # Era labels (plain text, no box)
  annotate("text",
           x = 2007.0, y = 70.0,
           label = paste0("Millennium Development\nGoal era (2000–2015)\n–", arr_mdg, "% per year"),
           family = fonts$text, size = 2.7, color = "gray55",
           hjust = 0.5, lineheight = 1.3
  ) +
  annotate("text",
           x = 2019.5, y = 90.0,
           label = paste0("Sustainable Development\nGoal era (2015–2023)\n–", arr_sdg, "% per year"),
           family = fonts$text, size = 2.7, color = "gray55",
           hjust = 0.5, lineheight = 1.3
  ) +
  
  # SDG target label
  annotate("text",
           x = 2015.5, y = 23.0,
           label = "SDG target (2030): ≤25 deaths per 1,000",
           family = fonts$text, size = 3.1, color = "#1e8449",
           hjust = 0, fontface = "bold"
  ) +
  
  # Deceleration callout — central narrative annotation
  annotate("curve",
           x = 2016.8, xend = 2015.6, y = 50, yend = 43,
           curvature = -0.3, color = "#5a6a7a", linewidth = 0.45,
           arrow = arrow(length = unit(0.08, "inches"), type = "closed")
  ) +
  annotate("text",
           x = 2016.9, y = 51.5,
           label = "Progress slowed after 2015
Annual decline fell from −3.7% to −2.2%",
           family = fonts$text, size = 3.1, color = colors$palette$text,
           hjust = 0, lineheight = 1.35, fontface = "bold"
  ) +
  
  # Series end labels
  annotate("text",
           x = 2023.4, y = endpoint_y("World") - 1.5,
           label = "World", family = fonts$text, size = 3.4,
           color = col_global_sdg, fontface = "bold", hjust = 0
  ) +
  annotate("text",
           x = 2023.7, y = endpoint_y("Sub-Saharan Africa") + 1.5,
           label = "Sub-Saharan
Africa", family = fonts$text, size = 3.4,
           color = col_ssa, fontface = "bold", hjust = 0, lineheight = 1.15
  ) +
  
  # Axes
  scale_x_continuous(
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023),
    limits = c(1989, 2030),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 95),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 95), clip = "off") +
  
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
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

# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────────────
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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpC0mAfJ/file26c46b0c3d52". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
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
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
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
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# readxl       * 1.4.5    2025-03-07 [1] CRAN (R 4.3.3)
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
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────