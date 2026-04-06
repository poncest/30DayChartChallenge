
## Challenge: #30DayChartChallenge 2026 
## Prompt:    Day 06 | Comparisons | Flowing Data Theme
## Topic:    Renewables at Different Speeds — Wind + Solar Share of Electricity
##           (1990–2023), 12 Countries

## Author:    Steven Ponce
## Date:      2026-04-06

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source: Our World in Data — Energy Dataset
##   https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
##   Metric: carbon_intensity_elec (gCO₂/kWh)
##   Years:  2020–2023 average per country

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
  width  = 8,
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
owid_energy_raw <- read_csv("2026/data/owid-energy-data.csv")


## 3. EXAMINING THE DATA ----
glimpse(owid_energy_raw)


## 4. TIDY DATA ----

### |- countries of interest ----
countries_select <- c(
  "Denmark", "Germany", "Portugal", "United Kingdom",
  "France", "United States", "China", "India",
  "Brazil", "Australia", "Poland", "South Africa"
)

### |- panel order: descending by 2023 renewables share ----
country_order <- owid_energy_raw |>
  filter(
    country %in% countries_select,
    year == 2023
  ) |>
  mutate(
    wind_share_elec  = replace_na(wind_share_elec, 0),
    solar_share_elec = replace_na(solar_share_elec, 0),
    renewables_share = wind_share_elec + solar_share_elec
  ) |>
  arrange(desc(renewables_share)) |>
  pull(country)

### |- country display labels (mixed case — editorial Yau register) ----
country_labels <- c(
  "Denmark"        = "Denmark",
  "Germany"        = "Germany",
  "Portugal"       = "Portugal",
  "United Kingdom" = "United Kingdom",
  "France"         = "France",
  "United States"  = "United States",
  "China"          = "China",
  "India"          = "India",
  "Brazil"         = "Brazil",
  "Australia"      = "Australia",
  "Poland"         = "Poland",
  "South Africa"   = "South Africa"
)

### |- tidy ----
df_renewables <- owid_energy_raw |>
  filter(
    country %in% countries_select,
    year >= 1990,
    year <= 2023
  ) |>
  select(country, year, wind_share_elec, solar_share_elec) |>
  mutate(
    wind_share_elec  = replace_na(wind_share_elec, 0),
    solar_share_elec = replace_na(solar_share_elec, 0),
    renewables_share = wind_share_elec + solar_share_elec,
    country          = factor(country, levels = country_order),
    country_label    = recode(country, !!!country_labels)
  )

### |- end-point labels (2023 value for direct annotation) ----
df_endpoint <- df_renewables |>
  filter(year == 2023) |>
  select(country, country_label, renewables_share)

### |- panel annotation: 2023 share label ----
# Used inside each facet as a direct value callout
df_annotation <- df_endpoint |>
  mutate(
    label = glue("{round(renewables_share, 0)}%"),
    year  = 2023
  )

### |- optional narrative annotation ----
df_note <- tibble(
  country_label = factor("Denmark", levels = levels(df_renewables$country_label)),
  x = 2006,
  y = 65,
  label = "Early leader"
)



# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_accent    = "#d4703a",
    col_area_fill = "#faeee7",
    col_grid      = "#eeeeee",
    col_text_dark = "#222222",
    col_text_mid  = "#666666",
    col_text_mute = "#aaaaaa",
    col_bg        = "#ffffff"
  )
)

### |- titles and caption ----
title_text    <- "Wind and solar are growing — but not at the same pace"

subtitle_text <- "Wind + solar as a share of electricity generation, 1990–2023. Each panel shows one country."

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 6,
  source_text = "Our World in Data — Energy Dataset (Ember / Energy Institute)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

# FlowingData-specific font additions
font_add_google("Special Elite", "special_elite")   
font_add_google("Source Sans 3", "source_sans")     
showtext_auto(enable = TRUE)

### |- base theme (FlowingData-inspired) ----
theme_flowingdata <- theme_minimal(base_size = 10) +
  theme(
    # -- plot background --
    plot.background    = element_rect(fill = colors$palette$col_bg, color = NA),
    panel.background   = element_rect(fill = colors$palette$col_bg, color = NA),
    
    # -- grid: horizontal only, barely visible --
    panel.grid.major.y = element_line(color = colors$palette$col_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # -- axes: no lines, no ticks --
    axis.line          = element_blank(),
    axis.ticks         = element_blank(),
    
    # -- axis text: small, muted --
    axis.text.x        = element_text(
      family = "source_sans", size = 7, color = colors$palette$col_text_mute,
      margin = margin(t = 2)
    ),
    axis.text.y        = element_text(
      family = "source_sans", size = 7, color = colors$palette$col_text_mute,
      margin = margin(r = 2)
    ),
    axis.title         = element_blank(),
    
    # -- facet strip: mixed case country labels, monospace, left-aligned --
    strip.text = element_text(
      family   = "special_elite",
      size     = 8,
      color    = colors$palette$col_text_dark,
      hjust    = 0,
      margin   = margin(b = 4)
    ),
    strip.background   = element_blank(),
    
    # -- legend: none (direct annotation) --
    legend.position    = "none",
    
    # -- plot titles --
    plot.title         = element_text(
      family = "special_elite",
      size   = 15,
      color  = colors$palette$col_text_dark,
      hjust  = 0,
      face   = "bold",
      margin = margin(b = 4)
    ),
    plot.subtitle      = element_text(
      family = "source_sans",
      size   = 10,
      color  = colors$palette$col_text_mid,
      hjust  = 0,
      margin = margin(b = 16)
    ),
    plot.caption       = element_textbox_simple(
      family    = "source_sans",
      size      = 7,
      color     = colors$palette$col_text_mute,
      hjust     = 0,
      margin    = margin(t = 12)
    ),
    
    # -- spacing --
    plot.margin        = margin(16, 16, 12, 16),
    panel.spacing.x    = unit(1.6, "lines"),
    panel.spacing.y    = unit(1.8, "lines")
  )


## |- main plot ----
p <- df_renewables |>
  ggplot(aes(x = year, y = renewables_share)) +
  
  # -- area fill beneath the line --
  geom_area(
    fill  = colors$palette$col_area_fill,
    alpha = 0.8
  ) +
  
  # -- main line --
  geom_line(
    color     = colors$palette$col_accent,
    linewidth = 0.8
  ) +
  
  # -- 2023 endpoint dot --
  geom_point(
    data  = df_endpoint,
    aes(x = 2023, y = renewables_share),
    color = colors$palette$col_accent,
    size  = 1.8
  ) +
  
  # -- direct annotation: 2023 % value, top-right of each panel --
  geom_text(
    data   = df_annotation,
    aes(x  = 2023, y = renewables_share, label = label),
    hjust  = 1.1,
    vjust  = -0.6,
    family = "special_elite",
    size   = 2.8,
    color  = colors$palette$col_accent
  ) +
  
  ## optional narrative note
  geom_text(
    data = df_note,
    aes(x = x, y = y, label = label),
    family = "source_sans",
    size   = 2.5,
    color  = colors$palette$col_text_mid,
    hjust  = 0,
    inherit.aes = FALSE
  ) +
  
  # -- reference line at 50% (parity) --
  geom_hline(
    yintercept = 50,
    linetype   = "dashed",
    color      = colors$palette$col_text_mute,
    linewidth  = 0.3
  ) +
  
  # -- scales --
  scale_x_continuous(
    breaks = c(1990, 2005, 2023),
    labels = c("1990", "'05", "'23")
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = c(0, 25, 50, 75),
    labels = function(x) ifelse(x == 0, "0%", paste0(x, "%"))
  ) +
  
  # -- facets: 3 rows × 4 cols, country label as strip --
  facet_wrap(
    ~ country_label,
    nrow   = 3,
    ncol   = 4
  ) +
  
  # -- labels --
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  theme_flowingdata


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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpmQ4tLk/file64d0483c21db". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
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

# ───────────────────────────────────────────────────────────────────────
