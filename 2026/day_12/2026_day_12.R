
## Challenge:   #30DayChartChallenge 2026
## Prompt:      Day 12 | Distributions | FlowingData Theme Day
## Topic:       Distance from Normal — Carbon Intensity of Electricity by Country
##              Deviation from global median (gCO₂/kWh), most recent year available
##
## Author:      Steven Ponce
## Date:        2026-04-12
##
## NOTE:        This script uses custom helper functions for theming and formatting.
##              See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.
##
## Data source: Our World in Data — Energy Dataset
##              https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
##              Metric: carbon_intensity_elec (gCO₂/kWh)
##              Year:   most recent non-NA per country (≤ 2023)


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
owid_energy_raw <- read_csv(here::here("2026/data/owid-energy-data.csv"))


## 3. EXAMINING THE DATA ----
glimpse(owid_energy_raw)


## 4. TIDY DATA ----

### |- exclude aggregates (OWID uses these non-country entities) ----
non_countries <- c(
  "World", "Africa", "Asia", "Europe", "North America", "South America",
  "Oceania", "Antarctica", "European Union (27)",
  "High-income countries", "Low-income countries",
  "Lower-middle-income countries", "Upper-middle-income countries",
  "OECD (Edelstein)", "Non-OECD (Edelstein)",
  "Asia Pacific (Ember)", "Europe (Ember)", "Middle East (Ember)",
  "Africa (Ember)", "Americas (Ember)"
)

### |- most recent non-NA carbon intensity per country ----
df_ci <- owid_energy_raw |>
  filter(
    !country %in% non_countries,
    year <= 2023,
    !is.na(carbon_intensity_elec),
    carbon_intensity_elec > 0
  ) |>
  group_by(country) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country, year, carbon_intensity_elec)

### |- compute global median + deviation ----
global_median <- median(df_ci$carbon_intensity_elec, na.rm = TRUE)

df_dev <- df_ci |>
  mutate(
    deviation    = carbon_intensity_elec - global_median,
    direction    = if_else(deviation >= 0, "above", "below"),
    rank_dev     = rank(deviation, ties.method = "first"),
    country_fct  = fct_reorder(country, deviation)
  )

### |- identify highlight countries ----
top_above <- df_dev |>
  filter(direction == "above") |>
  arrange(desc(deviation)) |>
  slice_head(n = 3) |>
  pull(country)

top_below <- df_dev |>
  filter(direction == "below") |>
  arrange(deviation) |>
  slice_head(n = 2) |>
  pull(country)

highlight_countries <- c(top_above, top_below)

df_dev <- df_dev |>
  mutate(
    highlight = country %in% highlight_countries,
    highlight_type = case_when(
      country %in% top_above ~ "above",
      country %in% top_below ~ "below",
      TRUE ~ "none"
    )
  )

### |- label data (only highlighted countries) ----
df_labels <- df_dev |>
  filter(highlight) |>
  mutate(
    label = case_when(
      deviation >= 0 ~ glue("{country}\n+{round(deviation)} gCO₂/kWh"),
      TRUE ~ glue("{country}\n{round(deviation)} gCO₂/kWh")
    )
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_accent_above = "#7a3b3b",   
    col_accent_below = "#6b7c85",   
    col_dot_base     = "#c9c6c1",   
    col_zero         = "#aaaaaa",  
    col_grid         = "#eeeeee",
    col_text_dark    = "#4a4a4a",
    col_text_mid     = "#6a6a6a",
    col_text_mute    = "#aaaaaa",
    col_bg           = "#ffffff"
  )
)

# extract scalars
col_above <- colors$palette$col_accent_above
col_below <- colors$palette$col_accent_below
col_base  <- colors$palette$col_dot_base

### |- titles and caption ----
title_text    <- "Distance from Normal"

subtitle_text <- glue(
  "Most countries sit near the global norm. A small group sits far above.<br>",
  "Each dot is one country. Deviation from global median ({round(global_median)} gCO₂/kWh)."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 12,
  source_text = "Our World in Data — Energy Dataset (Ember / Energy Institute)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

# FlowingData-specific font additions 
font_add_google("Special Elite", "special_elite")
font_add_google("Source Sans 3", "source_sans")
showtext_auto(enable = TRUE)

### |- theme (FlowingData-inspired) ----
theme_fd <- theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = colors$palette$col_bg, color = NA),
    panel.background = element_rect(fill = colors$palette$col_bg, color = NA),

    # very faint horizontal reference only
    panel.grid.major.x = element_line(color = colors$palette$col_grid, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),

    # y-axis: no country labels 
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      family = "source_sans", size = 8,
      color = colors$palette$col_text_mute,
      margin = margin(t = 3)
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_text(
      family = "source_sans", size = 9,
      color = colors$palette$col_text_mid,
      margin = margin(t = 6)
    ),
    legend.position = "none",
    plot.title = element_text(
      family = "special_elite",
      size  = 22,
      color = colors$palette$col_text_dark,
      hjust = 0,
      face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      family = "source_sans",
      size = 10,
      color = colors$palette$col_text_mid,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      family = "source_sans",
      size = 7,
      color = colors$palette$col_text_mute,
      hjust = 0,
      margin = margin(t = 14)
    ),
    plot.margin = margin(20, 20, 14, 20)
  )

### |- main plot ----
p <- df_dev |>
  ggplot(aes(x = deviation, y = rank_dev)) +
  
  # Geoms
  geom_vline(
    xintercept = 0,
    color = colors$palette$col_zero,
    linewidth  = 0.5,
    linetype = "solid"
  ) +
  geom_point(
    data = filter(df_dev, !highlight),
    color = col_base,
    size = 1.6,
    alpha = 0.90,
    shape = 16
  ) +
  geom_point(
    data = filter(df_dev, highlight_type == "above"),
    color = col_above,
    size = 2.8,
    shape = 16
  ) +
  geom_point(
    data = filter(df_dev, highlight_type == "below"),
    color = col_below,
    size = 1.8,    
    shape = 16
  ) +
  geom_text_repel(
    data = filter(df_labels, highlight_type == "above"),
    aes(label = label),
    family = "source_sans",
    size = 2.6,
    color = col_above,
    fontface = "bold",
    hjust = 0,
    direction = "y",
    nudge_x = 60,
    force = 2,
    force_pull = 0.5,
    segment.color = col_above,
    segment.size = 0.3,
    segment.alpha = 0.4,
    box.padding = 0.6,
    min.segment.length = 0.2,
    seed = 42
  ) +
  geom_text_repel(
    data = filter(df_labels, highlight_type == "below"),
    aes(label = label),
    family = "source_sans",
    size = 2.4,
    color = colors$palette$col_text_mid,   
    fontface = "plain",
    hjust = 1,
    direction = "y",
    nudge_x = -40,
    segment.color = colors$palette$col_text_mute,
    segment.size  = 0.25,
    segment.alpha = 0.4,
    box.padding   = 0.4,
    min.segment.length = 0.2
  ) +
  
  # Annotate
  annotate(
    "text",
    x = 0,
    y = nrow(df_dev) * 0.50,
    label = "Most countries\ncluster here",
    family = "source_sans",
    size = 3.0,
    color = colors$palette$col_text_mid,
    hjust = 0.5,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = 230,
    y = nrow(df_dev) * 0.76,
    label = "Long right tail driven\nby coal-dependent systems",
    family = "source_sans",
    size = 2.9,
    color = col_above,
    hjust = 0,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = -330,
    y = nrow(df_dev) * 0.14,
    label = "A small group of low-carbon\nsystems sits well below the norm",
    family = "source_sans",
    size = 2.8,
    color = colors$palette$col_text_mid,
    hjust = 0.5,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = 4,
    y = nrow(df_dev) + 2,
    label = "Global median\n(typical country)",
    family = "source_sans",
    size = 2.7,
    color = colors$palette$col_zero,
    hjust = 0,
    vjust = 1,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = min(df_dev$deviation) * 0.95,
    y = -3,
    label = "\u2190 cleaner",
    family = "source_sans",
    size = 2.7,
    color = colors$palette$col_text_mute,
    hjust = 0,
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = max(df_dev$deviation) * 0.82,   
    y = -3,
    label = "dirtier \u2192",
    family = "source_sans",
    size = 2.7,
    color = col_above,
    hjust = 1,
    fontface = "italic"
  ) +
  
  # Scales
  scale_x_continuous(
    labels = function(x) ifelse(x > 0, glue("+{x}"), as.character(x)),
    breaks = c(-600, -400, -200, 0, 200, 400, 600, 800)
  ) +
  
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x  = "Deviation from global median carbon intensity (gCO₂/kWh)"
  ) +
  
  # Theme
  theme_fd


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
# date     2026-03-29
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────
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
# ───────────────────────────────────────────────────────────────────────
