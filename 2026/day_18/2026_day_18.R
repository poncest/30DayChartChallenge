
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 18 · Relationships | Data Day: UNICEF
##
## Topic:      As child mortality falls, the remaining risk concentrates at birth
## Author:     Steven Ponce
## Date:       2026-04-18
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data:       UN Inter-agency Group for Child Mortality Estimation (UN IGME)
##             via UNICEF. "Child Mortality Estimates 2024" dataset.
##             Source: https://data.unicef.org/resources/dataset/child-mortality/
##             Files used:
##             Under-five_Mortality_Rates_2024.xlsx  → "U5MR Regional estimates"
##             Neonatal_Mortality_Rates_2024.xlsx    → "NMR Regional estimates"
##             Last updated: March 2025 | License: CC BY 3.0 IGO


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,
  scales, glue, camcorder, readxl, here,
  patchwork
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
path_u5mr <- here("2026", "data", "Under-five_Mortality_Rates_2024.xlsx")
path_nmr  <- here("2026", "data", "Neonatal_Mortality_Rates_2024.xlsx")

# Helper: read one regional sheet from an IGME xlsx and return a long tibble
read_igme_sheet <- function(path, sheet, indicator_label, skip = 15) {
  read_excel(path, sheet = sheet, skip = skip) |>
    clean_names() |>
    filter(uncertainty_bounds == "Median") |>
    select(region_name, starts_with("x")) |>
    pivot_longer(
      cols      = starts_with("x"),
      names_to  = "year_raw",
      values_to = "rate"
    ) |>
    mutate(
      year      = as.integer(str_extract(year_raw, "\\d{4}")),
      rate      = as.numeric(rate),
      indicator = indicator_label
    ) |>
    filter(!is.na(rate), year >= 1990, year <= 2023) |>
    select(region_name, year, rate, indicator) |>
    distinct(region_name, year, indicator, .keep_all = TRUE)
}

df_u5mr <- read_igme_sheet(path_u5mr, "U5MR Regional estimates", "Under-Five (U5MR)")
df_nmr <- read_igme_sheet(path_nmr, "NMR Regional estimates", "Neonatal (NMR)")

df_all <- bind_rows(df_u5mr, df_nmr)


## 3. EXAMINING THE DATA ----
glimpse(df_all)
df_all |> distinct(region_name) |> arrange(region_name) |> print(n = 30)
df_all |> filter(year %in% c(1990, 2023)) |> arrange(indicator, region_name, year)


## 4. TIDY DATA ----

# Regions to include — one global anchor + four regional contrasts
regions_keep <- c(
  "World",
  "Sub-Saharan Africa",
  "South Asia",
  "Latin America and the Caribbean",
  "East Asia and the Pacific"
)

# Dumbbell endpoints: 1990 and 2023
df_dumbbell <- df_all |>
  filter(
    region_name %in% regions_keep,
    year %in% c(1990, 2023)
  ) |>
  mutate(
    year_label = if_else(year == 1990, "y1990", "y2023"),
    # Short region names for axis labels
    region_short = case_when(
      region_name == "World" ~ "World",
      region_name == "Sub-Saharan Africa" ~ "Sub-Saharan\nAfrica",
      region_name == "South Asia" ~ "South Asia",
      str_detect(region_name, "Latin America") ~ "Latin America\n& Caribbean",
      str_detect(region_name, "East Asia") ~ "East Asia\n& Pacific",
      TRUE ~ region_name
    )
  ) |>
  pivot_wider(
    id_cols = c(region_name, region_short, indicator),
    names_from = year_label,
    values_from = rate
  ) |>
  mutate(
    decline_abs  = y1990 - y2023,
    decline_pct  = (y1990 - y2023) / y1990
  )

# Sort order
region_order <- df_dumbbell |>
  filter(indicator == "Under-Five (U5MR)") |>
  arrange(y2023) |>
  pull(region_short)

df_dumbbell <- df_dumbbell |>
  mutate(region_short = factor(region_short, levels = region_order))

# Neonatal share annotation
nmr_world_2023 <- df_dumbbell |>
  filter(indicator == "Neonatal (NMR)", region_name == "World") |>
  pull(y2023)

u5mr_world_2023 <- df_dumbbell |>
  filter(indicator == "Under-Five (U5MR)", region_name == "World") |>
  pull(y2023)

nmr_share_2023 <- round(nmr_world_2023 / u5mr_world_2023 * 100)

nmr_world_1990 <- df_dumbbell |>
  filter(indicator == "Neonatal (NMR)", region_name == "World") |>
  pull(y1990)

u5mr_world_1990 <- df_dumbbell |>
  filter(indicator == "Under-Five (U5MR)", region_name == "World") |>
  pull(y1990)

nmr_share_1990 <- round(nmr_world_1990 / u5mr_world_1990 * 100)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(palette = list(
  "u5mr"    = "#4a90b8",   
  "nmr"     = "#1a3a5c",   
  "segment" = "#d0dde6",  
  "dot_1990"= "#7a9bb5",  
  "text"    = "#1e2d3d",
  "subtle"  = "#7a8a99"
))

col_u5mr     <- colors$palette$u5mr
col_nmr      <- colors$palette$nmr
col_segment  <- colors$palette$segment
col_dot_1990 <- colors$palette$dot_1990
col_text     <- colors$palette$text
col_subtle   <- colors$palette$subtle

### |- titles and caption ----
title_text    <- "As child mortality falls, risk concentrates at birth"

subtitle_text <- paste0(
  "Regional mortality rates declined sharply from 1990 to 2023, but neonatal deaths fell ",
  "less than under-five deaths — shifting risk toward the earliest days of life.<br>",
  "Each dumbbell shows the change from ",
  "<span style='color:", col_dot_1990, "; font-weight:600'>1990 (open circle)</span>",
  " to ",
  "<span style='color:", col_nmr, "; font-weight:600'>2023 (filled)</span>",
  " by region. Rates per 1,000 live births."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 18,
  source_text = "UN IGME via UNICEF · Child Mortality Estimates 2024 (data.unicef.org)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base and weekly theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = fonts$title, face = "bold", size = 20,
      color = col_text, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 10, color = "#4a5568",
      margin = margin(b = 16), lineheight = 1.4
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = col_subtle,
      margin = margin(t = 12), hjust = 0
    ),
    legend.position = "none",
    plot.margin = margin(16, 16, 10, 16)
  )
)

theme_set(weekly_theme)

### |- dumbbell builder function ----
build_dumbbell_panel <- function(data, indicator_name, col_2023, panel_title, panel_subtitle) {
  d <- data |> filter(indicator == indicator_name)

  ggplot(d, aes(y = region_short)) +

    # Connector segment (1990 → 2023)
    geom_segment(
      aes(x = y2023, xend = y1990, yend = region_short),
      color = col_segment,
      linewidth = 1.8,
      lineend = "round"
    ) +

    # 1990 endpoint (muted)
    geom_point(
      aes(x = y1990),
      color = col_dot_1990,
      size = 3.5,
      shape = 21,
      fill = "white",
      stroke = 1.2
    ) +

    # 2023 endpoint (accent)
    geom_point(
      aes(x = y2023),
      color = col_2023,
      size = 4.5,
      shape = 21,
      fill = col_2023,
      stroke = 0
    ) +

    # 2023 rate label (right of dot)
    geom_text(
      aes(x = y2023, label = round(y2023, 1)),
      hjust = -0.5,
      size = 2.8,
      color = col_text,
      family = fonts$text
    ) +
    labs(
      title = panel_title,
      subtitle = panel_subtitle,
      x = NULL, y = NULL
    ) +
    theme(
      plot.title = element_markdown(
        family = fonts$title, face = "bold", size = 11,
        color = col_2023, margin = margin(b = 2)
      ),
      plot.subtitle = element_markdown(
        family = fonts$text, size = 8.5, color = col_subtle,
        margin = margin(b = 10), lineheight = 1.3
      ),
      axis.text.y = element_text(
        family = fonts$text, size = 8.5, color = col_text,
        lineheight = 1.2
      ),
      axis.text.x = element_text(
        family = fonts$text, size = 7.5, color = col_subtle
      ),
      panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 24, 8, 8)
    )
}

### |- individual plots ----
p_u5mr <- build_dumbbell_panel(
  data           = df_dumbbell,
  indicator_name = "Under-Five (U5MR)",
  col_2023       = col_u5mr,
  panel_title    = "Under-Five Mortality Rate",
  panel_subtitle = "Deaths per 1,000 live births, children under age 5"
)

p_nmr <- build_dumbbell_panel(
  data           = df_dumbbell,
  indicator_name = "Neonatal (NMR)",
  col_2023       = col_nmr,
  panel_title    = "Neonatal Mortality Rate",
  panel_subtitle = "Deaths per 1,000 live births, first 28 days of life"
) +

  annotate(
    "text",
    x = Inf, y = -Inf,
    label = glue::glue(
      "Globally, neonatal deaths rose from {nmr_share_1990}% to {nmr_share_2023}%\n",
      "of all under-five deaths (1990 to 2023)"
    ),
    hjust = 1.05, vjust = -0.6,
    size = 3.0, color = col_nmr,
    family = fonts$text,
    lineheight = 1.3,
    fontface = "italic"
  )

### |- combined plots ----
p <- (p_u5mr | p_nmr) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        family = fonts$title, face = "bold", size = 20,
        color = col_text, margin = margin(b = 6)
      ),
      plot.subtitle = element_markdown(
        family = fonts$text, size = 9.5, color = "#4a5568",
        margin = margin(b = 4), lineheight = 1.4
      ),
      plot.caption = element_markdown(
        family = fonts$text, size = 6.5, color = col_subtle,
        margin = margin(t = 10), hjust = 0
      ),
      plot.margin = margin(16, 16, 10, 16)
    )
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
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.3)
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
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
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
