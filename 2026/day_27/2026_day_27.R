
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 27 | Uncertainties | Animation

## Topic:      Energy Transitions Are Not Smooth
## Author:     Steven Ponce
## Date:       2026-04-27
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data source:
##   OWID Energy Data
##   Cache: 2026/data/owid-energy-data.csv


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
  height = 7,
  units  = "in",
  dpi    = 320
)

source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

### |- observed CO₂: read from local cache ----
raw_data <- read_csv(
  here::here("2026/data/owid-energy-data.csv"),
  show_col_types = FALSE
)


## 3. EXAMINING THE DATA ----
glimpse(raw_data)


## 4. TIDY DATA ----

selected_countries <- c(
  "Denmark",
  "Brazil",
  "Germany",
  "Spain",
  "Australia"
)

# Archetype labels —
archetype_map <- tribble(
  ~country,    ~archetype,                ~profile,
  "Denmark",   "Wind-driven volatility",  "moderate",
  "Brazil",    "Gradual mover",           "smooth",
  "Germany",   "Managed transition",      "moderate",
  "Spain",     "Boom & reversal",         "volatile",
  "Australia", "Political whipsaw",       "volatile"
)

### |- filter and compute delta ----
df_clean <- raw_data |>
  filter(
    country %in% selected_countries,
    year >= 1991,          
    year <= 2023
  ) |>
  select(country, year, renewables_share_elec) |>
  mutate(renewables_share_elec = as.numeric(renewables_share_elec)) |>
  filter(!is.na(renewables_share_elec)) |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(
    delta_renew = renewables_share_elec - lag(renewables_share_elec)
  ) |>
  filter(!is.na(delta_renew)) |>
  ungroup()

### |- join archetype metadata ----
df_plot <- df_clean |>
  left_join(archetype_map, by = "country") |>
  mutate(
    country = factor(country, levels = c(
      "Denmark", "Brazil", "Germany", "Spain", "Australia"
    )),
    # direction for coloring bars/fill
    direction = if_else(delta_renew >= 0, "positive", "negative")
  )

### |- per-country volatility stats (for annotation) ----
df_stats <- df_plot |>
  group_by(country, archetype, profile) |>
  summarise(
    sd_delta  = round(sd(delta_renew, na.rm = TRUE), 1),
    mean_delta = round(mean(delta_renew, na.rm = TRUE), 1),
    n_negative = sum(delta_renew < 0),
    .groups = "drop"
  )

### |- strip label: country + archetype ----
strip_labels <- archetype_map |>
  mutate(label = paste0(country, "\n", archetype)) |>
  select(country, label) |>
  deframe()

### |- volatility annotation per panel ----
df_annot <- df_stats |>
  mutate(
    label = paste0("SD = ", sd_col <- sd_delta, " pp"),
    x_pos = 2022,
    y_pos = Inf
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "background" = "#F5F3EE",
    "text"       = "#2C2C2C",
    "subtext"    = "#5C5C5C",
    "grid"       = "#E8E5DF",
    "positive"   = "#2E6B9E",  
    "negative"   = "#8B3030",   
    "zero_line"  = "#9A9591"     
  )
)

bg_col       <- colors$palette$background
text_col     <- colors$palette$text
sub_col      <- colors$palette$subtext
grid_col     <- colors$palette$grid
pos_col      <- colors$palette$positive
neg_col      <- colors$palette$negative
zero_col     <- colors$palette$zero_line

### |- titles and caption ----
title_text    <- "Energy Transitions Are Not Smooth"

subtitle_text <- paste0(
  "Year-over-year change in renewable electricity share (percentage points), 1991–2023.<br>",
  "Frequent reversals and uneven gains reveal how unpredictable the path to clean energy can be."
)
caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 27,
  source_text = "Our World in Data | Energy Mix"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),

    # minimal grid — only horizontal reference
    panel.grid.major.y = element_line(color = grid_col, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # facet strips — country name + archetype label
    strip.background = element_rect(fill = bg_col, color = NA),
    strip.text = element_text(
      family = fonts$text,
      face = "bold",
      size = 9,
      color = text_col,
      hjust = 0,
      margin = margin(b = 4)
    ),

    # axes
    axis.title = element_text(color = sub_col, size = 9, family = fonts$text),
    axis.text.x = element_text(color = sub_col, size = 7.5, family = fonts$text),
    axis.text.y = element_text(color = sub_col, size = 7.5, family = fonts$text),
    axis.ticks = element_blank(),

    # panel spacing
    panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(
      family = fonts$title, face = "bold",
      size = 18, color = text_col,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 9.5, color = sub_col,
      lineheight = 1.45, margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = sub_col,
      hjust = 0, margin = margin(t = 12)
    ),
    plot.margin = margin(20, 20, 14, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(df_plot, aes(x = year, y = delta_renew)) +

  # Geoms
  geom_hline(
    yintercept = 0,
    color      = zero_col,
    linewidth  = 0.6,
    linetype   = "solid"
  ) +
  geom_col(
    aes(fill = direction),
    width = 0.75
  ) +

  # Facet
  facet_wrap(
    ~country,
    nrow     = 1,
    labeller = labeller(country = strip_labels)
  ) +

  # Scales
  scale_fill_manual(
    values = c("positive" = pos_col, "negative" = neg_col)
  ) +
  scale_x_continuous(
    breaks = c(2000, 2010, 2020),
    labels = c("2000", "'10", "'20")
  ) +
  scale_y_continuous(
    name   = "Δ Renewable share (pp)",
    labels = label_number(suffix = "pp", style_positive = "plus")
  ) +
  guides(fill = "none") +

  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = NULL
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