
## Challenge: #30DayChartChallenge 2026 — Day 02
## Prompt:    Pictogram
## Topic:     Electricity generation by source — 8 countries, 2023
## Author:    Steven Ponce
## Date:      2026-04-02

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data:      Our World in Data — Electricity Mix (Ember / Energy Institute, 2023)
##            https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, sysfonts, systemfonts,    
  janitor, scales, glue, patchwork          
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

### |- load FontAwesome ----
fa_path <- system_fonts() |>
  filter(name == "FontAwesome6Free-Solid") |>
  pull(path) |>
  first()

font_add("FontAwesome6", regular = fa_path)
message("FontAwesome loaded: ", fa_path)

showtext_auto()
showtext_opts(dpi = 320)


## 2. READ IN THE DATA ----

### |- OWID energy data ----
# Download: https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
energy_raw <- read_csv("2026/data/owid-energy-data.csv")


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)


## 4. TIDY DATA ----

### |- target countries ----
target_countries <- c(
  "Norway", "Brazil", "Denmark", "France",
  "China", "United States", "India", "Saudi Arabia"
)

### |- column -> source lookup ----
col_source_map <- c(
  "coal_electricity"                        = "coal",
  "gas_electricity"                         = "gas",
  "oil_electricity"                         = "oil",
  "nuclear_electricity"                     = "nuclear",
  "hydro_electricity"                       = "hydro",
  "wind_electricity"                        = "wind",
  "solar_electricity"                       = "solar",
  "biofuel_electricity"                     = "bioenergy",
  "other_renewable_exc_biofuel_electricity" = "other_renewables"
  # NOTE: other_renewables retained for data completeness;
  # excluded from legend since none of the 8 countries use it in 2023
)

### |- sort orders (defined once, used throughout) ----
# Within-row display: renewables block | nuclear | fossil block
source_sort_order <- c(
  "hydro"            = 1,
  "wind"             = 2,
  "solar"            = 3,
  "bioenergy"        = 4,
  "other_renewables" = 5,
  "nuclear"          = 6,
  "gas"              = 7,
  "oil"              = 8,
  "coal"             = 9
)

# Legend order: same clean -> fossil sequence, other_renewables excluded
legend_order <- c(
  "hydro", "wind", "solar", "bioenergy",
  "nuclear", "gas", "oil", "coal"
)

### |- source group lookup ----
source_group_map <- c(
  "hydro"            = "renewable",
  "wind"             = "renewable",
  "solar"            = "renewable",
  "bioenergy"        = "renewable",
  "other_renewables" = "renewable",
  "nuclear"          = "nuclear",
  "gas"              = "fossil",
  "oil"              = "fossil",
  "coal"             = "fossil"
)

### |- icon unicode (FontAwesome 6 Solid) ----
icon_map <- c(
  "hydro"            = "\uf043", # fa-droplet
  "wind"             = "\uf72e", # fa-wind
  "solar"            = "\uf185", # fa-sun
  "bioenergy"        = "\uf06c", # fa-leaf
  "other_renewables" = "\uf0e7", # fa-bolt
  "nuclear"          = "\uf5d2", # fa-atom
  "gas"              = "\uf06d", # fa-fire
  "oil"              = "\uf1e6", # fa-plug
  "coal"             = "\uf275" # fa-industry
)

### |- display labels ----
source_labels <- c(
  "hydro"            = "Hydro",
  "wind"             = "Wind",
  "solar"            = "Solar",
  "bioenergy"        = "Bioenergy",
  "other_renewables" = "Other Renew.",
  "nuclear"          = "Nuclear",
  "gas"              = "Natural Gas",
  "oil"              = "Oil",
  "coal"             = "Coal"
)

### |- largest-remainder allocation: icons always sum to exactly 10 ----
allocate_icons <- function(shares, n_total = 10) {
  shares <- tidyr::replace_na(shares, 0)

  if (sum(shares) == 0) {
    return(rep(0L, length(shares)))
  }

  shares <- shares / sum(shares)

  floors <- floor(shares * n_total)
  remainder <- shares * n_total - floors
  n_remaining <- as.integer(n_total - sum(floors))

  if (n_remaining > 0) {
    top_idx <- order(remainder, decreasing = TRUE)[seq_len(n_remaining)]
    floors[top_idx] <- floors[top_idx] + 1L
  }

  floors
}

### |- filter, reshape, compute shares + icon counts ----
energy_clean <- energy_raw |>
  filter(country %in% target_countries, year == 2023) |>
  select(country, year, all_of(names(col_source_map))) |>
  pivot_longer(
    cols      = all_of(names(col_source_map)),
    names_to  = "col",
    values_to = "twh"
  ) |>
  mutate(
    source = col_source_map[col],
    twh    = replace_na(twh, 0)
  ) |>
  select(-col) |>
  group_by(country) |>
  mutate(
    total_twh = sum(twh, na.rm = TRUE),
    share     = if_else(total_twh > 0, twh / total_twh, 0),
    n_icons   = allocate_icons(share)
  ) |>
  ungroup() |>
  mutate(
    source = factor(source, levels = names(source_sort_order))
  ) |>
  # Keep only sources that receive at least one pictogram icon
  # after largest-remainder allocation
  filter(n_icons > 0)

### |- sanity check: every country must sum to exactly 10 icons ----
icon_check <- energy_clean |>
  group_by(country) |>
  summarise(total_icons = sum(n_icons), .groups = "drop")

stopifnot(all(icon_check$total_icons == 10))

### |- country order: cleanest -> most fossil-heavy ----
# left_join retains countries with zero fossil icons (e.g. Norway)
country_order <- energy_clean |>
  distinct(country) |>
  left_join(
    energy_clean |>
      filter(source %in% c("coal", "gas", "oil")) |>
      group_by(country) |>
      summarise(fossil_icons = sum(n_icons), .groups = "drop"),
    by = "country"
  ) |>
  mutate(fossil_icons = replace_na(fossil_icons, 0)) |>
  arrange(fossil_icons, country) |>
  pull(country)

### |- expand to one row per icon, sorted within each row ----
energy_expanded <- energy_clean |>
  uncount(n_icons) |>
  mutate(sort_key = source_sort_order[as.character(source)]) |>
  group_by(country) |>
  arrange(sort_key, .by_group = TRUE) |>
  mutate(
    icon_pos = row_number(),
    x        = icon_pos,
    y        = 0,
    icon     = icon_map[as.character(source)]
  ) |>
  ungroup() |>
  mutate(
    country = factor(country, levels = country_order),
    source  = factor(as.character(source), levels = legend_order)
  )

### |- right-side annotations (selected countries only) ----
annotation_df <- energy_clean |>
  mutate(group = source_group_map[as.character(source)]) |>
  group_by(country, group) |>
  summarise(pct = round(sum(share) * 100), .groups = "drop") |>
  pivot_wider(names_from = group, values_from = pct, values_fill = 0) |>
  mutate(
    annotation = case_when(
      country == "Norway" ~ glue("{renewable}% renewable"),
      country == "France" ~ glue("{nuclear}% nuclear electricity"),
      country %in% c("India", "Saudi Arabia") ~ glue("{fossil}% fossil fuels"),
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(annotation)) |>
  mutate(country = factor(country, levels = country_order))

### |- legend data ----
legend_df <- tibble(
  source = legend_order,
  label  = source_labels[legend_order],
  icon   = icon_map[legend_order]
) |>
  mutate(
    col = c(1, 2, 3, 4, 1, 2, 3, 4),
    row = c(2, 2, 2, 2, 1, 1, 1, 1)
  ) |>
  mutate(
    x_icon  = (col - 1) * 2.2 + 1,
    x_label = x_icon + 0.55,
    y       = row
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "coal"             = "#525252",
    "gas"              = "#E67E22",
    "oil"              = "#1a1a2e",
    "nuclear"          = "#7A1E3A",
    "hydro"            = "#4A90D9",
    "wind"             = "#27AE60",
    "solar"            = "#F39C12",
    "bioenergy"        = "#52b788",
    "other_renewables" = "#1ABC9C"
  )
)

### |- titles and caption ----
title_text <- "Norway runs on water. India runs on coal.\nElectricity generation, shown in 10 icons."

subtitle_text <- str_glue(
  "Each icon represents 10% of electricity generation (2023).\n",
  "Countries ordered from cleanest to most fossil-heavy."
)

caption_text <- create_dcc_caption(
  dcc_year = 2026,
  dcc_day = 02,
  source_text = "Our World in Data · Energy Institute Statistical Review of World Energy"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    strip.text.y.left = element_text(
      angle = 0, hjust = 1, vjust = 0.5,
      face = "bold", size = rel(1.0),
      margin = margin(r = 6, l = 0)
    ),
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.75),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 15)
    ),
    strip.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.y = unit(0.3, "lines"),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p_main <- energy_expanded |>
  ggplot(aes(x = x, y = y, color = source, label = icon)) +
  geom_text(
    family      = "FontAwesome6",
    size        = 8,
    show.legend = FALSE
  ) +
  geom_text(
    data = annotation_df,
    aes(x = 11.2, y = 0, label = annotation),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    color = "gray30",
    fontface = "bold",
    family = "sans"
  ) +
  scale_color_manual(values = colors$palette) +
  scale_x_continuous(limits = c(0.5, 14.5)) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  coord_equal() +
  facet_wrap(
    ~country,
    ncol           = 1,
    strip.position = "left"
  ) +
  labs(
    title    = title_text,
    subtitle = subtitle_text
  )

### |- custom legend as its own ggplot ----
p_legend <- ggplot(legend_df) +
  geom_text(
    aes(x = x_icon, y = y, label = icon, color = source),
    family = "FontAwesome6",
    size = 5
  ) +
  geom_text(
    aes(x = x_label, y = y, label = label),
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    color = "gray20",
    family = "sans"
  ) +
  scale_color_manual(values = colors$palette) +
  scale_x_continuous(limits = c(0.5, 11.5)) +
  scale_y_continuous(limits = c(0.4, 2.6)) +
  theme_void() +
  theme(
    legend.position  = "none",
    plot.background  = element_rect(fill = colors$background, color = colors$background),
    panel.background = element_rect(fill = colors$background, color = colors$background),
    plot.margin      = margin(t = 0, r = 0, b = 0, l = 0)
  )

### |- caption panel ----
p_caption <- ggplot() +
  labs(caption = caption_text) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = colors$background, color = colors$background),
    panel.background = element_rect(fill = colors$background, color = colors$background),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color = colors$caption,
      lineheight = 1.1,
      hjust = 0,
      halign = 0,
      margin = margin(t = 5, b = 5)
    )
  )

### |- combine with patchwork ----
combined_plots <- p_main / p_legend / p_caption +
  plot_layout(heights = c(8, 1.2, 0.35)) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(15, 15, 10, 15)
    )
  )

snap(combined_plots)


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
# date     2026-03-19
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmp29rdWK/file8528583a2f06". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────
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
# systemfonts  * 1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
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
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────