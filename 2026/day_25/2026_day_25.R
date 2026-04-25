
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 25 | Uncertainties | Space

## Topic:      The Exoplanet Mass–Radius Fog
## Author:     Steven Ponce
## Date:       2026-04-25
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data:     NASA Exoplanet Archive — Planetary Systems Composite Parameters
##           TAP/SQL: https://exoplanetarchive.ipac.caltech.edu/TAP/sync


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
  height = 8,
  units  = "in",
  dpi    = 320
)

source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

### |- cache path ----
cache_path <- here("2026", "data", "exoplanets_pscomppars.csv")

exo_raw <- read_csv(cache_path, show_col_types = FALSE, na = c("", "NA"))

# --- Rebuild cache (uncomment if re-downloading from NASA Exoplanet Archive) ---
# Requires: httr2
# Source: NASA Exoplanet Archive — Planetary Systems Composite Parameters (pscomppars)
# TAP endpoint: https://exoplanetarchive.ipac.caltech.edu/TAP/sync
# Columns used:
#   pl_name         — planet name
#   pl_rade         — radius (Earth radii)
#   pl_radeerr1/2   — upper/lower radius uncertainty; err2 stored as negative by NASA
#   pl_bmasse       — best mass estimate (Earth masses); uses pl_msinie when available
#   pl_bmasseerr1/2 — upper/lower mass uncertainty
#   discoverymethod — detection technique
#   disc_year       — year of confirmation
#
# query <- paste0(
#   "SELECT pl_name, pl_rade, pl_radeerr1, pl_radeerr2, ",
#   "pl_bmasse, pl_bmasseerr1, pl_bmasseerr2, ",
#   "discoverymethod, disc_year ",
#   "FROM pscomppars ",
#   "WHERE pl_rade IS NOT NULL ",
#   "AND pl_bmasse IS NOT NULL"
# )
# url <- paste0(
#   "https://exoplanetarchive.ipac.caltech.edu/TAP/sync?",
#   "query=", URLencode(query, reserved = TRUE),
#   "&format=csv"
# )
# resp    <- request(url) |> req_timeout(60) |> req_perform()
# exo_raw <- resp_body_string(resp) |> read_csv(show_col_types = FALSE, na = c("", "NA"))
# write_csv(exo_raw, cache_path)
# message("Cached to: ", cache_path)


## 3. EXAMINING THE DATA ----
glimpse(exo_raw)
cat("Detection methods:\n"); print(table(exo_raw$discoverymethod, useNA = "always"))


## 4. TIDY DATA ----

### |- clean, filter, classify ----
exo <- exo_raw |>
  mutate(across(
    c(
      pl_rade, pl_radeerr1, pl_radeerr2,
      pl_bmasse, pl_bmasseerr1, pl_bmasseerr2, disc_year
    ),
    as.numeric
  )) |>
  filter(!is.na(pl_rade), !is.na(pl_bmasse), pl_rade > 0, pl_bmasse > 0) |>
  # Remove extreme outliers (> ~3 Jupiter radii or > ~16 Jupiter masses)
  filter(pl_rade < 35, pl_bmasse < 6000) |>
  mutate(
    # NASA stores err2 as negative (signed lower bound) — take abs() for both
    rad_err_up = abs(pl_radeerr1),
    rad_err_dn = abs(pl_radeerr2),
    mass_err_up = abs(pl_bmasseerr1),
    mass_err_dn = abs(pl_bmasseerr2),
    has_rad_err = !is.na(rad_err_up) & !is.na(rad_err_dn),
    has_mass_err = !is.na(mass_err_up) & !is.na(mass_err_dn),
    # Compositional class using Fulton gap (~1.6 Re) + gas giant threshold (~4 Re)
    # These are inferred boundaries — the uncertainty cloud blurs them
    comp_class = case_when(
      pl_rade <= 1.6 ~ "Rocky",
      pl_rade <= 4.0 ~ "Sub-Neptune",
      TRUE ~ "Gas giant"
    ),
    comp_class = factor(comp_class, levels = c("Rocky", "Sub-Neptune", "Gas giant"))
  )

### |- subset with both error bars ----
exo_err <- exo |>
  filter(has_rad_err, has_mass_err) |>
  # Cap runaway bars on log scale
  mutate(
    rad_err_up  = pmin(rad_err_up, pl_rade * 2.0),
    rad_err_dn  = pmin(rad_err_dn, pl_rade * 0.85),
    mass_err_up = pmin(mass_err_up, pl_bmasse * 5.0),
    mass_err_dn = pmin(mass_err_dn, pl_bmasse * 0.85)
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg           = "#0A0F1A",
    rocky        = "#E8A87C",   
    subneptune   = "#7EB8D4",   
    gasgiant     = "#C4A4D4", 
    grid         = "#161E2E",
    text_main    = "#E8E8E8",
    text_sub     = "#8A94A0",
    annotation   = "#C0C8D0",
    earth_fill   = "#3366AA",
    earth_ring   = "#AACCFF",
    jupiter_fill = "#CC8833",
    jupiter_ring = "#FFD8A0",
    zone_label   = "#3D4A5A"
  )
)

col_bg <- colors$palette$bg
col_rocky <- colors$palette$rocky
col_subneptune <- colors$palette$subneptune
col_gasgiant <- colors$palette$gasgiant
col_grid <- colors$palette$grid
col_text_main <- colors$palette$text_main
col_text_sub <- colors$palette$text_sub
col_annotation <- colors$palette$annotation
col_earth_fill <- colors$palette$earth_fill
col_earth_ring <- colors$palette$earth_ring
col_jup_fill <- colors$palette$jupiter_fill
col_jup_ring <- colors$palette$jupiter_ring
col_zone <- colors$palette$zone_label

comp_colors <- c(
  "Rocky"       = col_rocky,
  "Sub-Neptune" = col_subneptune,
  "Gas giant"   = col_gasgiant
)

### |- titles and caption ----
title_text    <- "Most exoplanets are still a blur"

subtitle_text <- paste0(
  "Over 5,000 worlds confirmed, yet uncertainty in mass and radius ",
  "blurs the boundary between rocky worlds,<br>",
  "sub-Neptunes, and gas giants. ",
  "Color shows inferred class; error bars reveal how much we still don't know."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 25,
  source_text = "NASA Exoplanet Archive — Planetary Systems Composite Parameters (pscomppars)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = col_text_sub, family = fonts$text, size = 9),
    axis.title = element_text(color = col_text_sub, family = fonts$text, size = 10),
    axis.ticks = element_blank(),
    plot.title = element_markdown(
      color = col_text_main, family = fonts$title,
      size = 22, face = "bold", hjust = 0, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      color = col_text_sub, family = fonts$text,
      size = 10, lineheight = 1.5, hjust = 0, margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      color = col_text_sub, family = fonts$text,
      size = 7.5, hjust = 1, margin = margin(t = 14)
    ),
    plot.margin = margin(t = 20, r = 24, b = 12, l = 20),
    legend.position = "bottom",
    legend.background = element_rect(fill = col_bg, color = NA),
    legend.key = element_rect(fill = col_bg, color = NA),
    legend.text = element_text(color = col_text_sub, family = fonts$text, size = 8.5),
    legend.title = element_text(
      color = col_text_sub, family = fonts$text, size = 9, face = "bold"
    ),
    legend.key.width = unit(1.2, "lines"),
    legend.spacing.x = unit(0.4, "cm")
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot() +

  # Geoms
  geom_errorbar(
    data = exo_err,
    aes(
      x = pl_bmasse,
      ymin = pl_rade - rad_err_dn,
      ymax = pl_rade + rad_err_up,
      color = comp_class
    ),
    linewidth = 0.15, alpha = 0.12, width = 0
  ) +
  geom_errorbarh(
    data = exo_err,
    aes(
      y = pl_rade,
      xmin = pl_bmasse - mass_err_dn,
      xmax = pl_bmasse + mass_err_up,
      color = comp_class
    ),
    linewidth = 0.15, alpha = 0.12, height = 0
  ) +
  geom_point(
    data = exo,
    aes(x = pl_bmasse, y = pl_rade, color = comp_class),
    size = 0.85, alpha = 0.55, shape = 16
  ) +

  # --- Compositional boundary lines (subtle dashed) ---
  # Fulton gap: ~1.6 Re separates rocky from sub-Neptune
  geom_hline(yintercept = 1.6, color = col_zone, linewidth = 0.3, linetype = "dashed") +
  # Gas giant threshold: ~4 Re
  geom_hline(yintercept = 4.0, color = col_zone, linewidth = 0.3, linetype = "dashed") +

  # Annotate
  annotate("text",
    x = 0.38, y = 1.1,
    label = "Rocky", color = col_rocky, alpha = 0.55,
    family = fonts$text, size = 2.8, hjust = 0, fontface = "italic"
  ) +
  annotate("text",
    x = 0.38, y = 2.3,
    label = "Sub-Neptune", color = col_subneptune, alpha = 0.55,
    family = fonts$text, size = 2.8, hjust = 0, fontface = "italic"
  ) +
  annotate("text",
    x = 0.38, y = 7.0,
    label = "Gas giant", color = col_gasgiant, alpha = 0.55,
    family = fonts$text, size = 2.8, hjust = 0, fontface = "italic"
  ) +
  annotate("text",
    x = 10, y = 0.42,
    label = "Uncertainty spans entire\ncompositional classes\nfor many planets",
    color = col_annotation, family = fonts$text,
    size = 2.9, hjust = 0, lineheight = 1.3
  ) +
  annotate("segment",
    x = 20, xend = 25, y = 0.58, yend = 1.05,
    color = col_annotation, linewidth = 0.35, alpha = 0.6,
    arrow = arrow(length = unit(0.12, "cm"), type = "open")
  ) +
  annotate("point",
    x = 1, y = 1,
    color = col_earth_ring, size = 3.2,
    shape = 21, fill = col_earth_fill, stroke = 0.9
  ) +
  annotate("text",
    x = 1.5, y = 1.03,
    label = "Earth", color = col_earth_ring,
    family = fonts$text, size = 2.7, hjust = 0
  ) +
  annotate("text",
    x = 1.5, y = 0.88,
    label = "Well-constrained benchmark",
    color = col_text_sub, family = fonts$text,
    size = 2.3, hjust = 0, fontface = "italic"
  ) +
  annotate("point",
    x = 317.8, y = 11.2,
    color = col_jup_ring, size = 3.8,
    shape = 21, fill = col_jup_fill, stroke = 0.9
  ) +
  annotate("text",
    x = 430, y = 11.5,
    label = "Jupiter", color = col_jup_ring,
    family = fonts$text, size = 2.7, hjust = 0
  ) +
  annotate("text",
    x = 430, y = 9.5,
    label = "Even well-studied giants\nshow wide measurement ranges",
    color = col_text_sub, family = fonts$text,
    size = 2.3, hjust = 0, fontface = "italic", lineheight = 1.25
  ) +
  annotate("text",
    x = 3200, y = 1.72,
    label = "Boundaries are not discrete",
    color = col_zone, family = fonts$text,
    size = 2.2, hjust = 1, fontface = "italic"
  ) +

  # Scales
  scale_x_log10(
    name   = "Planet mass (Earth masses)",
    breaks = c(0.5, 1, 3, 10, 30, 100, 300, 1000, 3000),
    labels = label_number(accuracy = 1, big.mark = ","),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_y_log10(
    name   = "Planet radius (Earth radii)",
    breaks = c(0.5, 1, 2, 4, 8, 15, 25),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_color_manual(
    name = "Inferred composition",
    values = comp_colors,
    guide = guide_legend(
      nrow         = 1,
      override.aes = list(size = 3.5, alpha = 0.9)
    )
  ) +
  coord_cartesian(clip = "off") +

  # Labs
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
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────