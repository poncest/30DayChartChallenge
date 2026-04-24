## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 24 · Timeseries | Theme Day: South China Morning Post (SCMP)
## Topic:      China's energy pivot is rewriting the power equation
## Author:     Steven Ponce
## Date:       2026-04-24
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers: R/utils/fonts.R · R/utils/social_icons.R
##                R/themes/base_theme.R · R/utils/snap.R
##
## Data:       Our World in Data — Energy Dataset
##             https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
##             Cached: 2026/data/owid-energy-data.csv
##
## Key columns used:
##   coal_share_elec          — coal as % of total electricity generation
##   renewables_share_elec    — all renewables (incl. hydro) as % of total
##   solar_share_elec         — solar as % of total
##   wind_share_elec          — wind as % of total
##
## NOTE on series choice:
##   renewables_share_elec includes legacy hydro (~15–18% of China's mix) which
##   inflates the renewable share and makes the crossing happen earlier.
##   solar_share_elec + wind_share_elec = "new renewables" — a cleaner argument
##   about the *new* build. Examine Section 3 to decide which serves the story.
##   Final choice: renewables_share_elec (hydro-inclusive) — defensible because
##   hydro IS renewable; SCMP angle is about scale of pivot, not decomposition.


## 1. LOAD PACKAGES & SETUP ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,
  scales, glue, camcorder, here
)

camcorder::gg_record(
  dir    = here("temp_plots"),
  device = "png",
  width  = 6,
  height = 10,
  units  = "in",
  dpi    = 320
)

source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
energy_raw <- read_csv(
  here("2026/data/owid-energy-data.csv"),
  show_col_types = FALSE
)

## 3. EXAMINING THE DATA ----
glimpse(energy_raw)

### |- compare hydro-inclusive vs. new-renewables (solar + wind) ----
energy_raw |>
  filter(country == "China", year >= 2000) |>
  select(
    year,
    coal_share_elec,
    renewables_share_elec,
    solar_share_elec,
    wind_share_elec
  ) |>
  mutate(solar_wind = solar_share_elec + wind_share_elec) |>
  filter(year %in% c(2000, 2005, 2010, 2015, 2020, 2023)) |>
  select(year, coal_share_elec, renewables_share_elec, solar_wind)


## 4. TIDY DATA ----

### |- filter China, 2000–2023, pivot long ----
china <- energy_raw |>
  filter(country == "China", year >= 2000, year <= 2023) |>
  select(
    year,
    coal      = coal_share_elec,
    renewable = renewables_share_elec
  ) |>
  filter(!is.na(coal), !is.na(renewable)) |>
  pivot_longer(
    cols      = c(coal, renewable),
    names_to  = "source",
    values_to = "share"
  ) |>
  mutate(
    source = factor(source, levels = c("renewable", "coal"))
  )

### |- confirm 2023 endpoint values ----
val_coal_2023 <- china |>
  filter(year == 2023, source == "coal") |>
  pull(share) |>
  round(1)

val_renew_2023 <- china |>
  filter(year == 2023, source == "renewable") |>
  pull(share) |>
  round(1)

### |- find coal peak year ----
coal_peak <- china |>
  filter(source == "coal") |>
  slice_max(share, n = 1)

coal_peak_year <- coal_peak$year
coal_peak_val <- round(coal_peak$share, 1)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(palette = list(
  bg_color = "#0D1B2A",
  solar_gold = "#F4A932",
  coal_blue = "#6A80A0",
  text_white = "#FFFFFF",
  text_muted = "#8A98A8",
  text_sub = "#B0B8C4",
  grid_color = "#1A2E42",
  accent_coral = "#E8503A"
))

bg_color <- colors$palette$bg_color
solar_gold <- colors$palette$solar_gold
coal_blue <- colors$palette$coal_blue
text_white <- colors$palette$text_white
text_muted <- colors$palette$text_muted
text_sub <- colors$palette$text_sub
grid_color <- colors$palette$grid_color
accent_coral <- colors$palette$accent_coral
col_renew <- solar_gold
col_coal <- coal_blue


### |- titles and caption ----
title_text <- glue(
  "<span style='font-weight:900; color:{coal_blue};'>Coal falls.&nbsp;</span>",
  "<span style='font-weight:900; color:{solar_gold};'> Renewables rise.</span>"
)

subtitle_text <- glue(
  "<span style='font-size:12pt; font-weight:400; color:{text_sub};'>",
  "China's energy pivot is rewriting the power equation<br>",
  "Share of electricity generation · 2000–2023",
  "</span>"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 24,
  source_text = "Our World in Data · OWID Energy Dataset (BP / Ember)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- main plot ----
p <- china |>
  ggplot(aes(x = year, y = share, color = source, group = source)) +

  # Geom
  geom_hline(
    yintercept = seq(0, 80, by = 20),
    color = grid_color,
    linewidth = 0.25
  ) +
  geom_line(
    data = china |> filter(source == "renewable"),
    linewidth = 1.9,
    alpha = 1.0
  ) +
  geom_line(
    data = china |> filter(source == "coal"),
    linewidth = 1.3,
    alpha = 0.60
  ) +
  geom_point(
    data = china |> filter(year == 2000),
    size = 3.5,
    shape = 21,
    fill = bg_color,
    stroke = 1.8
  ) +
  geom_point(
    data = china |> filter(year == 2023),
    size = 3.5,
    shape = 21,
    fill = bg_color,
    stroke = 1.8
  ) +

  # Annotate
  annotate(
    "segment",
    x = coal_peak_year, xend = coal_peak_year,
    y = 0, yend = coal_peak_val - 3,
    color = accent_coral,
    linewidth = 0.5,
    linetype = "dotted"
  ) +
  annotate(
    "text",
    x = coal_peak_year - 0.3,
    y = coal_peak_val - 5,
    label = glue("Coal peak\n{coal_peak_year}: {coal_peak_val}%"),
    hjust = 1,
    size = 2.9,
    color = accent_coral,
    family = fonts$text,
    lineheight = 1.1
  ) +
  annotate(
    "segment",
    x = 2015, xend = 2015,
    y = 0, yend = 30,
    color = "#D8E4EE",
    linewidth = 0.45,
    linetype = "dotted"
  ) +
  annotate(
    "text",
    x = 2015.4,
    y = 32,
    label = "2015: National\nsolar targets set",
    hjust = 0,
    size = 2.9,
    color = "#D8E4EE",
    family = fonts$text,
    lineheight = 1.1
  ) +
  annotate(
    "text",
    x = 2019.8,
    y = 44,
    label = "2023: China installs more solar\nthan the US has ever built",
    hjust = 0,
    vjust = 0,
    size = 3.0,
    color = solar_gold,
    family = fonts$text,
    lineheight = 1.2,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2023.6,
    y = c(val_renew_2023, val_coal_2023),
    label = c(
      glue("Renewables\n{val_renew_2023}%"),
      glue("Coal\n{val_coal_2023}%")
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.2,
    color = c(col_renew, col_coal),
    family = fonts$text,
    lineheight = 1.15
  ) +

  # Scales
  scale_color_manual(
    values = c("renewable" = col_renew, "coal" = col_coal)
  ) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020, 2023),
    expand = expansion(mult = c(0.03, 0.24))
  ) +
  scale_y_continuous(
    limits = c(0, 90),
    breaks = seq(0, 80, by = 20),
    labels = \(x) glue("{x}%"),
    expand = expansion(mult = c(0.01, 0.04))
  ) +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL
  ) +

  # Theme
  theme_void() +
  theme(
    # canvas
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    plot.title = element_markdown(
      family = fonts$title,
      size = 28,
      color = text_white,
      hjust = 0,
      lineheight = 1.1,
      margin = margin(t = 10, b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text,
      size = 10,
      color = text_sub,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(b = 22)
    ),
    plot.caption = element_markdown(
      family = fonts$text,
      size = 6.5,
      color = "#5A6878",
      hjust = 1,
      margin = margin(t = 14)
    ),
    axis.text.x = element_text(
      family = fonts$text,
      size   = 9,
      color  = text_muted,
      margin = margin(t = 6)
    ),
    axis.text.y = element_text(
      family = fonts$text,
      size = 8,
      color = text_muted,
      hjust = 1
    ),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 16, b = 16, l = 20)
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
