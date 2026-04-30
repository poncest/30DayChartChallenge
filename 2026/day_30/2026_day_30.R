## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 30 | Uncertainties | Global Health Data Exchange (Data Day)

## Topic:      The Confidence Cascade — top 6 uncertain causes, normalized intervals
## Author:     Steven Ponce
## Date:       2026-04-30
##
## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/. See project README.
##
## Data source:
##   Global Burden of Disease Collaborative Network.
##   Global Burden of Disease Study 2021 (GBD 2021) Results.
##   Available from https://vizhub.healthdata.org/gbd-results/
##   (DALYs, Level 2 causes, Global, Both sexes, All ages, Year = 2021, Metric = Number)
##
### Download parameters:
###   GBD Estimate = Cause of death or injury
###   Measure      = DALYs | Metric = Number
###   Cause        = Level 2 causes (22 causes)
###   Location     = Global | Age = All ages | Sex = Both | Year = 2021

## 1. LOAD PACKAGES & SETUP ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,
  scales, glue, camcorder, ggdist
)

camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 7,
  units  = "in",
  dpi    = 320
)

source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

raw_gbd <- read_csv(
  here::here("2026/data/gbd_2021_dalys_level2_global.csv"),
  show_col_types = FALSE
) |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(raw_gbd)
raw_gbd |> count(cause_name)


## 4. TIDY DATA ----

gbd <- raw_gbd |>
  rename(cause = cause_name) |>
  select(cause, val, lower, upper) |>
  mutate(
    ui_width = upper - lower,
    rel_ui = ui_width / val * 100,
    # normalize: express as ratio of central estimate
    # dot always at 1.0; span = how far the CI reaches in either direction
    val_norm = 1.0,
    lower_norm = lower / val,
    upper_norm = upper / val,
    # retain billions for caption reference
    val_b = val / 1e9,
    lower_b = lower / 1e9,
    upper_b = upper / 1e9
  ) |>
  arrange(desc(rel_ui)) |>
  mutate(rank_ui = row_number())

# Top 6 by relative uncertainty
focus_gbd <- gbd |>
  slice_head(n = 6) |>
  mutate(
    cause_plot = factor(cause, levels = rev(cause)),
    label_text = paste0(round(rel_ui, 0), "% of estimate")
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg = "#F5F3EE",
    neutral = "#B0AAA2",
    accent = "#8B1A2A",
    text = "#2C2C2C",
    subtext = "#7A7570",
    grid = "#E4E0DA"
  )
)

# Extract scalars for annotate()
col_bg <- colors$palette$bg
col_subtext <- colors$palette$subtext
col_accent <- colors$palette$accent
col_grid <- colors$palette$grid
col_text <- colors$palette$text
col_neutral <- colors$palette$neutral

### |- titles and caption ----
title_text <- "The Confidence Cascade"

subtitle_text <- "For some major diseases, uncertainty spans nearly the size of the estimated burden \u00b7 GBD 2023\nDot = estimate (1.0) \u00b7 Span = 95% uncertainty interval"

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 30,
  source_text = "Global Burden of Disease Collaborative Network \u00b7 GBD 2021 Results \u00b7 IHME, 2022"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(
      size = 26, face = "bold",
      family = fonts$title, color = col_text,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size       = 9.5, family = fonts$subtitle,
      color      = col_subtext,
      lineheight = 1.4, margin = margin(b = 18)
    ),
    plot.caption = element_markdown(
      size = 7.0, family = fonts$caption,
      color = col_subtext, linewidth = 1.3,
      hjust = 0, margin = margin(t = 14)
    ),
    axis.text.y = element_text(
      size = 8.5, family = fonts$text,
      color = col_text, hjust = 1
    ),
    axis.text.x = element_text(
      size = 8, family = fonts$text,
      color = col_subtext, lineheight = 1.2
    ),
    axis.title.x = element_text(
      size = 8.5, family = fonts$text,
      color = col_subtext, margin = margin(t = 8)
    ),
    axis.ticks = element_blank(),

    # vertical grid at ratio breaks — helps read the span
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 55, b = 20, l = 15)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(
  focus_gbd,
  aes(
    y = cause_plot,
    x = val_norm,
    xmin = lower_norm,
    xmax = upper_norm
  )
) +
  # Geoms
  geom_vline(
    xintercept = 1,
    color = "#CECDCA",
    linewidth = 0.35,
    linetype = "solid"
  ) +
  geom_pointinterval(
    color = col_accent,
    point_color = col_bg,
    point_fill = col_accent,
    shape = 21,
    linewidth = 2.0,
    fatten_point = 4.5,
    alpha = 0.92
  ) +
  geom_text(
    aes(x = upper_norm, label = label_text),
    hjust = -0.15,
    family = fonts$text,
    size = 3.0,
    fontface = "bold",
    color = col_accent
  ) +

  # Annotate
  annotate(
    "text",
    x = 1.62, y = 6.4,
    label = "Nearly as large\nas the estimate",
    hjust = 0, vjust = 0.5,
    size = 2.6, family = fonts$text,
    color = col_subtext, fontface = "italic",
    lineheight = 1.25
  ) +

  # Scales
  scale_x_continuous(
    breaks = c(0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0),
    labels = function(x) ifelse(x == 1, "1.0\n(estimate)", paste0(x, "\u00d7")),
    expand = expansion(mult = c(0.02, 0.28))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Ratio to estimate (1.0) · Span = 95% uncertainty interval · Global · 2021",
    y = NULL
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

# ─ Session info ─────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-31
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
#
# ─ Packages ─────────────────────────────────────────────────────────
# ! package        * version  date (UTC) lib source
# base           * 4.3.1    2023-06-16 [?] local
# bit              4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64            4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      * 0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli              3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
# commonmark       2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler         4.3.1    2023-06-16 [2] local
# crayon           1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl             7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets       * 4.3.1    2023-06-16 [2] local
# distributional   0.7.0    2026-03-17 [1] CRAN (R 4.3.1)
# dplyr          * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# farver           2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# forcats        * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics         0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggdist         * 3.3.3    2025-04-23 [1] CRAN (R 4.3.1)
# ggplot2        * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggtext         * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gifski           1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# glue           * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics       * 4.3.1    2023-06-16 [2] local
# P grDevices      * 4.3.1    2023-06-16 [2] local
# P grid             4.3.1    2023-06-16 [2] local
# gridtext         0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable           0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here           * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms              1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# janitor        * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite         2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# lifecycle        1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown         0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate      * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick           2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown         2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods        * 4.3.1    2023-06-16 [2] local
# pacman         * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel         4.3.1    2023-06-16 [2] local
# pillar           1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr          * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6               2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg             1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# RColorBrewer     1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp             1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr          * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# rlang            1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot        2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi       0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg             2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7               0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales         * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo      1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext       * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb     * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# snakecase        0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats          * 4.3.1    2023-06-16 [2] local
# stringi          1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr        * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# svglite          2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts       * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts      1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping      1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble         * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr          * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect       1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidyverse      * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange       0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools            4.3.1    2023-06-16 [2] local
# tzdb             0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# utf8             1.2.6    2025-06-08 [1] CRAN (R 4.3.1)
# P utils          * 4.3.1    2023-06-16 [2] local
# vctrs            0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# vroom            1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
# withr            3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun             0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2             1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
#
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
#
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
#
# ────────────────────────────────────────────────────────────────────
