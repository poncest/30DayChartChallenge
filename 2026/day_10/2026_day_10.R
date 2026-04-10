
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 10 · Distributions | Pop Culture
## Topic:      From Survival to Stability — Global Income Distribution (2000 vs. 2022)

## Author:     Steven Ponce
## Date:       2026-04-10

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

# Data source: Kaggle — "Movie Industry" (Daniel Grijalva)
#              URL: https://www.kaggle.com/datasets/danielgrijalvas/movies
#              File: 2026/data/movies.csv
#              Key columns: name, genre, year, budget, gross


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,  
  janitor, scales, glue, ggdist
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
movies_raw <- read_csv(here::here("2026/data/movies.csv")) |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(movies_raw)


## 4. TIDY DATA ----

### |- filter and compute ROI ----
movies_clean <- movies_raw |>
  filter(
    genre %in% c("Horror", "Crime"),
    budget > 0,
    gross > 0
  ) |>
  mutate(
    roi       = (gross - budget) / budget,
    roi_log1p = log1p(roi),
    genre     = factor(genre, levels = c("Crime", "Horror"))
  )

### |- summary stats ----
summary_stats <- movies_clean |>
  group_by(genre) |>
  summarise(
    n          = n(),
    median_roi = median(roi),
    p25        = quantile(roi, 0.25),
    p75        = quantile(roi, 0.75),
    pct_loss   = mean(roi < 0) * 100,
    .groups    = "drop"
  )

### |- axis labels with counts ----
genre_labels <- summary_stats |>
  mutate(
    genre = as.character(genre),
    label = glue("{genre}\n(n = {scales::comma(n)})")
  ) |>
  select(genre, label) |>
  tibble::deframe()

### |- top Horror outlier ----
top_horror <- movies_clean |>
  filter(genre == "Horror") |>
  slice_max(roi, n = 1, with_ties = FALSE)

### |- pre-compute positions ----
break_even_log  <- log1p(0)
median_hor_log  <- log1p(summary_stats$median_roi[summary_stats$genre == "Horror"])
median_cri_log  <- log1p(summary_stats$median_roi[summary_stats$genre == "Crime"])
pct_loss_horror <- summary_stats$pct_loss[summary_stats$genre == "Horror"]
pct_loss_crime  <- summary_stats$pct_loss[summary_stats$genre == "Crime"]

### |- x-axis breaks and labels ----
roi_breaks  <- c(-0.75, 0, 1, 5, 10, 50, 100)
log_breaks  <- log1p(roi_breaks)
axis_labels <- c("−75%", "0x\n(break-even)", "1x", "5x", "10x", "50x", "100x")


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    horror     = "#8B1A2A",
    crime      = "#6B84A6",   
    median_pt  = "#1A1A1A",
    annotation = "#444444",
    bg         = "#FAFAF8" 
  )
)

### |- titles and caption ----
title_text <- "Horror Films Are Lottery Tickets"

subtitle_text <- glue(
  "USD return on investment (ROI) distribution for ",
  "<span style='color:{colors$palette$horror};font-weight:700'>Horror</span> and ",
  "<span style='color:{colors$palette$crime};font-weight:700'>Crime</span> films ",
  "(1980–2020, US box office).<br>",
  "Horror shows a much wider spread of outcomes, with fewer losses overall but a handful of extreme wins."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 10,
  source_text = "Kaggle — Movie Industry dataset (Daniel Grijalva)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

font_body    <- fonts$text    %||% ""
font_title   <- fonts$title   %||% ""
font_caption <- fonts$caption %||% ""

### |- theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = colors$palette$bg, color = NA),
    panel.background = element_rect(fill = colors$palette$bg, color = NA),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(
      size = 9, color = colors$palette$annotation, margin = margin(t = 8)
    ),
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      size = 12, face = "bold",
      color = c(colors$palette$crime, colors$palette$horror)
    ),
    axis.text.x = element_text(
      size = 8.5,
      color = colors$palette$annotation,
      lineheight = 1.2
    ),
    axis.ticks = element_blank(),
    plot.title = element_text(
      family = fonts$title,
      face   = "bold",
      size   = 22,
      color  = "#1A1A1A",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family     = fonts$text,
      size       = 10,
      color      = "#444444",
      lineheight = 1.45,
      margin     = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = fonts$text,
      size   = 7.5,
      color  = "gray50",
      hjust  = 0,
      margin = margin(t = 14)
    ),
    plot.margin = margin(20, 24, 14, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- movies_clean |>
  ggplot(aes(x = roi_log1p, y = genre, fill = genre, color = genre)) +
  geom_vline(
    xintercept = break_even_log,
    color = "gray50",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  stat_halfeye(
    .width = c(0.50, 0.90),
    point_size = 3.5,
    point_color = colors$palette$median_pt,
    interval_color = colors$palette$median_pt,
    slab_alpha = 0.3,
    normalize = "groups",
    scale = 0.72,
    adjust = 1.2
  ) +
  annotate(
    "text",
    x = break_even_log + 0.05,
    y = 0.58,
    label = "0x ROI\n(break-even)",
    size = 2.9,
    color = "gray40",
    hjust = 0,
    fontface = "bold",
    lineheight = 1.2,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = break_even_log - 0.15,
    y = 2.38,
    label = glue("{round(pct_loss_horror, 0)}% of Horror\nfilms lose money"),
    size = 2.8,
    color = colors$palette$horror,
    hjust = 1,
    lineheight = 1.2,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = break_even_log - 0.15,
    y = 1.38,
    label = glue("{round(pct_loss_crime, 0)}% of Crime\nfilms lose money"),
    size = 2.8,
    color = colors$palette$crime,
    hjust = 1,
    lineheight = 1.2,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = median_hor_log + 0.08,
    y = 2.44,
    label = glue("Median: {round(summary_stats$median_roi[summary_stats$genre == 'Horror'], 1)}x"),
    size = 2.9,
    color = colors$palette$horror,
    hjust = 0,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = median_cri_log + 0.08,
    y = 1.44,
    label = glue("Median: {round(summary_stats$median_roi[summary_stats$genre == 'Crime'], 1)}x"),
    size = 2.9,
    color = colors$palette$crime,
    hjust = 0,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = log1p(20),
    y = 2.20,
    label = "A handful of films\ndeliver extreme returns",
    size = 2.8,
    color = colors$palette$horror,
    hjust = 0.5,
    lineheight = 1.2,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = log1p(top_horror$roi) - 0.35,
    y = 2.18,
    label = glue("{top_horror$name} ({top_horror$year})\n$15k budget → 12,889x ROI"),
    size = 2.6,
    color = colors$palette$horror,
    hjust = 1,
    lineheight = 1.2,
    family = fonts$text
  ) +
  annotate(
    "point",
    x = log1p(top_horror$roi),
    y = 2.08,
    size = 2,
    color = colors$palette$horror,
    shape = 21,
    fill = "white"
  ) +
  scale_fill_manual(values = c(
    "Horror" = colors$palette$horror,
    "Crime"  = colors$palette$crime
  )) +
  scale_color_manual(values = c(
    "Horror" = colors$palette$horror,
    "Crime"  = colors$palette$crime
  )) +
  scale_x_continuous(
    name = "Return on Investment (USD)  —  log scale  [ (Gross − Budget) / Budget ]",
    breaks = log_breaks,
    labels = axis_labels
  ) +
  scale_y_discrete(labels = genre_labels) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

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
# date     2026-03-23
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpoTI4No/file162cb3132b3". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
# ! package        * version  date (UTC) lib source
# base           * 4.3.1    2023-06-16 [?] local
# bit              4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64            4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder        0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli              3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
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
# labeling         0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────
