
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 11 · Distributions | Physical
## Topic:      From Survival to Stability — Global Income Distribution (2000 vs. 2022)

## Author:     Steven Ponce
## Date:       2026-04-11

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data:       AI4I 2020 Predictive Maintenance Dataset (synthetic)
##             UCI Machine Learning Repository
##             https://archive.ics.uci.edu/ml/machine-learning-databases/00601/ai4i2020.csv
##             Matzka, S. (2020). AI4I 2020 Predictive Maintenance Dataset.
##             UCI Machine Learning Repository. https://doi.org/10.24432/C5HS5C


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,  
  janitor, scales, glue, survival,
  broom
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
raw_data <- read_csv("2026/data/ai4i2020.csv") |> clean_names()


# 3. EXAMINING THE DATA ----
glimpse(raw_data)


## 4. TIDY DATA ----

# Method note:
# AI4I is a synthetic snapshot dataset, not a longitudinal history of machines.
# tool_wear_min is used as a physical exposure proxy. KM-style curves show at
# what point in accumulated wear each failure type begins to appear.
# These are best interpreted as failure-signature curves, not literal machine
# life histories.

### |- Reshape: one row per failure mode per observation ----
failure_modes <- raw_data |>
  select(udi, tool_wear_min, twf, hdf, pwf, osf, rnf) |>
  pivot_longer(
    cols      = c(twf, hdf, pwf, osf, rnf),
    names_to  = "failure_mode",
    values_to = "event"
  ) |>
  mutate(
    failure_label = case_when(
      failure_mode == "twf" ~ "Wear-Out",
      failure_mode == "hdf" ~ "Heat Stress",
      failure_mode == "pwf" ~ "Power Failure",
      failure_mode == "osf" ~ "Overstrain",
      failure_mode == "rnf" ~ "Random Failure"
    ),
    failure_label = factor(
      failure_label,
      levels = c("Wear-Out", "Overstrain", "Power Failure", "Heat Stress", "Random Failure")
    )
  )

### |- Diagnostic: event timing by mode ----
failure_modes |>
  filter(event == 1) |>
  group_by(failure_label) |>
  summarise(
    n_events = n(),
    min_time = min(tool_wear_min),
    med_time = median(tool_wear_min),
    max_time = max(tool_wear_min)
  ) |>
  arrange(med_time)

### |- Fit Kaplan-Meier curves ----
surv_fit <- survfit(
  Surv(time = tool_wear_min, event = event) ~ failure_label,
  data = failure_modes
)

### |- Tidy to ggplot-ready format ----
surv_tidy <- tidy(surv_fit) |>
  mutate(
    failure_label = str_remove(strata, "failure_label="),
    failure_label = factor(
      failure_label,
      levels = c("Wear-Out", "Overstrain", "Power Failure", "Heat Stress", "Random Failure")
    )
  )

### |- End-line label positions ----
# Get the last observed y (estimate) per curve at the terminal time point.
label_ends <- surv_tidy |>
  group_by(failure_label) |>
  slice_tail(n = 1) |>
  ungroup() |>
  mutate(
    label_y = case_when(
      failure_label == "Random Failure" ~ 0.975,
      failure_label == "Heat Stress" ~ 0.900,
      failure_label == "Power Failure" ~ 0.845,
      failure_label == "Wear-Out" ~ 0.600,
      failure_label == "Overstrain" ~ 0.220
    )
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Wear-Out"       = "#8B1A2A",
    "Overstrain"     = "#C47A2C",
    "Power Failure"  = "#3B6EA8",
    "Heat Stress"    = "#8B5FBF",
    "Random Failure" = "#7A8A8F",
    "background"     = "#FAFAFA"
  )
)

### |- titles and caption ----
title_text <- "Different Failure Modes, Different Timelines"

subtitle_text <- paste0(
  "In a synthetic milling machine, five failure modes emerge at different points in tool wear.\n",
  "Kaplan-Meier-style curves show survival probability as accumulated wear increases."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 11,
  source_text = "AI4I 2020 Predictive Maintenance Dataset (synthetic) · UCI Machine Learning Repository · doi.org/10.24432/C5HS5C"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

font_body    <- fonts$text    %||% ""
font_title   <- fonts$title   %||% ""
font_caption <- fonts$caption %||% ""

### |- Base + weekly theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    axis.ticks   = element_blank(),
    axis.text    = element_text(family = fonts$text, size = 9, color = "gray40"),
    axis.title   = element_text(family = fonts$text, size = 9.5, color = "gray30"),
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    
    plot.title = element_text(
      family = fonts$title, face = "bold", size = 22,
      color  = "#2C3E50", margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      family = fonts$text, size = 11, color = "gray40",
      lineheight = 1.3, margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = "gray55",
      hjust  = 0, margin = margin(t = 12)
    ),
    
    # Wide right margin — labels render here via clip = "off"
    plot.margin = margin(t = 20, r = 120, b = 12, l = 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(
  data = surv_tidy,
  aes(x = time, y = estimate, color = failure_label, group = failure_label)
) +

  # Geoms
  annotate("rect",
    xmin = 185, xmax = 253, ymin = 0, ymax = 1,
    fill = "gray95", color = NA
  ) +
  annotate("text",
    x = 182, y = 0.30,
    label = "stable operation",
    hjust = 1,
    family = fonts$text,
    size = 2.5,
    color = "gray65",
    fontface = "italic"
  ) +
  annotate("text",
    x = 188, y = 0.30,
    label = "wear-out zone",
    hjust = 0,
    family = fonts$text,
    size = 2.5,
    color = "gray65",
    fontface = "italic"
  ) +
  annotate("segment",
    x = 185, xend = 185, y = 0.24, yend = 0.36,
    color = "gray75",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  geom_ribbon(
    data = surv_tidy |> filter(failure_label == "Wear-Out"),
    aes(x = time, ymin = conf.low, ymax = conf.high, group = 1),
    inherit.aes = FALSE,
    fill = colors$palette["Wear-Out"],
    alpha = 0.07
  ) +
  geom_step(
    data = surv_tidy |> filter(failure_label != "Random Failure"),
    linewidth = 1.0,
    lineend = "round"
  ) +
  geom_step(
    data = surv_tidy |> filter(failure_label == "Random Failure"),
    linewidth = 0.6,
    linetype = "dashed",
    lineend = "round"
  ) +
  geom_segment(
    data = label_ends,
    aes(x = 253, xend = 258, y = label_y, yend = label_y, color = failure_label),
    inherit.aes = FALSE,
    linewidth = 0.4,
    alpha = 0.6
  ) +
  geom_text(
    data = label_ends,
    aes(x = 259, y = label_y, label = failure_label, color = failure_label),
    inherit.aes = FALSE,
    hjust = 0,
    family = fonts$text,
    size = 3.0,
    show.legend = FALSE
  ) +
  annotate("text",
    x = 4,
    y = 0.08,
    label = "All failures begin late — but each has its own signature.",
    hjust = 0,
    vjust = 0,
    family = fonts$text,
    size = 3.0,
    color = "gray48",
    fontface = "italic"
  ) +

  # Scales
  scale_color_manual(values = colors$palette) +
  scale_x_continuous(
    name   = "Accumulated tool wear (minutes)",
    breaks = seq(0, 240, by = 40)
  ) +
  scale_y_continuous(
    name   = "Survival probability",
    breaks = seq(0, 1, 0.25),
    labels = label_percent(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_cartesian(xlim = c(0, 253), clip = "off") +

  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  guides(color = "none")


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

# ─ Session info ───────────────────────────────────────────────────────────────────
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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpAP1xPF/file1f9441c11abf". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# backports      1.5.0    2024-05-23 [1] CRAN (R 4.3.3)
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# broom        * 1.0.12   2026-01-27 [1] CRAN (R 4.3.1)
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
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# Matrix         1.5-4.1  2023-05-18 [2] CRAN (R 4.3.1)
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
# P splines        4.3.1    2023-06-16 [2] local
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# survival     * 3.5-5    2023-03-12 [2] CRAN (R 4.3.1)
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
# ────────────────────────────────────────────────────────────────────────────────────────
