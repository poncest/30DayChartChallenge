
## Challenge: #30DayChartChallenge 2026 
## Prompt:    Day 06 | Comparisons | Data Day — Reporters Without Borders
## Topic:     A Decade of Divergence in Press Freedom Rankings

## Author:    Steven Ponce
## Date:      2026-04-06

## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/:
##       fonts.R, social_icons.R, base_theme.R, snap.R
##       Caption function: create_dcc_caption(dcc_year, dcc_day, source_text)

## Data source:
##   Reporters Without Borders — World Press Freedom Index 2015 & 2025
##   Downloaded directly from: https://rsf.org/en/index
##   Files: RSF_2015.csv, RSF_2025.csv
##   NOTE: Scores are NOT directly comparable across editions (methodology
##   changed in 2022). Rank movement used instead — directionally valid.


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
  width  = 6,
  height = 9,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----

### |- RSF 2025 ----
rsf_2025 <- read_delim(
  here("2026", "data", "RSF_2025.csv"),
  delim          = ";",
  locale         = locale(encoding = "windows-1252", decimal_mark = ","),
  show_col_types = FALSE
)

### |- RSF 2015 ----
rsf_2015 <- read_delim(
  here("2026", "data", "RSF_2015.csv"),
  delim          = ";",
  locale         = locale(encoding = "windows-1252", decimal_mark = ","),
  show_col_types = FALSE
)


## 3. EXAMINING THE DATA ----
glimpse(rsf_2025)
glimpse(rsf_2015)


## 4. TIDY DATA ----

### |- standardise 2025 ----
r2025 <- rsf_2025 |>
  clean_names() |>
  select(
    iso = iso,
    country = country_en,
    rank_2025 = rank
  ) |>
  mutate(rank_2025 = as.integer(rank_2025))

### |- standardise 2015 ----
r2015 <- rsf_2015 |>
  clean_names() |>
  select(
    iso       = iso,
    rank_2015 = rank_n
  ) |>
  mutate(rank_2015 = as.integer(rank_2015))

### |- join on ISO ----
rsf <- r2025 |>
  inner_join(r2015, by = "iso") |>
  filter(!is.na(rank_2015), !is.na(rank_2025)) |>
  mutate(
    rank_change = rank_2015 - rank_2025,
    direction = case_when(
      rank_change > 5 ~ "improved",
      rank_change < -5 ~ "declined",
      TRUE ~ "stable"
    )
  )

### |- top 10 improvers + top 10 decliners ----
top_improved <- rsf |>
  filter(direction == "improved") |>
  slice_max(rank_change, n = 10, with_ties = FALSE)

top_declined <- rsf |>
  filter(direction == "declined") |>
  slice_min(rank_change, n = 10, with_ties = FALSE)

rsf_plot <- bind_rows(top_improved, top_declined) |>
  arrange(
    direction,
    if_else(direction == "declined", rank_change, -rank_change)
  ) |>
  mutate(country = fct_inorder(country))


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg       = "#F5F3EE",
    text     = "#2C2C2C",
    grid     = "#E8E5E0",
    improved = "#2E6B9E",
    declined = "#8B3030",
    dot_2015 = "#AAAAAA"
  )
)

col_bg <- colors$palette$bg
col_text <- colors$palette$text
col_grid <- colors$palette$grid
col_improved <- colors$palette$improved
col_declined <- colors$palette$declined
col_dot_2015 <- colors$palette$dot_2015

### |- pre-extract arrow color as scalar vector ----
arrow_color_vec <- rsf_plot |>
  mutate(col = if_else(direction == "improved", col_improved, col_declined)) |>
  pull(col) |>
  setNames(as.character(rsf_plot$country))

### |- delta label formatting — compact ----
rsf_plot <- rsf_plot |>
  mutate(
    # compact: ↑51 or ↓48
    delta_label = if_else(
      direction == "improved",
      paste0("\u2191", rank_change),
      paste0("\u2193", abs(rank_change))
    ),
    # delta label
    label_x = rank_2025,
    label_hjust = if_else(direction == "improved", 1.15, -0.15),
    name_x = if_else(direction == "improved", rank_2015, rank_2015),
    name_hjust = if_else(direction == "improved", -0.15, 1.12)
  )

### |- section separator y position ----
n_declined <- sum(rsf_plot$direction == "declined")
sep_y <- n_declined + 0.5

### |- titles and caption ----
title_text    <- "A Decade of Divergence in Press Freedom"

subtitle_text <- glue(
  "The 10 biggest improvements and declines in RSF rank since 2015 &nbsp;<br>",
  "<span style='color:{col_dot_2015}'>&#9679;</span> 2015 rank &nbsp; ",
  "<span style='color:{col_improved}'>&#9658;</span> 2025 rank (improved) &nbsp; ",
  "<span style='color:{col_declined}'>&#9658;</span> 2025 rank (declined)"
)

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 6,
  source_text = "Reporters Without Borders (RSF) | rsf.org/en/index — Rank comparison only;<br>scores not comparable across methodology changes"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base + weekly theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      family = fonts$text, size = 7.5, color = "#777777"
    ),
    axis.title.x = element_text(
      family = fonts$text, size = 7.5, color = "#777777",
      margin = margin(t = 6)
    ),
    plot.title = element_markdown(
      family = fonts$title, size = 14, face = "bold",
      color = col_text, margin = margin(b = 4)
    ),
    plot.subtitle = element_markdown(
      family = "sans", size = 8, color = "#555555",
      lineheight = 1.4, margin = margin(b = 12)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 6, color = "#999999",
      hjust = 0, margin = margin(t = 10)
    ),
    plot.margin = margin(t = 16, r = 16, b = 12, l = 16)
  )
)

theme_set(weekly_theme)


## |- main plot ----
p <- ggplot(rsf_plot) +

  # Geoms
  geom_hline(
    yintercept = sep_y,
    color      = "#DDDDDD",
    linewidth  = 0.3,
    linetype   = "dashed"
  ) +
  geom_segment(
    aes(
      x     = rank_2015, xend = rank_2025,
      y     = country,   yend = country,
      color = country
    ),
    linewidth = 1.1,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed"),
    show.legend = FALSE
  ) +
  geom_point(
    aes(x = rank_2015, y = country),
    size  = 1.6,
    color = col_dot_2015
  ) +
  geom_text(
    aes(x = rank_2015, y = country, label = rank_2015),
    family = fonts$text,
    size = 2.0,
    color = "#BBBBBB",
    vjust = -0.9
  ) +
  geom_text(
    aes(
      x     = name_x,
      y     = country,
      label = country,
      hjust = name_hjust
    ),
    family = fonts$text,
    size = 2.55,
    color = col_text
  ) +
  geom_text(
    aes(
      x     = label_x,
      y     = country,
      label = delta_label,
      hjust = label_hjust,
      color = country
    ),
    family = "sans",
    size = 2.5,
    fontface = "bold",
    show.legend = FALSE
  ) +

  # Annotate
  annotate(
    "text",
    x = 10, y = sep_y + 1.0,
    label = "Improved",
    family = fonts$text,
    size = 2.5, fontface = "bold",
    color = col_improved, hjust = 0
  ) +
  annotate(
    "text",
    x = 10, y = sep_y - 0.4,
    label = "Declined",
    family = fonts$text,
    size = 2.5, fontface = "bold",
    color = col_declined, hjust = 0
  ) +

  # Scales
  scale_color_manual(values = arrow_color_vec) +
  scale_x_continuous(
    breaks = c(1, 50, 100, 150, 180),
    labels = c("1\nMore free", "50", "100", "150", "180\nLess free"),
    expand = expansion(mult = c(0.22, 0.22))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "RSF Rank",
    y        = NULL
  )

# preview
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
# date     2026-03-28
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpUVQzc7/file34b0475a5852". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
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
# ───────────────────────────────────────────────────────────────────────
