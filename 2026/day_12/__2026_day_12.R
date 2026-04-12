
## Challenge: #30DayChartChallenge 2026 | Day 12
## Prompt:    Distribution | Data Day — Reporters Without Borders
## Topic:     The Shape of Press Freedom

## Author:    Steven Ponce
## Date:      2026-04-12

## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/:
##       fonts.R, social_icons.R, base_theme.R, snap.R
##       Caption function: create_dcc_caption(dcc_year, dcc_day, source_text)

## Data source:
##   Reporters Without Borders — World Press Freedom Index 2025
##   https://rsf.org/en/index?year=2025
##   JSON API: https://rsf.org/index/ajax/index?year=2025&lang=en


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
  height = 6,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
qog_raw <- read_csv(
  here::here("2026/data/qog_std_cs_jan25.csv"),
  show_col_types = FALSE
)


## 3. EXAMINING THE DATA ----
glimpse(qog_raw)

# All RSF columns present in this QoG edition
qog_raw |> select(starts_with("rsf_")) |> names()

# Bottom 10 by press freedom score
qog_raw |>
  select(cname, ccodealp, starts_with("rsf_")) |>
  filter(!is.na(rsf_pfi)) |>
  arrange(rsf_pfi) |>
  print(n = 10)

# Verify exact country name spellings used in QoG
qog_raw |>
  filter(str_detect(cname, "Norway|United States|Russia|China|Eritrea")) |>
  select(cname, rsf_pfi, rsf_si)


## 4. TIDY DATA ----
rsf <- qog_raw |>
  select(
    country      = cname,
    iso          = ccodealp,
    score_global = rsf_pfi,
    score_safety = rsf_si
  ) |>
  filter(!is.na(score_global))

# n check — expect ~180 rows
# nrow(rsf)

### |- annotation countries ----
annotations <- tribble(
  ~country,                   ~label,           ~x_nudge, ~hjust,
  "Norway",                   "Norway",          0,         0.5,
  "United States of America", "United\nStates",  0,         0.5,
  "Russia",                   "Russia",          0,         0.5,
  "China",                    "China",           3.5,       0,    
  "Eritrea",                  "Eritrea",        -3.5,       1     
) |>
  left_join(rsf |> select(country, score_global, score_safety), by = "country") |>
  mutate(x_label = score_global + x_nudge)

### |- RSF tier bands ----
# Good ≥75 | Fairly good 55–74.9 | Problematic 40–54.9
# Difficult 25–39.9 | Very serious 0–24.9
tier_bands <- tibble(
  xmin  = c(0,    25,   40,   55,   75),
  xmax  = c(25,   40,   55,   75,  100),
  tier  = c("Very\nserious", "Difficult", "Problematic", "Fairly\ngood", "Good"),
  fill  = c("gray20", "#8b1a1a", "#c45c00", "#b8860b", "#2d6a4f")
)

### |- pre-compute density peak for y-placement of tier labels ----
dens   <- density(rsf$score_global, bw = "nrd0", n = 512)
peak_y <- max(dens$y)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg_col         = "#0d0d1a",
    col_curve      = "#F5F5F5",   
    col_annotation = "#FFFFFF",
    col_rug_free   = "#52b788",   
    col_rug_mid    = "#adb5bd",   
    col_rug_danger = "#e63946" 
  )
)

### |- titles and caption ----
title_text    <- "The Shape of Press Freedom"

subtitle_text <- glue(
  "Most countries cluster in the middle — but the extremes define the reality<br>",
  "<span style='color:#adb5bd;font-size:8pt;'>",
  "Global distribution of press freedom scores across 180 countries (RSF, 2024)<br>",
  "Rug marks coloured by journalist safety: ",
  "<span style='color:#52b788;'>Safe \u226560</span>  \u00b7  ",
  "<span style='color:#adb5bd;'>At risk 35\u201359</span>  \u00b7  ",
  "<span style='color:#e63946;'>Danger &lt;35</span>",
  "</span>"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 12,
  source_text = "Reporters Without Borders (RSF) via QoG Institute · rsf.org/en/index"
)


### |- fonts ----
setup_fonts()
fonts <- get_font_families()

font_body    <- fonts$text    %||% ""
font_title   <- fonts$title   %||% ""
font_caption <- fonts$caption %||% ""

### |- theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # canvas
    plot.background  = element_rect(fill = colors$palette$bg_col, color = NA),
    panel.background = element_rect(fill = colors$palette$bg_col, color = NA),
    
    # grid — off; annotations carry the story
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # axes
    axis.text.x  = element_text(color = "#adb5bd", size = 9),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.title.x = element_text(
      color = "#adb5bd", size = 9, margin = margin(t = 8)
    ),
    axis.title.y = element_blank(),
    
    # titles
    plot.title = element_text(
      family = fonts$title, face = "bold",
      size = 22, color = "#FFFFFF",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 8,
      color = "#d4d4d4", lineheight = 1.4,
      margin = margin(b = 16)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 6,
      color = "#6c757d", hjust = 0,
      margin = margin(t = 12)
    ),
    plot.margin = margin(20, 24, 12, 24)
  )
)

theme_set(weekly_theme)


### |- main plot ----
p <- ggplot(rsf, aes(x = score_global)) +
  
  # --- tier background bands (softened — context not competition) ---
  geom_rect(
    data        = tier_bands,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    alpha = 0.15
  ) +
  scale_fill_identity() +
  
  # --- tier labels ---
  geom_text(
    data        = tier_bands,
    inherit.aes = FALSE,
    aes(
      x     = (xmin + xmax) / 2,
      y     = peak_y * 1.04,
      label = tier,
      color = fill
    ),
    size       = 2.3,
    family     = fonts$text,
    vjust      = 0,
    lineheight = 0.85,
    alpha      = 0.75
  ) +
  scale_color_identity() +
  
  # --- density curve — explicit scalar color, slightly heavier line ---
  geom_density(
    bw        = "nrd0",
    color     = colors$palette$col_curve,
    fill      = colors$palette$col_curve,
    alpha     = 0.07,
    linewidth = 1.3
  ) +
  
  # --- rug: coloured by safety score, reduced alpha for less noise ---
  geom_rug(
    aes(color = case_when(
      score_safety >= 60 ~ colors$palette$col_rug_free,
      score_safety >= 35 ~ colors$palette$col_rug_mid,
      TRUE               ~ colors$palette$col_rug_danger
    )),
    length    = unit(0.025, "npc"),
    alpha     = 0.60,
    linewidth = 0.35
  ) +
  
  # --- drop lines: anchor to true score position ---
  geom_segment(
    data        = annotations,
    inherit.aes = FALSE,
    aes(x = score_global, xend = score_global, y = 0, yend = -0.0008),
    color     = "#888888",
    linewidth = 0.35,
    linetype  = "dotted"
  ) +
  
  # --- country labels: nudged x position for separation ---
  geom_text(
    data        = annotations,
    inherit.aes = FALSE,
    aes(x = x_label, y = -0.0016, label = label, hjust = hjust),
    color      = colors$palette$col_annotation,
    family     = fonts$text,
    size       = 2.7,
    vjust      = 1,
    lineheight = 0.85
  ) +
  
  # --- scales ---
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = label_number()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.16))) +
  
  # --- labels ---
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "Press Freedom Score  (0 = No freedom  \u00b7  100 = Full freedom)"
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

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpIzdxUL/file5544485931d2". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
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
# ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
