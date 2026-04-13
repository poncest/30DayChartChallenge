
## Challenge: #30DayChartChallenge 2026 — Day 13
## Prompt:    Relationships | Ecosystems
##
## Topic:     One Intervention, System-Wide Impact
##            Wolf reintroduction reshaped Yellowstone's ecosystem
##            through cascading trophic effects
##
## Author:    Steven Ponce
## Date:      2026-04-13

## NOTE: This script uses custom helper functions for theming and formatting.
##       Sources: R/utils/fonts.R, R/utils/social_icons.R,
##                R/themes/base_theme.R
##       Caption: create_dcc_caption(dcc_year, dcc_day, source_text)

## Data: Curated index values (illustrative, direction-accurate)
##       Based on: Ripple & Beschta (2012), Beschta & Ripple (2009),
##       Hebblewhite et al. (2005) — Yellowstone trophic cascade literature
##       Values are relative indices (0–100), not absolute counts


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
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. DATA (Curated) ----

### |- trophic nodes ----
# Each entity = one level in the Yellowstone cascade
# y positions are fixed for vertical layout (top → bottom)
nodes <- tribble(
  ~entity,    ~trophic_level,  ~y,  ~before, ~after,  ~direction,
  "Wolves",   "Apex Predator", 4.0,       0,    100,  "restored",
  "Elk",      "Herbivore",     3.0,     100,     40,  "reduced",
  "Willows",  "Vegetation",    2.0,      20,     80,  "recovered",
  "Beavers",  "Engineer",      1.0,       5,     25,  "recovered",
  "Rivers",   "Landscape",     0.0,      30,     70,  "stabilized"
)

### |- arrows (edges) ----,
# Each row = one causal link between consecutive trophic levels
# label = annotation text on the arrow
# effect = positive or negative pressure
edges <- tribble(
  ~from,      ~to,        ~y_from, ~y_to, ~effect,    ~label,
  "Wolves",   "Elk",          5.0,   3.8, "negative", "elk population declined\nmovement patterns shifted",
  "Elk",      "Willows",      3.8,   2.6, "positive", "overgrazing reduced\nvegetation recovered",
  "Willows",  "Beavers",      2.6,   1.4, "positive", "willow thickets expanded\nbeaver habitat restored",
  "Beavers",  "Rivers",       1.4,   0.2, "positive", "beaver dams increased\nrivers stabilized"
)


## 3. EXAMINING THE DATA ----
glimpse(nodes)


## 4. TIDY DATA ----

### |- x positions ----
x_node        <-  0.0
x_seg         <-  0.0     
x_label_right <-  0.28    
x_left        <- -0.88    
node_r <- 0.38


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_wolf      = "#8B1A1A",   
    col_elk       = "#C0392B",   
    col_recovered = "#2E7D32",   
    col_neutral   = "#455A64",   
    col_arrow_neg = "#C0392B",   
    col_arrow_pos = "#1B5E20",   
    col_label_bg  = "#FFFFFF",
    bg_color      = "#FAFAF7",
    text_color    = "#1C1C1C",
    grid_color    = "#E8E8E4"
  )
)

### |- node color assignment ----
nodes <- nodes |>
  mutate(
    node_color = case_when(
      entity == "Wolves"  ~ colors$palette$col_wolf,
      entity == "Elk"     ~ colors$palette$col_elk,
      TRUE                ~ colors$palette$col_recovered
    ),
    node_color_text = case_when(
      entity == "Wolves"  ~ colors$palette$col_wolf,
      entity == "Elk"     ~ colors$palette$col_elk,
      TRUE                ~ colors$palette$col_recovered
    ),
    node_fill = case_when(
      entity == "Wolves"  ~ "#FFF0F0",
      entity == "Elk"     ~ "#FFF5F5",
      TRUE                ~ "#F1F8F1"
    )
  )

edges <- edges |>
  mutate(
    y_from = case_when(
      from == "Wolves"  ~ 4.0,
      from == "Elk"     ~ 3.0,
      from == "Willows" ~ 2.0,
      from == "Beavers" ~ 1.0
    ),
    y_to = case_when(
      to == "Elk"     ~ 3.0,
      to == "Willows" ~ 2.0,
      to == "Beavers" ~ 1.0,
      to == "Rivers"  ~ 0.0
    ),
    arrow_color = if_else(
      effect == "negative", 
      colors$palette$col_arrow_neg, 
      colors$palette$col_arrow_pos)
  )

### |- titles and caption ----
title_text    <- "One Intervention, System-Wide Impact"

subtitle_text <- "Wolf reintroduction triggered a trophic cascade across Yellowstone's ecosystem."

caption_text  <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 13,
  source_text = "Ripple & Beschta (2012); Beschta & Ripple (2009)<br>Values are directional indices, not absolute counts"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- main plot ----
p <- ggplot() +

  # trophic level labels
  geom_text(
    data = nodes,
    aes(x = x_left, y = y + 0.18, label = str_to_upper(trophic_level)),
    hjust = 0,
    vjust = 1,
    size = 2.2,
    color = "#888888",
    family = fonts$text,
    fontface = "plain"
  ) +
  # before/after index 
  geom_text(
    data = nodes,
    aes(
      x = x_left, y = y - 0.04,
      label = glue("{before} to {after}"),
      color = I(node_color)
    ),
    hjust = 0,
    vjust = 1,
    size = 2.9,
    family = fonts$text,
    fontface = "bold"
  ) +
  # straight arrow segments 
  geom_segment(
    data = edges,
    aes(
      y     = y_from - node_r,
      yend  = y_to + node_r,
      color = I(arrow_color)
    ),
    x = 0,
    xend = 0,
    linewidth = 1.5,
    arrow = arrow(length = unit(0.18, "inches"), type = "closed", ends = "last"),
    lineend = "butt"
  ) +
  # edge annotation labels
  geom_label(
    data = edges,
    aes(
      x     = x_label_right,
      y     = (y_from + y_to) / 2,
      label = label,
      color = I(arrow_color)
    ),
    hjust = 0,
    vjust = 0.5,
    size = 2.9,
    family = fonts$text,
    fontface = "italic",
    fill = colors$palette$col_label_bg,
    linewidth = 0,
    label.padding = unit(0.18, "lines"),
    lineheight = 1.3
  ) +
  # node circles
  geom_point(
    data = nodes,
    aes(y = y, color = I(node_color), fill = I(node_fill)),
    x = 0,
    size = 17,
    shape = 21,
    stroke = 2.0
  ) +
  # node entity labels 
  geom_text(
    data = nodes,
    aes(y = y, label = entity, color = I(node_color)),
    x = 0,
    size = 3.1,
    family = fonts$text,
    fontface = "bold"
  ) +
  # "INDEX" column header
  annotate(
    "text",
    x = x_left, y = 4.40,
    label = "INDEX: before vs. after",
    hjust = 0,
    vjust = 1,
    size = 2.2,
    color = "#AAAAAA",
    family = fonts$text
  ) +
  # intervention callout 
  annotate(
    "label",
    x = 0, y = 4.44,
    label = "1995: Wolves reintroduced to Yellowstone",
    hjust = 0.5,
    vjust = 0,
    size = 3.0,
    color = colors$palette$col_wolf,
    fill = "#FFF5F5",
    linewidth = 0.3,
    family = fonts$text,
    fontface = "bold"
  ) +
  # outcome callout
  annotate(
    "label",
    x = 0, y = -0.45,
    label = "Result: Rivers stabilized  |  Biodiversity increased  |  Ecosystem resilience restored",
    hjust = 0.5,
    vjust = 1,
    size = 2.8,
    color = colors$palette$col_recovered,
    fill = "#F1F8F1",
    linewidth = 0.3,
    family = fonts$text
  ) +
  # scales
  scale_x_continuous(limits = c(-1.05, 1)) +
  scale_y_continuous(limits = c(-0.72, 4.75)) +
  # labs 
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  # theme 
  theme_void() +
  theme(
    plot.background = element_rect(fill = colors$palette$bg_color, color = NA),
    panel.background = element_rect(fill = colors$palette$bg_color, color = NA),
    plot.title = element_text(
      family = fonts$title, face = "bold",
      size = 26, color = colors$palette$text_color,
      margin = margin(t = 10, b = 6, l = 5)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$text,
      size = 11,
      color = "#444444",
      lineheight = 1.3,
      margin = margin(t = 5, b = 8, l = 5)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$text,
      size = 7.5,
      color = "#888888",
      lineheight = 1.3,
      margin = margin(t = 8, b = 6)
    ),
    plot.margin = margin(t = 20, r = 20, b = 12, l = 20)
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

# ─ Session info ──────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-24
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmpc5sO5X/file8f887a4a6414". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ──────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
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
# ─────────────────────────────────────────────────────────────────
