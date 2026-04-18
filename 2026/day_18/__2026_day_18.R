
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 18 · Relationships | SCMP Theme Day

## Topic:      Renewables vs Per-Capita CO₂ — The Gap Is Already Structural
## Author:     Steven Ponce
## Date:       2026-04-18

## NOTE: This script uses custom helper functions for theming and formatting.
##       Helpers sourced from R/utils/ and R/themes/:
##       fonts.R · social_icons.R · base_theme.R
##       Call create_dcc_caption(), get_theme_colors(), get_font_families(),
##       create_base_theme(), extend_weekly_theme(), setup_fonts()

## Data:       Our World in Data — Energy Dataset 
##             https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv

##             Our World in Data — CO₂ per capita
##             https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, patchwork,
  janitor, scales, glue, ggrepel
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


## 2. READ IN THE DATA ----
owid_raw <- read_csv(here("2026/data/owid-energy-data.csv"),
  show_col_types = FALSE
) |>
  clean_names()

owid_co2_raw <- read_csv(here("2026/data/owid-co2-data.csv"),
  show_col_types = FALSE
) |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(owid_raw)
glimpse(owid_co2_raw)


## 4. TIDY DATA ----

### |- Define groups ----
asia_pacific <- c(
  "China", "India", "Japan", "South Korea", "Australia",
  "Vietnam", "Indonesia", "New Zealand", "Thailand",
  "Philippines", "Malaysia", "Pakistan", "Bangladesh",
  "Taiwan", "Singapore", "Myanmar", "Cambodia"
)

# Aggregates and non-countries to exclude
exclude_entities <- c(
  "World", "Asia", "Europe", "Africa", "North America",
  "South America", "Oceania", "European Union (27)",
  "High-income countries", "Low-income countries",
  "Lower-middle-income countries", "Upper-middle-income countries",
  "OECD (Eur)", "Non-OECD", "OPEC"
)

### |- Pull co2_per_capita from CO₂ dataset ----
co2_2022 <- owid_co2_raw |>
  filter(year == 2022) |>
  select(country, co2_per_capita)

### |- Filter energy data and join CO₂ ----
df <- owid_raw |>
  filter(
    year == 2022,
    !country %in% exclude_entities,
    !is.na(renewables_share_elec),
    !is.na(population),
    population >= 1e6 # exclude micro-states
  ) |>
  select(country, iso_code, population, renewables_share_elec) |>
  left_join(co2_2022, by = "country") |>
  filter(!is.na(co2_per_capita)) |>
  mutate(
    group = case_when(
      country %in% asia_pacific ~ "asia_pacific",
      co2_per_capita > 12 & renewables_share_elec < 20 ~ "laggard",
      TRUE ~ "other"
    ),
    color_group = factor(group,
      levels = c("asia_pacific", "laggard", "other")
    )
  )

### |- Compute anchor stats for headline ----
stats <- df |>
  summarise(
    median_co2_global = median(co2_per_capita, na.rm = TRUE),
    mean_co2_high_ren = mean(co2_per_capita[renewables_share_elec >= 50],
      na.rm = TRUE
    ),
    mean_co2_low_ren = mean(co2_per_capita[renewables_share_elec < 20],
      na.rm = TRUE
    ),
    n_high_ren = sum(renewables_share_elec >= 50),
    n_low_ren = sum(renewables_share_elec < 20)
  ) |>
  mutate(
    ratio = mean_co2_low_ren / mean_co2_high_ren
  )

### |- Label data (annotated countries — max 8) ----
label_countries <- c(
  "China", "India", "Japan", "Australia",
  "New Zealand", "Vietnam", "South Korea"
)

laggard_label_countries <- c(
  "United Arab Emirates", "Bahrain", "Saudi Arabia"
)

df_labels <- df |>
  filter(country %in% c(label_countries, laggard_label_countries))

### |- Compute real values for text ----
ratio_val   <- round(stats$ratio, 1)
median_co2  <- round(stats$median_co2_global, 1)
mean_hi_ren <- round(stats$mean_co2_high_ren, 1)
mean_lo_ren <- round(stats$mean_co2_low_ren, 1)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "asia_pacific" = "#F4A932",   
    "laggard"      = "#E8503A",   
    "other"        = "#4A5568",   
    "background"   = "#0D1B2A",   
    "panel"        = "#14253A",   
    "grid"         = "#1E3045",   
    "text_primary" = "#FFFFFF",
    "text_muted"   = "#B0B8C4",
    "text_body"    = "#9AA0AA",
    "annotation"   = "#E8EDF2"
  )
)

### |- titles and caption ----
title_text <- str_glue(
  "Renewables are already separating\nlow- and high-emission economies"
)

subtitle_text <- str_glue(
  "<span style='font-family:barlow_condensed;font-size:38pt;color:#F4A932;'><b>{ratio_val}×</b></span>",
  "<span style='font-size:10pt;color:#9AA0AA;'> lower emissions in high-renewable economies</span><br><br>",
  "<span style='font-size:10pt;color:#C9D1D9;'>",
  "In 2022, high-renewable economies (>50%) averaged **{mean_hi_ren} tonnes** of CO₂ per person.<br>",
  "Fossil-heavy economies averaged **{mean_lo_ren} tonnes**. The transition is already dividing economies.",
  "</span>"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 18,
  source_text = "Our World in Data · Energy Institute · Ember (2022)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

# Load condensed bold for headline (SCMP-style heavy title)
font_add_google("Barlow Condensed", "barlow_condensed")
showtext_auto()

### |- Base and weekly theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Background — deep navy throughout
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#14253A", color = NA),

    # Grid — barely visible
    panel.grid.major = element_line(color = "#1E3045", linewidth = 0.3),
    panel.grid.minor = element_blank(),

    # Axes
    axis.text = element_text(
      color = "#B0B8C4", size = 9,
      family = fonts$text
    ),
    axis.title = element_text(
      color = "#B0B8C4", size = 10,
      family = fonts$text
    ),
    axis.ticks = element_blank(),

    # Title block
    plot.title = element_text(
      family = "barlow_condensed",
      size = 26,
      color = "#FFFFFF",
      face = "bold",
      lineheight = 1.05,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family     = fonts$text,
      size       = 10.5,
      color      = "#C9D1D9",
      lineheight = 2.0,
      margin     = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family    = fonts$text,
      size      = 8,
      color     = "#4A5568",
      hjust     = 0,
      margin    = margin(t = 12)
    ),

    # Margins
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )
)

theme_set(weekly_theme)


### |- main plot ----
p <- ggplot(df, aes(
  x = renewables_share_elec,
  y = co2_per_capita,
  color = color_group
  )) +

  # Geoms
  geom_point(
    data   = filter(df, group == "other"),
    size   = 1.8,
    alpha  = 0.25,
    shape  = 16
  ) +
  geom_point(
    data  = filter(df, group == "laggard"),
    size  = 2.8,
    alpha = 0.90,
    shape = 16
  ) +
  geom_point(
    data  = filter(df, group == "asia_pacific"),
    size  = 3.2,
    alpha = 0.95,
    shape = 16
  ) +
  geom_hline(
    yintercept = stats$median_co2_global,
    color      = "#B0B8C4",
    linewidth  = 0.4,
    linetype   = "dashed",
    alpha      = 0.5
  ) +
  geom_text_repel(
    data = df_labels,
    aes(label = country),
    size = 2.8,
    family = fonts$text,
    color = "#E8EDF2",
    segment.color = "#4A5568",
    segment.size = 0.3,
    box.padding = 0.5,
    point.padding = 0.4,
    max.overlaps = 12,
    min.segment.length = 0.2,
    direction = "both",
    seed = 42
  ) +
  
  # Annotate
  annotate(
    "text",
    x = 97,
    y = stats$median_co2_global + 0.45,
    label = glue("Global median: {median_co2} tonnes"),
    color = "#5A6878",
    size = 2.5,
    hjust = 1,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 8,
    y = 16,
    label = "Fossil-dependent\neconomies",
    color = "#6A80A0",
    size = 3.8,
    hjust = 0,
    lineheight = 1.1,
    fontface = "italic",
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 75,
    y = 1.8,
    label = "Transition\nleaders",
    color = "#506070", 
    size = 3.8,
    hjust = 1,
    lineheight = 1.1,
    fontface = "italic",
    family = fonts$text
  ) +
  annotate(
    "richtext",
    x = 90,
    y = 4.2,
    label = "Renewable leaders<br>sit below **4 tonnes** per person",
    color = "#F4A932",
    size = 3.2,
    hjust = 1,
    lineheight = 1.3,
    family = fonts$text,
    fill = NA,
    label.color = NA
  ) +
  annotate(
    "richtext",
    x = 10,
    y = 18.5,
    label = "Fossil-heavy economies<br>remain above **12 tonnes** per person",
    color = "#E8503A",
    size = 3.2,
    hjust = 0,
    lineheight = 1.3,
    family = fonts$text,
    fill = NA,
    label.color = NA
  ) +
  annotate(
    "richtext",
    x = 52,
    y = 14,
    label = "Asia shows the widest spread —<br>from transition leaders to major emitters",
    color = "#F4A932",
    size = 3.2,
    hjust = 0,
    lineheight = 1.3,
    family = fonts$text,
    fill = NA,
    label.color = NA
  ) +
  annotate(
    "text",
    x = 97,
    y = 0.4,
    label = "Higher renewables →",
    color = "#5A6878",
    size = 2.8,
    hjust = 1,
    fontface = "italic",
    family = 'sans'
  ) +
  annotate(
    "text",
    x = 12,
    y = 24.8,
    label = "↑ Higher emissions",
    color = "#5A6878",
    size = 2.8,
    hjust = 0,
    fontface = "italic",
    family = 'sans'
  ) +
  scale_color_manual(
    values = c(
      "asia_pacific" = "#F4A932",
      "laggard"      = "#E8503A",
      "other"        = "#4A5568"
    ),
    guide = "none" 
  ) +

  # Scales 
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5),
    labels = function(y) paste0(y, "t"),
    expand = expansion(mult = c(0.02, 0.04))
  ) +

  #  Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "Share of electricity from renewables (%)",
    y        = "CO₂ per capita (tonnes)"
  )

### |- Preview ----
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
# date     2026-03-27
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpeA9Rfm/file6abc416437cd". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
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
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
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
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────


