
## Challenge: #30DayChartChallenge 2026 
## Prompt:    Day 05 | Comparisons | Experimental
## Title:     Carbon Intensity of Electricity by World Bank Income Group

## Author:    Steven Ponce
## Date:      2026-04-05

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source: Our World in Data — Energy Dataset
##   https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
##   Metric: carbon_intensity_elec (gCO₂/kWh)
##   Years:  2020–2023 average per country

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,     
  janitor, scales, glue, ggrepel           
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 6,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
energy_raw <- read_csv("2026/data/owid-energy-data.csv")


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)


## 4. TIDY DATA ----

### |- World Bank FY2024 income classification ----
# Sourced from: https://databank.worldbank.org/data/download/site-content/CLASS.xlsx
# Hardcoded from the same tribble used in Day 03

wb_income <- tribble(
  ~country,                              ~income_group,
  "Afghanistan",                         "Low income",
  "Albania",                             "Upper middle income",
  "Algeria",                             "Lower middle income",
  "Angola",                              "Lower middle income",
  "Argentina",                           "Upper middle income",
  "Armenia",                             "Upper middle income",
  "Australia",                           "High income",
  "Austria",                             "High income",
  "Azerbaijan",                          "Upper middle income",
  "Bangladesh",                          "Lower middle income",
  "Belarus",                             "Upper middle income",
  "Belgium",                             "High income",
  "Benin",                               "Low income",
  "Bolivia",                             "Lower middle income",
  "Bosnia and Herzegovina",              "Upper middle income",
  "Brazil",                              "Upper middle income",
  "Bulgaria",                            "Upper middle income",
  "Burkina Faso",                        "Low income",
  "Burundi",                             "Low income",
  "Cambodia",                            "Lower middle income",
  "Cameroon",                            "Lower middle income",
  "Canada",                              "High income",
  "Chad",                                "Low income",
  "Chile",                               "High income",
  "China",                               "Upper middle income",
  "Colombia",                            "Upper middle income",
  "Congo",                               "Lower middle income",
  "Costa Rica",                          "Upper middle income",
  "Croatia",                             "High income",
  "Czech Republic",                      "High income",
  "Democratic Republic of Congo",        "Low income",
  "Denmark",                             "High income",
  "Dominican Republic",                  "Upper middle income",
  "Ecuador",                             "Upper middle income",
  "Egypt",                               "Lower middle income",
  "El Salvador",                         "Lower middle income",
  "Estonia",                             "High income",
  "Ethiopia",                            "Low income",
  "Finland",                             "High income",
  "France",                              "High income",
  "Georgia",                             "Upper middle income",
  "Germany",                             "High income",
  "Ghana",                               "Lower middle income",
  "Greece",                              "High income",
  "Guatemala",                           "Upper middle income",
  "Honduras",                            "Lower middle income",
  "Hungary",                             "High income",
  "India",                               "Lower middle income",
  "Indonesia",                           "Upper middle income",
  "Iran",                                "Lower middle income",
  "Iraq",                                "Upper middle income",
  "Ireland",                             "High income",
  "Israel",                              "High income",
  "Italy",                               "High income",
  "Ivory Coast",                         "Lower middle income",
  "Japan",                               "High income",
  "Jordan",                              "Upper middle income",
  "Kazakhstan",                          "Upper middle income",
  "Kenya",                               "Lower middle income",
  "Kosovo",                              "Upper middle income",
  "Kuwait",                              "High income",
  "Kyrgyzstan",                          "Lower middle income",
  "Laos",                                "Lower middle income",
  "Latvia",                              "High income",
  "Lebanon",                             "Lower middle income",
  "Libya",                               "Upper middle income",
  "Lithuania",                           "High income",
  "Luxembourg",                          "High income",
  "Madagascar",                          "Low income",
  "Malawi",                              "Low income",
  "Malaysia",                            "Upper middle income",
  "Mali",                                "Low income",
  "Mexico",                              "Upper middle income",
  "Moldova",                             "Lower middle income",
  "Mongolia",                            "Lower middle income",
  "Morocco",                             "Lower middle income",
  "Mozambique",                          "Low income",
  "Myanmar",                             "Lower middle income",
  "Nepal",                               "Lower middle income",
  "Netherlands",                         "High income",
  "New Zealand",                         "High income",
  "Nicaragua",                           "Lower middle income",
  "Niger",                               "Low income",
  "Nigeria",                             "Lower middle income",
  "North Macedonia",                     "Upper middle income",
  "Norway",                              "High income",
  "Pakistan",                            "Lower middle income",
  "Panama",                              "Upper middle income",
  "Paraguay",                            "Upper middle income",
  "Peru",                                "Upper middle income",
  "Philippines",                         "Lower middle income",
  "Poland",                              "High income",
  "Portugal",                            "High income",
  "Romania",                             "Upper middle income",
  "Russia",                              "Upper middle income",
  "Rwanda",                              "Low income",
  "Saudi Arabia",                        "High income",
  "Senegal",                             "Lower middle income",
  "Serbia",                              "Upper middle income",
  "Slovakia",                            "High income",
  "Slovenia",                            "High income",
  "South Africa",                        "Upper middle income",
  "South Korea",                         "High income",
  "Spain",                               "High income",
  "Sri Lanka",                           "Lower middle income",
  "Sudan",                               "Low income",
  "Sweden",                              "High income",
  "Switzerland",                         "High income",
  "Syria",                               "Low income",
  "Taiwan",                              "High income",
  "Tajikistan",                          "Low income",
  "Tanzania",                            "Lower middle income",
  "Thailand",                            "Upper middle income",
  "Togo",                                "Low income",
  "Trinidad and Tobago",                 "High income",
  "Tunisia",                             "Lower middle income",
  "Turkey",                              "Upper middle income",
  "Turkmenistan",                        "Upper middle income",
  "Uganda",                              "Low income",
  "Ukraine",                             "Lower middle income",
  "United Arab Emirates",                "High income",
  "United Kingdom",                      "High income",
  "United States",                       "High income",
  "Uruguay",                             "High income",
  "Uzbekistan",                          "Lower middle income",
  "Venezuela",                           "Upper middle income",
  "Vietnam",                             "Lower middle income",
  "Yemen",                               "Low income",
  "Zambia",                              "Low income",
  "Zimbabwe",                            "Lower middle income"
)

### |- compute 2020–2023 average carbon intensity per country ----
carbon_avg <- energy_raw |>
  filter(year %in% 2020:2023) |>
  filter(!is.na(carbon_intensity_elec)) |>
  # exclude aggregates / regions (no iso_code or all-caps codes like OWID_*)
  filter(!is.na(iso_code), !str_detect(iso_code, "^OWID")) |>
  group_by(country) |>
  summarise(
    carbon_intensity = mean(carbon_intensity_elec, na.rm = TRUE),
    n_years          = n(),
    .groups          = "drop"
  ) |>
  # require at least 3 of 4 years of data for reliability
  filter(n_years >= 3)

### |- join income classification ----
plot_data <- carbon_avg |>
  inner_join(wb_income, by = "country") |>
  mutate(
    income_group = factor(income_group, levels = c(
      "Low income", "Lower middle income",
      "Upper middle income", "High income"
    ))
  )

### |- compute group-level summary stats ----
group_stats <- plot_data |>
  group_by(income_group) |>
  summarise(
    median_ci  = median(carbon_intensity),
    q25        = quantile(carbon_intensity, 0.25),
    q75        = quantile(carbon_intensity, 0.75),
    .groups    = "drop"
  )

### |- sort income groups by median carbon intensity (highest → lowest) ----
group_order <- group_stats |>
  arrange(desc(median_ci)) |>
  pull(income_group)

plot_data <- plot_data |>
  mutate(income_group = factor(income_group, levels = group_order)) |>
  mutate(
    income_group = fct_recode(
      income_group,
      "Upper middle\nincome" = "Upper middle income",
      "Lower middle\nincome" = "Lower middle income"
    )
  )

group_stats <- group_stats |>
  mutate(income_group = factor(income_group, levels = group_order)) |>
  mutate(
    income_group = fct_recode(
      income_group,
      "Upper middle\nincome" = "Upper middle income",
      "Lower middle\nincome" = "Lower middle income"
    )
  )

### |- identify labels: extremes + analytically chosen surprises ----
# Per group: highest, lowest, and country farthest from the global median
global_median <- median(plot_data$carbon_intensity)

label_data <- plot_data |>
  group_by(income_group) |>
  mutate(
    deviation_from_global = abs(carbon_intensity - global_median),
    rank_in_group         = rank(carbon_intensity)
  ) |>
  mutate(
    is_extreme  = rank_in_group == max(rank_in_group) | rank_in_group == min(rank_in_group),
    is_surprise = deviation_from_global == max(deviation_from_global) & !is_extreme
  ) |>
  ungroup() |>
  filter(is_extreme | is_surprise)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "Low income"           = "#8B1A2A",  
    "Lower middle income"  = "#C0434E",  
    "Upper middle income"  = "#B08090",  
    "High income"          = "#2E1122",  
    "iqr_band"             = "#D4C7BB",  
    "median_dot_fill"      = "#FFFFFF",  
    "median_dot_stroke"    = "#3D0C1E",  
    "median_label"         = "#3D0C1E",  
    "label_text"           = "gray20",
    "global_ref"           = "#B29A8B"   
  )
)

### |- titles and caption ----
title_text <- "Same Income Group, Very Different Power Systems"

# "Each tick is a country" — plain weight; methodological note, not the story
subtitle_text <- str_glue(
  "Carbon intensity varies widely within World Bank income groups.<br><br>",
  "Each tick is a country; dots and bands show the median and middle 50% (2020–2023 average).<br>",
  "<span style='color:gray45;font-style:italic;'>Within-group variation often rivals differences between groups.</span>"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 05,
  source_text = "Our World in Data — Energy Dataset (Ember / Energy Institute)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # strip / facet labels
    strip.text         = element_blank(),
    
    # axes
    axis.title.x       = element_text(
      size = 9, color = "gray40", margin = margin(t = 5, b = 5), family = fonts$text
    ),
    axis.title.y       = element_blank(),
    axis.text.x        = element_text(size = 8, color = "gray40", family = fonts$text),
    axis.text.y        = element_text(
      size = 9.5, color = "gray25", face = "plain", family = fonts$text, hjust = 1
    ),
    axis.ticks         = element_blank(),
    axis.line          = element_blank(),
    
    # grid — only faint vertical reference lines
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # plot margins
    plot.margin        = margin(t = 20, r = 25, b = 10, l = 15),
    
    # title / subtitle / caption
    plot.title         = element_text(
      size = 18, face = "bold", family = fonts$title,
      color = "gray10", margin = margin(b = 6)
    ),
    plot.subtitle      = element_markdown(
      size = 9.2, family = fonts$text, color = "gray35",
      lineheight = 1.1, margin = margin(b = 18)
    ),
    plot.caption       = element_markdown(
      size = 7, family = fonts$caption, color = "gray55",
      hjust = 0, margin = margin(t = 5)
    )
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot() +
  geom_vline(
    xintercept = global_median,
    color      = colors$palette$global_ref,
    linewidth  = 0.45,
    linetype   = "22",
    alpha      = 0.9
  ) +
  annotate(
    "text",
    x      = global_median,
    y      = 4.65,
    label  = "Global median",
    size   = 2.4,
    color  = "gray45",
    family = fonts$text,
    vjust  = 0,
    hjust  = -0.1
  ) +
  geom_segment(
    data = group_stats,
    aes(
      x    = q25,
      xend = q75,
      y    = income_group,
      yend = income_group
    ),
    linewidth = 3.6,
    color = colors$palette$iqr_band,
    lineend = "round",
    alpha = 0.75
  ) +
  geom_segment(
    data = plot_data,
    aes(
      x     = carbon_intensity,
      xend  = carbon_intensity,
      y     = as.numeric(income_group) - 0.36,
      yend  = as.numeric(income_group) + 0.36,
      color = income_group
    ),
    linewidth = 0.45,
    alpha = 0.65
  ) +
  geom_point(
    data = group_stats,
    aes(x = median_ci, y = income_group),
    size = 4.5,
    shape = 21,
    fill = colors$palette$median_dot_fill,
    color = colors$palette$median_dot_stroke,
    stroke = 1.6
  ) +
  geom_text(
    data = group_stats,
    aes(
      x     = median_ci,
      y     = income_group,
      label = glue("{round(median_ci)} g")
    ),
    vjust = -1.4,
    size = 2.8,
    color = colors$palette$median_label,
    family = fonts$text,
    fontface = "bold"
  ) +
  geom_text_repel(
    data = label_data,
    aes(
      x     = carbon_intensity,
      y     = income_group,
      label = country
    ),
    size = 2.6,
    color = colors$palette$label_text,
    family = fonts$text,
    nudge_y = 0.42,
    direction = "x",
    segment.size = 0.25,
    segment.color = "gray55",
    segment.curvature = 0,
    min.segment.length = 0.1,
    box.padding = 0.15,
    point.padding = 0.1,
    force = 0.8,
    max.overlaps = 20,
    seed = 42
  ) +
  scale_color_manual(
    values = c(
      "Low income"           = colors$palette$`Low income`,
      "Lower middle\nincome" = colors$palette$`Lower middle income`,
      "Upper middle\nincome" = colors$palette$`Upper middle income`,
      "High income"          = colors$palette$`High income`
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = label_number(suffix = " g"),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_discrete(
    expand = expansion(add = c(0.8, 0.8))
  ) +
  coord_cartesian(xlim = c(0, 1400)) +
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "Carbon intensity of electricity (gCO₂ per kWh, 2020–2023 average)"
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
# date     2026-03-20
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpmQ4tLk/file64d0483c21db". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
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

# ───────────────────────────────────────────────────────────────────────
