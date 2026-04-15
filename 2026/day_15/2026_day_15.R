
## Challenge: #30DayChartChallenge 2026
## Prompt:    Day 15 · Relationships | Correlation
## Topic:     Fossil Share vs. GDP per Capita by Income Group

## Author:    Steven Ponce
## Date:      2026-04-15

## NOTE: This script uses custom helper functions for theming and formatting.
##       Sourced from R/utils/ and R/themes/ — see DOCUMENTATION section at end.

## Data source:
##   Our World in Data — Energy Data (OWID/Ember/Energy Institute)
##   https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
##   World Bank FY2024 Income Classification (hardcoded tribble from Day 03)


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
  width  = 12,
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
owid_energy <- read_csv(
  here::here("2026/data/owid-energy-data.csv"),
  locale = locale(encoding = "latin1")
) |> clean_names()


## 3. EXAMINING THE DATA ----
glimpse(owid_energy)

owid_energy |>
  filter(year == 2022) |>
  select(country, iso_code, gdp, population, fossil_share_elec) |>
  skimr::skim()


## 4. TIDY DATA ----

### |- World Bank FY2024 income classification ----
# Sourced from: https://databank.worldbank.org/data/download/site-content/CLASS.xlsx
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

### |- ordered factor for income groups ----
income_levels <- c("Low income", "Lower middle income", "Upper middle income", "High income")

### |- filter, compute, join ----
energy_2022 <- owid_energy |>
  filter(
    year == 2022,
    !is.na(iso_code),           
    !is.na(gdp),
    !is.na(population),
    !is.na(fossil_share_elec),
    population > 1e6            
  ) |>
  mutate(gdp_per_capita = gdp / population) |>
  select(country, iso_code, gdp_per_capita, fossil_share_elec) |>
  left_join(wb_income, by = "country") |>
  filter(!is.na(income_group)) |>
  mutate(
    income_group = factor(income_group, levels = income_levels),
    # 2 labels max per panel — policy-relevant or structurally interesting outliers only
    label_flag = case_when(
      country == "Democratic Republic of Congo" ~ "DR Congo",
      country == "Ethiopia"                     ~ "Ethiopia",
      country == "India"                        ~ "India",
      country == "Nigeria"                      ~ "Nigeria",
      country == "China"                        ~ "China",
      country == "Brazil"                       ~ "Brazil",
      country == "Norway"                       ~ "Norway",
      country == "Saudi Arabia"                 ~ "Saudi Arabia",
      TRUE ~ NA_character_
    )
  )

### |- per-panel correlation labels ----
panel_cors <- energy_2022 |>
  group_by(income_group) |>
  summarise(
    r     = cor(log10(gdp_per_capita), fossil_share_elec, use = "complete.obs"),
    n     = n(),
    label = glue("r = {sprintf('%.2f', r)}  (n = {n})")
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(palette = c(
  "Low income"           = "#2E1122",   
  "Lower middle income"  = "#8B1A2A",   
  "Upper middle income"  = "#246B7D",   
  "High income"          = "#1B3A52"    
))

### |- titles and caption ----
title_text    <- "Fossil Dependence Rises Through Middle Income — Then Breaks Down at High Income"

subtitle_text <- str_glue(
  "Fossil fuels' share of electricity vs. GDP per capita (2022), by World Bank income group.<br>",
  "The relationship strengthens through middle income, then disappears among high-income economies."
)
caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 15,
  source_text = "Our World in Data — Energy Data (Ember / Energy Institute, 2022); World Bank FY2024 Income Classification"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # strip labels
    strip.text   = element_text(
      family = fonts$text, size = 11, face = "bold",
      margin = margin(b = 6)
    ),
    # axes
    axis.title = element_text(family = fonts$text, size = 9, color = "gray50"),
    axis.text = element_text(family = fonts$text, size = 8, color = "gray40"),
    axis.ticks = element_blank(),
    # grid — horizontal only, very faint
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # panel spacing — more breathing room
    panel.spacing.x = unit(2.0, "lines"),
    panel.spacing.y = unit(1.8, "lines"),
    # plot margins
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(energy_2022, aes(x = gdp_per_capita, y = fossil_share_elec)) +
  
  # Geoms
  geom_smooth(
    method = "lm", formula = y ~ x,
    se = TRUE,
    color = "#888888",
    fill  = "gray80",
    linewidth = 0.8,
    alpha = 0.20
  ) +
  geom_point(
    aes(fill = income_group),
    shape = 21,
    sizw = 2.8,
    color = "white",
    stroke = 0.35,
    alpha = 0.80
  ) +
  geom_text_repel(
    aes(label = label_flag),
    family = fonts$text,
    size = 2.6,
    color = "gray25",
    segment.color = "gray65",
    segment.size = 0.3,
    box.padding = 0.4,
    max.overlaps = 10,
    na.rm = TRUE, 
    seed = 42 
  ) +
  geom_text(
    data = panel_cors,
    aes(label = label),
    x = Inf, y = Inf,
    hjust = 1.1, vjust = 1.6,
    family = fonts$text,
    size = 2.8,
    color = "gray45",
    inherit.aes = FALSE
  ) +
  
  # Facets
  facet_wrap(~ income_group, nrow = 2, scales = "free_x") +
  
  # Scales
  scale_x_log10(
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = c(500, 1000, 5000, 10000, 50000, 100000)
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1)
  ) +
  scale_fill_manual(
    values = colors$palette[income_levels]
    ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "GDP per Capita (2017 international $, log scale)",
    y = "Fossil Fuels Share of Electricity (%)",
    fill = NULL
  ) +
  
  guides(fill = "none") +
  # Theme
  theme(
    legend.position = "none",
    plot.title = element_textbox_simple(
      family = fonts$title, size = 20, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$text, size = 11, color = "gray30",
      lineheight = 1.4,
      margin = margin(b = 25)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$text, size = 7, color = "gray50",
      margin = margin(t = 10)
    )
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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-25
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpYvT6s1/file2dac7bc5a47". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
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
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.3.1)
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# Matrix         1.5-4.1  2023-05-18 [2] CRAN (R 4.3.1)
# P methods      * 4.3.1    2023-06-16 [2] local
# mgcv           1.8-42   2023-03-02 [2] CRAN (R 4.3.1)
# nlme           3.1-162  2023-01-31 [2] CRAN (R 4.3.1)
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
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
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.3.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P splines        4.3.1    2023-06-16 [2] local
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
# ──────────────────────────────────────────────────────────────────