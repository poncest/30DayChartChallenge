
## Challenge: #30DayChartChallenge 2026 — Day 03
## Prompt:    Comparisons | Mosaic 
##
## Title:     The Energy Transition Is Uneven
## Data:      Our World in Data — Energy Data (OWID)
##            World Bank Income Classifications
##
## Author:    Steven Ponce
## Date:      2026-04-03

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data:      Our World in Data — Electricity Mix (Ember / Energy Institute, 2023)
##            https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,     
  janitor, scales, glue, ggmosaic           
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
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

### |- World Bank income classification ----
# Source: World Bank Country and Lending Groups (FY2024)
# Full list: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
# Download:  https://databank.worldbank.org/data/download/site-content/CLASS.xlsx
# Note: Subset of ~130 major electricity-producing countries used here.
#       Join key: wb_income$country → energy_2022$country

### |- World Bank income classification ----
wb_income <- tribble(
  ~country,                       ~income_group,
  # Low income
  "Afghanistan",                  "Low income",
  "Burkina Faso",                 "Low income",
  "Burundi",                      "Low income",
  "Central African Republic",     "Low income",
  "Chad",                         "Low income",
  "Democratic Republic of Congo", "Low income",
  "Eritrea",                      "Low income",
  "Ethiopia",                     "Low income",
  "Guinea",                       "Low income",
  "Guinea-Bissau",                "Low income",
  "Liberia",                      "Low income",
  "Madagascar",                   "Low income",
  "Malawi",                       "Low income",
  "Mali",                         "Low income",
  "Mozambique",                   "Low income",
  "Niger",                        "Low income",
  "Rwanda",                       "Low income",
  "Sierra Leone",                 "Low income",
  "Somalia",                      "Low income",
  "South Sudan",                  "Low income",
  "Sudan",                        "Low income",
  "Syria",                        "Low income",
  "Togo",                         "Low income",
  "Uganda",                       "Low income",
  "Yemen",                        "Low income",
  "Zimbabwe",                     "Low income",
  # Lower-middle income
  "Bangladesh",                   "Lower-middle income",
  "Bolivia",                      "Lower-middle income",
  "Cambodia",                     "Lower-middle income",
  "Cameroon",                     "Lower-middle income",
  "Congo",                        "Lower-middle income",
  "Cote d'Ivoire",                "Lower-middle income",
  "Egypt",                        "Lower-middle income",
  "El Salvador",                  "Lower-middle income",
  "Ghana",                        "Lower-middle income",
  "Haiti",                        "Lower-middle income",
  "Honduras",                     "Lower-middle income",
  "India",                        "Lower-middle income",
  "Indonesia",                    "Lower-middle income",
  "Kenya",                        "Lower-middle income",
  "Kyrgyzstan",                   "Lower-middle income",
  "Laos",                         "Lower-middle income",
  "Lesotho",                      "Lower-middle income",
  "Mauritania",                   "Lower-middle income",
  "Morocco",                      "Lower-middle income",
  "Myanmar",                      "Lower-middle income",
  "Nepal",                        "Lower-middle income",
  "Nicaragua",                    "Lower-middle income",
  "Nigeria",                      "Lower-middle income",
  "Pakistan",                     "Lower-middle income",
  "Papua New Guinea",             "Lower-middle income",
  "Philippines",                  "Lower-middle income",
  "Senegal",                      "Lower-middle income",
  "Sri Lanka",                    "Lower-middle income",
  "Tanzania",                     "Lower-middle income",
  "Tunisia",                      "Lower-middle income",
  "Ukraine",                      "Lower-middle income",
  "Uzbekistan",                   "Lower-middle income",
  "Vietnam",                      "Lower-middle income",
  "Zambia",                       "Lower-middle income",
  # Upper-middle income
  "Albania",                      "Upper-middle income",
  "Algeria",                      "Upper-middle income",
  "Argentina",                    "Upper-middle income",
  "Armenia",                      "Upper-middle income",
  "Azerbaijan",                   "Upper-middle income",
  "Belarus",                      "Upper-middle income",
  "Belize",                       "Upper-middle income",
  "Bosnia and Herzegovina",       "Upper-middle income",
  "Botswana",                     "Upper-middle income",
  "Brazil",                       "Upper-middle income",
  "Bulgaria",                     "Upper-middle income",
  "China",                        "Upper-middle income",
  "Colombia",                     "Upper-middle income",
  "Costa Rica",                   "Upper-middle income",
  "Cuba",                         "Upper-middle income",
  "Dominican Republic",           "Upper-middle income",
  "Ecuador",                      "Upper-middle income",
  "Equatorial Guinea",            "Upper-middle income",
  "Fiji",                         "Upper-middle income",
  "Gabon",                        "Upper-middle income",
  "Georgia",                      "Upper-middle income",
  "Guatemala",                    "Upper-middle income",
  "Iran",                         "Upper-middle income",
  "Iraq",                         "Upper-middle income",
  "Jamaica",                      "Upper-middle income",
  "Jordan",                       "Upper-middle income",
  "Kazakhstan",                   "Upper-middle income",
  "Kosovo",                       "Upper-middle income",
  "Libya",                        "Upper-middle income",
  "Malaysia",                     "Upper-middle income",
  "Maldives",                     "Upper-middle income",
  "Mexico",                       "Upper-middle income",
  "Moldova",                      "Upper-middle income",
  "Montenegro",                   "Upper-middle income",
  "Namibia",                      "Upper-middle income",
  "North Macedonia",              "Upper-middle income",
  "Paraguay",                     "Upper-middle income",
  "Peru",                         "Upper-middle income",
  "Russia",                       "Upper-middle income",
  "Serbia",                       "Upper-middle income",
  "South Africa",                 "Upper-middle income",
  "Thailand",                     "Upper-middle income",
  "Turkmenistan",                 "Upper-middle income",
  "Turkey",                       "Upper-middle income",
  "Venezuela",                    "Upper-middle income",
  # High income
  "Australia",                    "High income",
  "Austria",                      "High income",
  "Bahrain",                      "High income",
  "Belgium",                      "High income",
  "Canada",                       "High income",
  "Chile",                        "High income",
  "Croatia",                      "High income",
  "Cyprus",                       "High income",
  "Czech Republic",               "High income",
  "Denmark",                      "High income",
  "Estonia",                      "High income",
  "Finland",                      "High income",
  "France",                       "High income",
  "Germany",                      "High income",
  "Greece",                       "High income",
  "Hungary",                      "High income",
  "Iceland",                      "High income",
  "Ireland",                      "High income",
  "Israel",                       "High income",
  "Italy",                        "High income",
  "Japan",                        "High income",
  "South Korea",                  "High income",
  "Kuwait",                       "High income",
  "Latvia",                       "High income",
  "Lithuania",                    "High income",
  "Luxembourg",                   "High income",
  "Malta",                        "High income",
  "Netherlands",                  "High income",
  "New Zealand",                  "High income",
  "Norway",                       "High income",
  "Oman",                         "High income",
  "Poland",                       "High income",
  "Portugal",                     "High income",
  "Qatar",                        "High income",
  "Romania",                      "High income",
  "Saudi Arabia",                 "High income",
  "Singapore",                    "High income",
  "Slovakia",                     "High income",
  "Slovenia",                     "High income",
  "Spain",                        "High income",
  "Sweden",                       "High income",
  "Switzerland",                  "High income",
  "Trinidad and Tobago",          "High income",
  "United Arab Emirates",         "High income",
  "United Kingdom",               "High income",
  "United States",                "High income",
  "Uruguay",                      "High income"
)


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)


## 4. TIDY DATA ----

### |- Filter to 2022 and relevant columns ----
energy_2022 <- energy_raw |>
  filter(year == 2022) |>
  select(
    country,
    coal_electricity, oil_electricity, gas_electricity,
    hydro_electricity, wind_electricity, solar_electricity,
    nuclear_electricity
  ) |>
  filter(!str_detect(
    country,
    "World|Africa|Asia|Europe|America|OECD|income|Non-OECD|Asia Pacific|Middle East|CIS|G20|European Union|High-income|Low-income|Lower-middle|Upper-middle|OPEC|Eurasia"
  ))

### |- Join World Bank income classification ----
energy_income <- energy_2022 |>
  inner_join(wb_income, by = "country")

### |- Compute energy source totals per country ----
energy_sources <- energy_income |>
  mutate(
    fossil            = coal_electricity + oil_electricity + gas_electricity,
    hydro             = hydro_electricity,
    modern_renewables = wind_electricity + solar_electricity,
    nuclear           = nuclear_electricity
  ) |>
  select(country, income_group, fossil, hydro, modern_renewables, nuclear) |>
  mutate(across(fossil:nuclear, ~ replace_na(.x, 0)))

### |- Pivot to long format ----
energy_long <- energy_sources |>
  pivot_longer(
    cols      = fossil:nuclear,
    names_to  = "source",
    values_to = "twh"
  )

### |- Factor ordering ----
income_order <- c(
  "Low income", "Lower-middle income",
  "Upper-middle income", "High income"
)
source_order <- c("fossil", "hydro", "modern_renewables", "nuclear")

### |- Aggregate to income group level ----
energy_agg <- energy_long |>
  mutate(
    income_group = factor(income_group, levels = income_order),
    source       = factor(source, levels = source_order)
  ) |>
  group_by(income_group, source) |>
  summarise(total_twh = sum(twh), .groups = "drop") |>
  tidyr::complete(income_group, source, fill = list(total_twh = 0)) |>
  group_by(income_group) |>
  mutate(
    group_total = sum(total_twh),
    share       = if_else(group_total > 0, total_twh / group_total, 0)
  ) |>
  ungroup()

### |- Pre-compute tile coordinates for manual labels ----
group_totals <- energy_agg |>
  distinct(income_group, group_total) |>
  arrange(income_group) |>
  mutate(
    grand_total = sum(group_total),
    x_width     = group_total / grand_total,
    x_max       = cumsum(x_width),
    x_min       = x_max - x_width,
    x_mid       = (x_min + x_max) / 2
  )

grand_total <- sum(group_totals$group_total)

low_income_xmax <- group_totals |>
  filter(income_group == "Low income") |>
  pull(x_max)

upper_middle_xmax <- group_totals |>
  filter(income_group == "Upper-middle income") |>
  pull(x_max)

tile_coords <- energy_agg |>
  arrange(income_group, source) |>
  group_by(income_group) |>
  mutate(
    y_max = cumsum(share),
    y_min = y_max - share,
    y_mid = (y_min + y_max) / 2
  ) |>
  ungroup() |>
  left_join(
    group_totals |> select(income_group, x_min, x_max, x_mid, x_width),
    by = "income_group"
  ) |>
  mutate(
    global_share = group_total / grand_total * share,
     label        = if_else(global_share >= 0.018,
                           percent(global_share, accuracy = 1), ""),
    label_color  = if_else(source == "nuclear", "gray20", "white")
  )

### |- Plot data ----
set.seed(2026)
mosaic_data <- energy_agg |>
  mutate(units = round(total_twh / 10)) |>
  filter(units > 0) |>
  uncount(units)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "fossil"            = "#7A1E3A",
    "hydro"             = "#4F81BD",
    "modern_renewables" = "#2CA25F",
    "nuclear"           = "#FDB863"
  )
)

### |- titles and caption ----
title_text <- str_glue("The Energy Transition Is Uneven")

subtitle_text <- str_glue(
  "**High-income countries** generate a large share of the world's electricity with a more diversified mix,<br>",
  "while **upper-middle-income countries** still contain the largest fossil block globally. ",
  "Income groups use World Bank 2022 classifications.<br>",
  "<span style='color:#808080;'>Tile width = share of global generation. ",
  "Percentages shown for tiles ≥ 2% of global output; low-income nations produce < 1% collectively.</span>"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 03,
  source_text = "Our World in Data · Energy Institute Statistical Review of World Energy"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_text(
      face = "bold", family = fonts$title, color = colors$title,
      size = rel(1.75), margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$subtitle, size = rel(0.75), color = colors$subtitle,
      lineheight = 1.1, margin = margin(t = 5, b = 18)
    ),
    plot.caption = element_markdown(
      family = fonts$caption, , size = rel(0.55), color = colors$caption,
      margin = margin(t = 16), hjust = 0, lineheight = 1.3,
    ),

    # Legend
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(family = fonts$text, size = rel(0.8)),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.x = unit(0.35, "cm"),

    # Axes
    axis.title.x = element_text(
      family = fonts$text, size = rel(0.8),
      margin = margin(t = 10), color = "gray30"
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      family = fonts$text, size = rel(0.75), color = "gray30"
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),

    # Panel
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(20, 30, 20, 30)
  )
)

theme_set(weekly_theme)

### |- main plot ----
set.seed(2026)

p <- ggplot(mosaic_data) +
  # Geoms
  geom_mosaic(
    aes(
      x    = product(source, income_group),
      fill = source
    ),
    color     = "white",
    linewidth = 0.6,
    na.rm     = TRUE
  ) +
  geom_text(
    data        = tile_coords |> filter(label != ""),
    aes(x = x_mid, y = y_mid, label = label, color = label_color),
    size        = 3,
    fontface    = "bold",
    inherit.aes = FALSE
  ) +
  # Annotate
  annotate(
    "segment",
    x    = upper_middle_xmax,
    xend = upper_middle_xmax,
    y    = 0,
    yend = 1,
    color     = "gray60",
    linewidth = 0.4,
    linetype  = "dashed"
  ) +
  annotate(
    "text",
    x     = upper_middle_xmax + 0.005,
    y     = 1.03,
    label = "Mix diversifies",
    hjust = 0,
    size  = 2.5,
    color = "gray45"
  ) +
  # Scales
  scale_color_identity() +
  scale_fill_manual(
    values = colors$palette,
    labels = c(
      "fossil"            = "Fossil fuels",
      "hydro"             = "Hydro",
      "modern_renewables" = "Wind + solar",
      "nuclear"           = "Nuclear"
    )
  ) +
  scale_x_productlist(
    labels = c(
      "Low income"          = "Low\nIncome",
      "Lower-middle income" = "Lower-\nMiddle",
      "Upper-middle income" = "Upper-\nMiddle",
      "High income"         = "High\nIncome"
    )
  ) +
  scale_y_productlist(labels = NULL) +
  coord_cartesian(clip = "off") +   
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "World Bank income group"
  ) +
  guides(
    fill = guide_legend(
      nrow         = 1,
      override.aes = list(color = NA)
    )
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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpIrO4LT/file63d07df13017". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────
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
# data.table     1.18.2.1 2026-01-27 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggmosaic     * 0.4.0    2026-03-19 [1] Github (haleyjeppson/ggmosaic@4b71443)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggrepel        0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
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
# htmlwidgets    1.6.4    2023-12-06 [1] CRAN (R 4.3.3)
# httr           1.4.8    2026-02-13 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# lazyeval       0.2.2    2019-03-15 [1] CRAN (R 4.3.3)
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
# plotly         4.12.0   2026-01-24 [1] CRAN (R 4.3.1)
# plyr           1.8.9    2023-10-02 [1] CRAN (R 4.3.3)
# productplots   0.1.2    2025-10-27 [1] CRAN (R 4.3.1)
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
# viridisLite    0.4.3    2026-02-04 [1] CRAN (R 4.3.1)
# vroom          1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.

# ───────────────────────────────────────────────────────────────────────
