
## Challenge: #30DayChartChallenge 2026 
## Prompt:    Day 07 | Distributions | Multiscale
## Topic:     The Energy Transition Looks Different at Every Scale

## Author:    Steven Ponce
## Date:      2026-04-07

## NOTE:      This script uses custom helper functions for theming and formatting.
##            See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data:      Our World in Data – Energy Dataset
##            https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
##            Saved locally: 2026/data/owid-energy-data.csv
##
##            World Bank FY2024 Income Classification (CLASS.xlsx)
##            https://databank.worldbank.org/data/download/site-content/CLASS.xlsx


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, patchwork,     
  janitor, scales, glue, ggdist, ggrepel, grid        
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
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

# World Bank FY2024 income classification — hardcoded tribble
# Source: https://databank.worldbank.org/data/download/site-content/CLASS.xlsx
wb_income <- tribble(
  ~country,                          ~income_group,
  "Afghanistan",                     "Low income",
  "Albania",                         "Upper-middle income",
  "Algeria",                         "Lower-middle income",
  "Angola",                          "Lower-middle income",
  "Argentina",                       "Upper-middle income",
  "Armenia",                         "Upper-middle income",
  "Azerbaijan",                      "Upper-middle income",
  "Bangladesh",                      "Lower-middle income",
  "Belarus",                         "Upper-middle income",
  "Belize",                          "Upper-middle income",
  "Benin",                           "Low income",
  "Bhutan",                          "Lower-middle income",
  "Bolivia",                         "Lower-middle income",
  "Bosnia and Herzegovina",          "Upper-middle income",
  "Botswana",                        "Upper-middle income",
  "Brazil",                          "Upper-middle income",
  "Bulgaria",                        "Upper-middle income",
  "Cabo Verde",                      "Lower-middle income",
  "Cambodia",                        "Lower-middle income",
  "Cameroon",                        "Lower-middle income",
  "Central African Republic",        "Low income",
  "Chad",                            "Low income",
  "China",                           "Upper-middle income",
  "Colombia",                        "Upper-middle income",
  "Comoros",                         "Lower-middle income",
  "Congo",                           "Lower-middle income",
  "Costa Rica",                      "Upper-middle income",
  "Cote d'Ivoire",                   "Lower-middle income",
  "Cuba",                            "Upper-middle income",
  "Dem. Rep. Congo",                 "Low income",
  "Dominican Republic",              "Upper-middle income",
  "Ecuador",                         "Upper-middle income",
  "Egypt",                           "Lower-middle income",
  "El Salvador",                     "Lower-middle income",
  "Equatorial Guinea",               "Upper-middle income",
  "Eritrea",                         "Low income",
  "Eswatini",                        "Lower-middle income",
  "Ethiopia",                        "Low income",
  "Fiji",                            "Upper-middle income",
  "Gabon",                           "Upper-middle income",
  "Gambia",                          "Low income",
  "Georgia",                         "Upper-middle income",
  "Ghana",                           "Lower-middle income",
  "Guatemala",                       "Upper-middle income",
  "Guinea",                          "Low income",
  "Guinea-Bissau",                   "Low income",
  "Guyana",                          "Upper-middle income",
  "Haiti",                           "Low income",
  "Honduras",                        "Lower-middle income",
  "India",                           "Lower-middle income",
  "Indonesia",                       "Upper-middle income",
  "Iran",                            "Lower-middle income",
  "Iraq",                            "Upper-middle income",
  "Jamaica",                         "Upper-middle income",
  "Jordan",                          "Upper-middle income",
  "Kazakhstan",                      "Upper-middle income",
  "Kenya",                           "Lower-middle income",
  "Kyrgyzstan",                      "Lower-middle income",
  "Laos",                            "Lower-middle income",
  "Lebanon",                         "Lower-middle income",
  "Lesotho",                         "Lower-middle income",
  "Liberia",                         "Low income",
  "Libya",                           "Upper-middle income",
  "Madagascar",                      "Low income",
  "Malawi",                          "Low income",
  "Malaysia",                        "Upper-middle income",
  "Maldives",                        "Upper-middle income",
  "Mali",                            "Low income",
  "Marshall Islands",                "Upper-middle income",
  "Mauritania",                      "Lower-middle income",
  "Mauritius",                       "Upper-middle income",
  "Mexico",                          "Upper-middle income",
  "Micronesia",                      "Lower-middle income",
  "Mongolia",                        "Lower-middle income",
  "Montenegro",                      "Upper-middle income",
  "Morocco",                         "Lower-middle income",
  "Mozambique",                      "Low income",
  "Myanmar",                         "Lower-middle income",
  "Namibia",                         "Upper-middle income",
  "Nepal",                           "Lower-middle income",
  "Nicaragua",                       "Lower-middle income",
  "Niger",                           "Low income",
  "Nigeria",                         "Lower-middle income",
  "North Macedonia",                 "Upper-middle income",
  "Pakistan",                        "Lower-middle income",
  "Papua New Guinea",                "Lower-middle income",
  "Paraguay",                        "Upper-middle income",
  "Peru",                            "Upper-middle income",
  "Philippines",                     "Lower-middle income",
  "Rwanda",                          "Low income",
  "Samoa",                           "Lower-middle income",
  "Sao Tome and Principe",           "Lower-middle income",
  "Senegal",                         "Lower-middle income",
  "Serbia",                          "Upper-middle income",
  "Sierra Leone",                    "Low income",
  "Solomon Islands",                 "Lower-middle income",
  "Somalia",                         "Low income",
  "South Africa",                    "Upper-middle income",
  "South Sudan",                     "Low income",
  "Sri Lanka",                       "Lower-middle income",
  "Sudan",                           "Low income",
  "Suriname",                        "Upper-middle income",
  "Syrian Arab Republic",            "Low income",
  "Tajikistan",                      "Lower-middle income",
  "Tanzania",                        "Low income",
  "Thailand",                        "Upper-middle income",
  "Timor-Leste",                     "Lower-middle income",
  "Togo",                            "Low income",
  "Tonga",                           "Upper-middle income",
  "Tunisia",                         "Lower-middle income",
  "Turkey",                          "Upper-middle income",
  "Turkmenistan",                    "Upper-middle income",
  "Tuvalu",                          "Upper-middle income",
  "Uganda",                          "Low income",
  "Ukraine",                         "Lower-middle income",
  "Uzbekistan",                      "Lower-middle income",
  "Vanuatu",                         "Lower-middle income",
  "Venezuela",                       "Upper-middle income",
  "Vietnam",                         "Lower-middle income",
  "West Bank and Gaza",              "Lower-middle income",
  "Yemen",                           "Low income",
  "Zambia",                          "Lower-middle income",
  "Zimbabwe",                        "Lower-middle income",
  # High income
  "Australia",                       "High income",
  "Austria",                         "High income",
  "Bahrain",                         "High income",
  "Belgium",                         "High income",
  "Canada",                          "High income",
  "Chile",                           "High income",
  "Croatia",                         "High income",
  "Cyprus",                          "High income",
  "Czech Republic",                  "High income",
  "Denmark",                         "High income",
  "Estonia",                         "High income",
  "Finland",                         "High income",
  "France",                          "High income",
  "Germany",                         "High income",
  "Greece",                          "High income",
  "Hong Kong",                       "High income",
  "Hungary",                         "High income",
  "Iceland",                         "High income",
  "Ireland",                         "High income",
  "Israel",                          "High income",
  "Italy",                           "High income",
  "Japan",                           "High income",
  "Kuwait",                          "High income",
  "Latvia",                          "High income",
  "Lithuania",                       "High income",
  "Luxembourg",                      "High income",
  "Malta",                           "High income",
  "Netherlands",                     "High income",
  "New Zealand",                     "High income",
  "Norway",                          "High income",
  "Oman",                            "High income",
  "Poland",                          "High income",
  "Portugal",                        "High income",
  "Qatar",                           "High income",
  "Romania",                         "High income",
  "Saudi Arabia",                    "High income",
  "Singapore",                       "High income",
  "Slovakia",                        "High income",
  "Slovenia",                        "High income",
  "South Korea",                     "High income",
  "Spain",                           "High income",
  "Sweden",                          "High income",
  "Switzerland",                     "High income",
  "Taiwan",                          "High income",
  "Trinidad and Tobago",             "High income",
  "United Arab Emirates",            "High income",
  "United Kingdom",                  "High income",
  "United States",                   "High income",
  "Uruguay",                         "High income"
)


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)


## 4. TIDY DATA ----

### |- filter to 2022, countries only, join income classification ----
energy_2022 <- energy_raw |>
  filter(
    year == 2022,
    !country %in% c(
      "World", "Asia", "Europe", "Africa", "North America",
      "South America", "Oceania", "European Union (27)",
      "High-income countries", "Low-income countries",
      "Lower-middle-income countries", "Upper-middle-income countries",
      "OECD (Ember)", "Non-OECD (Ember)"
    )
  ) |>
  select(country, year, renewables_share_elec) |>
  filter(!is.na(renewables_share_elec)) |>
  left_join(wb_income, by = "country")

### |- panel 1: global distribution (all countries with income classification) ----
p1_data <- energy_2022 |>
  filter(!is.na(income_group)) |>
  mutate(group = "Global")

### |- panel 2: by income group ----
income_order <- c(
  "Low income",
  "Lower-middle income",
  "Upper-middle income",
  "High income"
)

p2_data <- energy_2022 |>
  filter(!is.na(income_group)) |>
  mutate(income_group = factor(income_group, levels = income_order))

### |- panel 3: upper-middle income countries, labeled highlights ----
label_countries <- c("Brazil", "South Africa", "China", "Turkey")

p3_data <- energy_2022 |>
  filter(income_group == "Upper-middle income") |>
  arrange(renewables_share_elec) |>
  mutate(
    country_label = if_else(country %in% label_countries, country, NA_character_),
    rank          = row_number()
  )

### |- compute medians for annotations ----
group_medians <- p2_data |>
  group_by(income_group) |>
  summarise(
    median_val = median(renewables_share_elec, na.rm = TRUE),
    .groups = "drop"
  )

global_median <- median(p1_data$renewables_share_elec, na.rm = TRUE)
p3_median <- median(p3_data$renewables_share_elec, na.rm = TRUE)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "Low income"           = "#8B1A2A",  
    "Lower-middle income"  = "#C0434E",  
    "Upper-middle income"  = "#B08090", 
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
title_text <- "The Energy Transition Looks Different at Every Scale"

subtitle_text <- "Global distributions mask large differences across income groups—and even wider gaps between\ncountries within the same group (2022)"

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 07,
  source_text = "Our World in Data · Energy Dataset (Ember / Energy Institute)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = colors$palette$bg, color = NA),
    panel.background = element_rect(fill = colors$palette$bg, color = NA),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(
      family = fonts$body,
      size   = 9,
      color  = colors$palette$label_text
    ),
    axis.title = element_text(
      family = fonts$body,
      size   = 9.5,
      color  = colors$palette$label_text
    ),
    strip.text = element_blank(),
    panel.spacing = unit(12, "pt"),
    plot.margin = margin(10, 16, 10, 16)
  )
)

theme_set(weekly_theme)

### |- panel 1: global histinterval ----
p1 <- ggplot(p1_data, aes(x = renewables_share_elec, y = 0)) +
  stat_histinterval(
    fill           = colors$palette$global_ref,
    slab_color     = NA,
    alpha          = 0.55,
    point_color    = colors$palette$median_dot_stroke,
    interval_color = colors$palette$iqr_band,
    point_size     = 2.8,
    breaks         = 20
  ) +
  geom_vline(
    xintercept = global_median,
    linetype   = "dashed",
    color      = colors$palette$median_dot_stroke,
    linewidth  = 0.5
  ) +
  annotate(
    "text",
    x      = global_median + 2,
    y      = Inf,
    label  = glue("Median: {round(global_median, 1)}%"),
    hjust  = 0,
    vjust  = 1.4,
    size   = 3,
    family = fonts$body,
    color  = colors$palette$median_label
  ) +
  annotate(
    "text",
    x        = 97,
    y        = Inf,
    label    = "GLOBAL",
    hjust    = 1,
    vjust    = 1.4,
    size     = 3.2,
    fontface = "bold",
    family   = fonts$body,
    color    = colors$palette$label_text
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1, suffix = "%"),
    expand = c(0.01, 0)
  ) +
  labs(
    x = "Renewable electricity share (%)",
    y = NULL
  ) +
  theme(
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )

### |- panel 2: by income group — halfeye ----
p2 <- ggplot(p2_data, aes(x = renewables_share_elec, y = fct_rev(income_group))) +
  stat_halfeye(
    aes(
      fill  = income_group,
      alpha = income_group == "Upper-middle income"
    ),
    .width = c(0.5, 0.9),
    point_color = colors$palette$median_dot_stroke,
    point_fill = colors$palette$median_dot_fill,
    interval_color = colors$palette$iqr_band,
    slab_color = NA,
    normalize = "all",
    scale = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "Low income"           = colors$palette$`Low income`,
      "Lower-middle income"  = colors$palette$`Lower-middle income`,
      "Upper-middle income"  = colors$palette$`Upper-middle income`,
      "High income"          = colors$palette$`High income`
    )
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.45),
    guide  = "none"
  ) +
  geom_text(
    data = group_medians,
    aes(
      x = median_val + 3,
      y = fct_rev(income_group),
      label = glue("{round(median_val, 0)}%")
    ),
    hjust = 0,
    size = 2.8,
    family = fonts$body,
    color = colors$palette$median_label,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x        = 95,
    y        = 4.45,
    label    = "BY INCOME GROUP",
    hjust    = 1,
    size     = 3.2,
    fontface = "bold",
    family   = fonts$body,
    color    = colors$palette$label_text
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1, suffix = "%"),
    expand = c(0.01, 0)
  ) +
  labs(
    x = "Renewable electricity share (%)",
    y = NULL
  ) +
  guides(fill = "none")

### |- panel 3: upper-middle income — quantile dot plot ----
p3 <- ggplot(p3_data, aes(x = renewables_share_elec, y = 0)) +
  stat_dots(
    fill   = colors$palette$`Upper-middle income`,
    color  = NA,
    alpha  = 0.9,
    size   = 2.4,
    layout = "bin"
  ) +
  geom_vline(
    xintercept = p3_median,
    linetype   = "dashed",
    color      = colors$palette$median_dot_stroke,
    linewidth  = 0.5
  ) +
  annotate(
    "text",
    x      = p3_median + 2,
    y      = 1.18,
    label  = glue("Median: {round(p3_median, 1)}%"),
    hjust  = 0,
    size   = 2.8,
    family = fonts$body,
    color  = colors$palette$median_label
  ) +
  geom_label_repel(
    data = p3_data |> filter(!is.na(country_label)),
    aes(x = renewables_share_elec, y = 0, label = country_label),
    nudge_y = 0.42,
    size = 2.8,
    family = fonts$body,
    color = colors$palette$label_text,
    fill = colors$palette$bg,
    label.size = 0.2,
    label.padding = unit(0.15, "lines"),
    min.segment.length = 0.3,
    segment.color = "gray60",
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x        = 95,
    y        = 1.1,
    label    = "UPPER-MIDDLE INCOME",
    hjust    = 1,
    size     = 3.2,
    fontface = "bold",
    family   = fonts$body,
    color    = colors$palette$label_text
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1, suffix = "%"),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.5, 1.4)
  ) +
  labs(
    x = "Renewable electricity share (%)",
    y = NULL
  ) +
  theme(
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )

### |- combined plots ----
combined_plots <- p1 / p2 / p3 +
  plot_layout(heights = c(1.3, 2.2, 1.3)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        family = fonts$title,
        face   = "bold",
        size   = 22,
        color  = colors$palette$label_text,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        family = fonts$body,
        size   = 11,
        color  = colors$palette$label_text,
        margin = margin(b = 16)
      ),
      plot.caption = element_markdown(
        family = fonts$body,
        size   = 7.5,
        color  = "gray50",
        margin = margin(t = 12),
        hjust = 0,
      ),
      plot.background = element_rect(fill = colors$palette$bg, color = NA),
      plot.margin = margin(20, 20, 12, 20)
    )
  )

snap(combined_plots)


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

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-22
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpAT9ECx/file512453042e8f". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
# ! package        * version  date (UTC) lib source
# base           * 4.3.1    2023-06-16 [?] local
# bit              4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64            4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder        0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli              3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# P compiler         4.3.1    2023-06-16 [2] local
# crayon           1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# P datasets       * 4.3.1    2023-06-16 [2] local
# digest           0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
# distributional   0.7.0    2026-03-17 [1] CRAN (R 4.3.1)
# dplyr          * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# farver           2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# forcats        * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics         0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggdist         * 3.3.3    2025-04-23 [1] CRAN (R 4.3.1)
# ggplot2        * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggrepel        * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
# ggtext         * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gifski           1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# glue           * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics       * 4.3.1    2023-06-16 [2] local
# P grDevices      * 4.3.1    2023-06-16 [2] local
# P grid           * 4.3.1    2023-06-16 [2] local
# gridtext         0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable           0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here           * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms              1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# janitor        * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite         2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# lifecycle        1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# lubridate      * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick           2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# P methods        * 4.3.1    2023-06-16 [2] local
# pacman           0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel         4.3.1    2023-06-16 [2] local
# patchwork      * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar           1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr          * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R.cache          0.16.0   2022-07-21 [1] CRAN (R 4.3.3)
# R.methodsS3      1.8.2    2022-06-13 [1] CRAN (R 4.3.3)
# R.oo             1.27.0   2024-11-01 [1] CRAN (R 4.3.3)
# R.utils          2.13.0   2025-02-24 [1] CRAN (R 4.3.3)
# R6               2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
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
# styler           1.11.0   2025-10-13 [1] CRAN (R 4.3.1)
# svglite          2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts       * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts      1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
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
# xml2             1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────