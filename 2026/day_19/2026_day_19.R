
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 19 · Timeseries | Evolution

## Topic:     The Renewable Energy Cost Collapse (2010–2024)
## Author:    Steven Ponce
## Date:      2026-04-19

## NOTE: This script uses custom helper functions from R/utils/ and R/themes/
##       See HELPER FUNCTIONS DOCUMENTATION at the end for details.
##       Source: IRENA Renewable Power Generation Costs reports (2010–2024)
##       URL: https://www.irena.org/Energy-Transition/Technology/Power-generation-costs

## Data: IRENA — International Renewable Energy Agency
##       "Renewable Power Generation Costs" annual reports (2010–2024)
##       URL: https://www.irena.org/Energy-Transition/Technology/Power-generation-costs
##       Data hardcoded from published report tables (global weighted-average LCOE)
##       All values in 2024 USD/kWh | License: CC BY 4.0


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

### |- LCOE data (USD/kWh, 2024 USD) ----
# Source: IRENA Renewable Power Generation Costs reports 2010–2024
# All values are global weighted-average LCOE for newly-commissioned utility-scale plants
# Fossil fuel = weighted-average cost of new fossil fuel-fired capacity

lcoe_raw <- tribble(
  ~year, ~technology,       ~lcoe,
  # Solar PV
  2010,  "Solar PV",        0.417,
  2011,  "Solar PV",        0.360,
  2012,  "Solar PV",        0.280,
  2013,  "Solar PV",        0.200,
  2014,  "Solar PV",        0.165,
  2015,  "Solar PV",        0.122,
  2016,  "Solar PV",        0.100,
  2017,  "Solar PV",        0.086,
  2018,  "Solar PV",        0.068,
  2019,  "Solar PV",        0.057,
  2020,  "Solar PV",        0.057,
  2021,  "Solar PV",        0.048,
  2022,  "Solar PV",        0.049,
  2023,  "Solar PV",        0.044,
  2024,  "Solar PV",        0.043,
  # Onshore Wind
  2010,  "Onshore Wind",    0.111,
  2011,  "Onshore Wind",    0.104,
  2012,  "Onshore Wind",    0.097,
  2013,  "Onshore Wind",    0.090,
  2014,  "Onshore Wind",    0.083,
  2015,  "Onshore Wind",    0.076,
  2016,  "Onshore Wind",    0.066,
  2017,  "Onshore Wind",    0.060,
  2018,  "Onshore Wind",    0.056,
  2019,  "Onshore Wind",    0.053,
  2020,  "Onshore Wind",    0.039,
  2021,  "Onshore Wind",    0.033,
  2022,  "Onshore Wind",    0.033,
  2023,  "Onshore Wind",    0.033,
  2024,  "Onshore Wind",    0.034,
  # Offshore Wind
  2010,  "Offshore Wind",   0.188,
  2011,  "Offshore Wind",   0.198,
  2012,  "Offshore Wind",   0.193,
  2013,  "Offshore Wind",   0.185,
  2014,  "Offshore Wind",   0.178,
  2015,  "Offshore Wind",   0.170,
  2016,  "Offshore Wind",   0.152,
  2017,  "Offshore Wind",   0.140,
  2018,  "Offshore Wind",   0.127,
  2019,  "Offshore Wind",   0.115,
  2020,  "Offshore Wind",   0.084,
  2021,  "Offshore Wind",   0.075,
  2022,  "Offshore Wind",   0.081,
  2023,  "Offshore Wind",   0.076,
  2024,  "Offshore Wind",   0.079,
  # Fossil Fuel benchmark (weighted-average new capacity)
  2010,  "Fossil Fuels",    0.090,
  2011,  "Fossil Fuels",    0.090,
  2012,  "Fossil Fuels",    0.092,
  2013,  "Fossil Fuels",    0.093,
  2014,  "Fossil Fuels",    0.094,
  2015,  "Fossil Fuels",    0.093,
  2016,  "Fossil Fuels",    0.085,
  2017,  "Fossil Fuels",    0.086,
  2018,  "Fossil Fuels",    0.085,
  2019,  "Fossil Fuels",    0.084,
  2020,  "Fossil Fuels",    0.082,
  2021,  "Fossil Fuels",    0.094,
  2022,  "Fossil Fuels",    0.142,
  2023,  "Fossil Fuels",    0.100,
  2024,  "Fossil Fuels",    0.073
)


## 3. EXAMINING THE DATA ----
glimpse(lcoe_raw)


## 4. TIDY DATA ----

### |- factor levels and label data ----
tech_order <- c("Solar PV", "Onshore Wind", "Offshore Wind", "Fossil Fuels")

lcoe <- lcoe_raw |>
  mutate(
    technology = factor(technology, levels = tech_order),
    lcoe_cents = lcoe * 100 # convert to cents/kWh for more readable axis
  )

### |- endpoint labels (2024 values) ----
labels_2024 <- lcoe |>
  filter(year == 2024) |>
  mutate(
    label = glue("{technology}\n${round(lcoe, 3)}/kWh")
  )

### |- compute % decline for annotation ----
solar_2010 <- lcoe |>
  filter(technology == "Solar PV", year == 2010) |>
  pull(lcoe)
solar_2024 <- lcoe |>
  filter(technology == "Solar PV", year == 2024) |>
  pull(lcoe)
solar_pct_decline <- round((1 - solar_2024 / solar_2010) * 100, 0)

wind_2010 <- lcoe |>
  filter(technology == "Onshore Wind", year == 2010) |>
  pull(lcoe)
wind_2024 <- lcoe |>
  filter(technology == "Onshore Wind", year == 2024) |>
  pull(lcoe)
wind_pct_decline <- round((1 - wind_2024 / wind_2010) * 100, 0)

### |- parity crossover: first year solar <= fossil ----
# Used to place the "cost parity" annotation
solar_parity_yr <- lcoe |>
  filter(technology %in% c("Solar PV", "Fossil Fuels")) |>
  select(year, technology, lcoe) |>
  pivot_wider(names_from = technology, values_from = lcoe) |>
  filter(`Solar PV` <= `Fossil Fuels`) |>
  slice_min(year) |>
  pull(year)

wind_parity_yr <- lcoe |>
  filter(technology %in% c("Onshore Wind", "Fossil Fuels")) |>
  select(year, technology, lcoe) |>
  pivot_wider(names_from = technology, values_from = lcoe) |>
  filter(`Onshore Wind` <= `Fossil Fuels`) |>
  slice_min(year) |>
  pull(year)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    solar    = "#F4A261",   
    onshore  = "#2A9D8F",   
    offshore = "#457B9D",  
    fossil   = "#6B6B6B",   
    bg       = "#FAFAF8",  
    parity   = "#E63946"
  )
)

### |- titles and caption ----
title_text    <- "Renewables Became the Cheapest Source of Electricity"
subtitle_text <- "Global levelized cost of energy (LCOE), 2010–2024 · Solar and wind costs fell below<br>
fossil fuels, marking a structural shift in energy economics · 2024 USD/kWh"

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 19,
  source_text = "IRENA · Renewable Power Generation Costs reports (2010–2024)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- technology colors vector ----
tech_colors <- c(
  "Solar PV"      = colors$palette$solar,
  "Onshore Wind"  = colors$palette$onshore,
  "Offshore Wind" = colors$palette$offshore,
  "Fossil Fuels"  = "#3D3D3D"    
)

tech_linewidth <- c(
  "Solar PV"      = 1.4,
  "Onshore Wind"  = 1.4,
  "Offshore Wind" = 1.0,
  "Fossil Fuels"  = 1.3          
)

tech_linetype <- c(
  "Solar PV"      = "solid",
  "Onshore Wind"  = "solid",
  "Offshore Wind" = "solid",
  "Fossil Fuels"  = "dashed"
)

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Background
    plot.background = element_rect(fill = colors$palette$bg, color = NA),
    panel.background = element_rect(fill = colors$palette$bg, color = NA),

    # Grid — horizontal only, very light
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Axes
    axis.ticks = element_blank(),
    axis.text = element_text(family = fonts$text, size = 9, color = "gray40"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = fonts$text, size = 9, color = "gray40",
      margin = margin(r = 8)
    ),

    # Title / subtitle / caption
    plot.title = element_text(
      family = fonts$title, face = "bold",
      size = 18, color = "gray10",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 10.5, color = "gray35",
      lineheight = 1.4, margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = "gray55",
      hjust = 0, margin = margin(t = 16)
    ),
    plot.margin = margin(t = 20, r = 100, b = 16, l = 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(
  data = lcoe,
  mapping = aes(
    x = year, y = lcoe, color = technology,
    linewidth = technology, linetype = technology
  )
) +

  # Geoms
  geom_vline(
    xintercept = solar_parity_yr,
    color = colors$palette$solar,
    linetype = "dotted",
    linewidth = 0.55,
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = wind_parity_yr,
    color = colors$palette$onshore,
    linetype = "dotted",
    linewidth = 0.55,
    alpha = 0.7
  ) +
  geom_line(lineend = "round") +
  geom_point(
    data = labels_2024,
    size = 3,
    shape = 21,
    fill = colors$palette$bg,
    stroke = 1.4
  ) +

  # Annotate
  annotate(
    "text",
    x = 2010.2,
    y = lcoe |> filter(technology == "Solar PV", year == 2010) |> pull(lcoe),
    label = "Solar PV",
    hjust = 0,
    vjust = -0.5,
    size = 3,
    family = fonts$text,
    color = colors$palette$solar,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2010.2,
    y = lcoe |> filter(technology == "Offshore Wind", year == 2010) |> pull(lcoe),
    label = "Offshore Wind",
    hjust = 0,
    vjust = 1.6,
    size = 3,
    family = fonts$text,
    color = colors$palette$offshore,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2010.2,
    y = lcoe |> filter(technology == "Onshore Wind", year == 2010) |> pull(lcoe),
    label = "Onshore Wind",
    hjust = 0,
    vjust = -0.5,
    size = 3,
    family = fonts$text,
    color = colors$palette$onshore,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2010.2,
    y = lcoe |> filter(technology == "Fossil Fuels", year == 2010) |> pull(lcoe),
    label = "Fossil Fuels",
    hjust = 0,
    vjust = 1.6,
    size = 3,
    family = fonts$text,
    color = colors$palette$fossil,
    fontface = "bold"
  ) +
  annotate(
    "richtext",
    x = 2021,
    y = 0.43,
    label = glue(
      "<span style='font-size:15pt; color:{colors$palette$solar}'>**\u221290%**</span><br>",
      "<span style='font-size:8pt; color:gray35'>Solar cost decline since 2010</span>"
    ),
    hjust = 0,
    vjust = 1,
    size = 3,
    family = fonts$text,
    fill = NA,
    label.color = NA
  ) +
  annotate(
    "text",
    x = wind_parity_yr + 0.2,
    y = 0.35,
    label = glue("{wind_parity_yr}: Wind becomes\ncost-competitive"),
    hjust = 0,
    vjust = 1,
    size = 2.8,
    family = fonts$text,
    color = colors$palette$onshore,
    lineheight = 1.25
  ) +
  annotate(
    "text",
    x = solar_parity_yr + 0.2,
    y = 0.35,
    label = glue("{solar_parity_yr}: Solar becomes\ncost-competitive"),
    hjust = 0,
    vjust = 1,
    size = 2.8,
    family = fonts$text,
    color = colors$palette$solar,
    lineheight = 1.25
  ) +
  annotate("segment",
    x = 2024, xend = 2024.55,
    y = labels_2024 |> filter(technology == "Offshore Wind") |> pull(lcoe),
    yend = 0.185,
    color = colors$palette$offshore, linewidth = 0.35, alpha = 0.6
  ) +
  annotate("segment",
    x = 2024, xend = 2024.55,
    y = labels_2024 |> filter(technology == "Fossil Fuels") |> pull(lcoe),
    yend = 0.145,
    color = colors$palette$fossil, linewidth = 0.35, linetype = "dashed", alpha = 0.6
  ) +
  annotate("segment",
    x = 2024, xend = 2024.55,
    y = labels_2024 |> filter(technology == "Solar PV") |> pull(lcoe),
    yend = 0.105,
    color = colors$palette$solar, linewidth = 0.35, alpha = 0.6
  ) +
  annotate("segment",
    x = 2024, xend = 2024.55,
    y = labels_2024 |> filter(technology == "Onshore Wind") |> pull(lcoe),
    yend = 0.060,
    color = colors$palette$onshore, linewidth = 0.35, alpha = 0.6
  ) +
  annotate("text",
    x = 2024.6, y = 0.185,
    label = "Offshore Wind",
    hjust = 0, vjust = 0.5, size = 2.9,
    family = fonts$text, color = colors$palette$offshore,
    fontface = "bold"
  ) +
  annotate("text",
    x = 2024.6, y = 0.145,
    label = "Fossil Fuels",
    hjust = 0, vjust = 0.5, size = 2.9,
    family = fonts$text, color = colors$palette$fossil,
    fontface = "bold"
  ) +
  annotate("richtext",
    x = 2024.6, y = 0.105,
    label = glue(
      "<span style='color:{colors$palette$solar}'>**Solar PV**</span>  ",
      "<span style='color:{colors$palette$solar}'>\u2212{solar_pct_decline}%</span>"
    ),
    hjust = 0, vjust = 0.5, size = 2.9,
    family = fonts$text, fill = NA, label.color = NA, color = "gray35"
  ) +
  annotate("richtext",
    x = 2024.6, y = 0.060,
    label = glue(
      "<span style='color:{colors$palette$onshore}'>**Onshore Wind**</span>  ",
      "<span style='color:{colors$palette$onshore}'>\u2212{wind_pct_decline}%</span>"
    ),
    hjust = 0, vjust = 0.5, size = 2.9,
    family = fonts$text, fill = NA, label.color = NA, color = "gray35"
  ) +

  # Scales
  scale_color_manual(values = tech_colors) +
  scale_linewidth_manual(values = tech_linewidth) +
  scale_linetype_manual(values = tech_linetype) +
  scale_x_continuous(
    breaks = seq(2010, 2024, 2),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(
    labels = label_dollar(suffix = "/kWh", accuracy = 0.01),
    breaks = seq(0, 0.40, 0.05),
    limits = c(0, 0.45),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    y = "LCOE (USD/kWh, 2024 real)"
  ) +
  guides(
    color = "none",
    linewidth = "none",
    linetype = "none"
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
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmp8WBaTz/file92f8b231830". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
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
