
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 14 · Relationships | Trade
## Topic:      Global Trade Is Dominated by Manufactured Goods

## Author:     Steven Ponce
## Date:       2026-04-14

## NOTE: This script uses custom helper functions sourced from R/utils/ and R/themes/

## Data source:
##   WTO Statistics Portal — Merchandise trade values annual dataset (bulk download)
##   URL: https://data.wto.org/en/dataset/bulkdownload
##   File: wto_merchandise_values_annual_dataset.csv
##   Save as: 2026/data/wto_merchandise_values_annual_dataset.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext,  
  janitor, scales, glue, ggalluvial
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

### |- WTO merchandise exports bulk CSV ----
wto_raw <- read_csv(
  here::here("2026/data/wto_merchandise_values_annual_dataset.csv"),
  locale = locale(encoding = "latin1")
) |>
  clean_names()


# 3. EXAMINING THE DATA ----
glimpse(wto_raw)


## 4. TIDY DATA ----

### |- OWID continent lookup ----
owid_regions <- tribble(
  ~iso3,  ~region,
  # Africa
  "DZA", "Africa", "AGO", "Africa", "BEN", "Africa", "BWA", "Africa",
  "BFA", "Africa", "BDI", "Africa", "CPV", "Africa", "CMR", "Africa",
  "CAF", "Africa", "TCD", "Africa", "COM", "Africa", "COD", "Africa",
  "COG", "Africa", "CIV", "Africa", "DJI", "Africa", "EGY", "Africa",
  "GNQ", "Africa", "ERI", "Africa", "SWZ", "Africa", "ETH", "Africa",
  "GAB", "Africa", "GMB", "Africa", "GHA", "Africa", "GIN", "Africa",
  "GNB", "Africa", "KEN", "Africa", "LSO", "Africa", "LBR", "Africa",
  "LBY", "Africa", "MDG", "Africa", "MWI", "Africa", "MLI", "Africa",
  "MRT", "Africa", "MUS", "Africa", "MAR", "Africa", "MOZ", "Africa",
  "NAM", "Africa", "NER", "Africa", "NGA", "Africa", "RWA", "Africa",
  "STP", "Africa", "SEN", "Africa", "SLE", "Africa", "SOM", "Africa",
  "ZAF", "Africa", "SSD", "Africa", "SDN", "Africa", "TZA", "Africa",
  "TGO", "Africa", "TUN", "Africa", "UGA", "Africa", "ZMB", "Africa",
  "ZWE", "Africa",
  # Americas
  "ARG", "Americas", "BHS", "Americas", "BRB", "Americas", "BLZ", "Americas",
  "BOL", "Americas", "BRA", "Americas", "CAN", "Americas", "CHL", "Americas",
  "COL", "Americas", "CRI", "Americas", "CUB", "Americas", "DOM", "Americas",
  "ECU", "Americas", "SLV", "Americas", "GTM", "Americas", "GUY", "Americas",
  "HTI", "Americas", "HND", "Americas", "JAM", "Americas", "MEX", "Americas",
  "NIC", "Americas", "PAN", "Americas", "PRY", "Americas", "PER", "Americas",
  "TTO", "Americas", "USA", "Americas", "URY", "Americas", "VEN", "Americas",
  # Asia
  "AFG", "Asia", "ARM", "Asia", "AZE", "Asia", "BGD", "Asia",
  "BTN", "Asia", "BRN", "Asia", "KHM", "Asia", "CHN", "Asia",
  "GEO", "Asia", "IND", "Asia", "IDN", "Asia", "IRN", "Asia",
  "IRQ", "Asia", "ISR", "Asia", "JPN", "Asia", "JOR", "Asia",
  "KAZ", "Asia", "PRK", "Asia", "KOR", "Asia", "KWT", "Asia",
  "KGZ", "Asia", "LAO", "Asia", "LBN", "Asia", "MYS", "Asia",
  "MDV", "Asia", "MNG", "Asia", "MMR", "Asia", "NPL", "Asia",
  "OMN", "Asia", "PAK", "Asia", "PSE", "Asia", "PHL", "Asia",
  "QAT", "Asia", "SAU", "Asia", "SGP", "Asia", "LKA", "Asia",
  "SYR", "Asia", "TWN", "Asia", "TJK", "Asia", "THA", "Asia",
  "TLS", "Asia", "TKM", "Asia", "ARE", "Asia", "UZB", "Asia",
  "VNM", "Asia", "YEM", "Asia",
  # Europe
  "ALB", "Europe", "AND", "Europe", "AUT", "Europe", "BLR", "Europe",
  "BEL", "Europe", "BIH", "Europe", "BGR", "Europe", "HRV", "Europe",
  "CYP", "Europe", "CZE", "Europe", "DNK", "Europe", "EST", "Europe",
  "FIN", "Europe", "FRA", "Europe", "DEU", "Europe", "GRC", "Europe",
  "HUN", "Europe", "ISL", "Europe", "IRL", "Europe", "ITA", "Europe",
  "LVA", "Europe", "LIE", "Europe", "LTU", "Europe", "LUX", "Europe",
  "MLT", "Europe", "MDA", "Europe", "MCO", "Europe", "MNE", "Europe",
  "NLD", "Europe", "MKD", "Europe", "NOR", "Europe", "POL", "Europe",
  "PRT", "Europe", "ROU", "Europe", "RUS", "Europe", "SMR", "Europe",
  "SRB", "Europe", "SVK", "Europe", "SVN", "Europe", "ESP", "Europe",
  "SWE", "Europe", "CHE", "Europe", "TUR", "Europe", "UKR", "Europe",
  "GBR", "Europe", "VAT", "Europe",
  # Oceania
  "AUS", "Oceania", "FJI", "Oceania", "KIR", "Oceania", "MHL", "Oceania",
  "FSM", "Oceania", "NRU", "Oceania", "NZL", "Oceania", "PLW", "Oceania",
  "PNG", "Oceania", "WSM", "Oceania", "SLB", "Oceania", "TON", "Oceania",
  "TUV", "Oceania", "VUT", "Oceania"
)

### |- filter to exports + world partner ----
wto_clean <- wto_raw |>
  filter(
    indicator == "Merchandise exports by product group - annual",
    partner == "World"
  ) |>
  rename(
    country     = reporter,
    iso3        = reporter_iso3a,
    value_usd_m = value
  ) |>
  filter(!is.na(value_usd_m), value_usd_m > 0)

### |- keep three top-level product groups only ----
product_groups <- c(
  "Agricultural products",
  "Fuels and mining products",
  "Manufactures"
)

wto_filtered <- wto_clean |>
  filter(product %in% product_groups)

### |- attach OWID regions ----
wto_regions <- wto_filtered |>
  left_join(owid_regions, by = "iso3") |>
  filter(!is.na(region))

### |- 5-year average (2019–2023) ----
wto_avg <- wto_regions |>
  filter(year %in% 2019:2023) |>
  group_by(region, product) |>
  summarise(
    avg_value = mean(value_usd_m, na.rm = TRUE),
    .groups   = "drop"
  )

# housekeeping
rm(wto_raw, wto_clean, wto_filtered, wto_regions)
gc()

### |- recode product labels for display ----
wto_avg <- wto_avg |>
  mutate(
    product_label = case_when(
      product == "Agricultural products" ~ "Agriculture",
      product == "Fuels and mining products" ~ "Fuels & Mining",
      product == "Manufactures" ~ "Manufactures",
      TRUE ~ product
    )
  )

### |- order regions ----
region_order <- c("Asia", "Europe", "Americas", "Africa", "Oceania") 

product_order <- c("Agriculture", "Fuels & Mining", "Manufactures")

wto_avg <- wto_avg |>
  mutate(
    region        = factor(region, levels = region_order),
    product_label = factor(product_label, levels = product_order)
  )

### |- build alluvial-ready data ----
alluvial_data <- wto_avg |>
  filter(!is.na(region), !is.na(product_label)) |>
  rename(
    origin_region = region,
    product_group = product_label,
    flow_value    = avg_value
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Agriculture"    = "#7FB069",   
    "Fuels & Mining" = "#D08C60",   
    "Manufactures"   = "#7A1E2C" 
  )
)

### |- titles and caption ----
title_text <- "Global Trade Connects Regions Through a Few Dominant Product Flows"

subtitle_text <- glue(
  "Average annual exports by **region** and **product group** (2019–2023)"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 14,
  source_text = "WTO Statistics Portal — Merchandise Trade Values by Product Group"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Axes
    axis.text.x   = element_blank(),
    axis.text.y   = element_text(size = 9, family = fonts$text),
    axis.ticks    = element_blank(),
    axis.title    = element_blank(),
    
    # Grid
    panel.grid    = element_blank(),
    
    # Legend
    legend.position = "top",
    legend.title    = element_text(size = 9, family = fonts$text, face = "bold"),
    legend.text     = element_text(size = 9, family = fonts$text),
    
    # Titles
    plot.title = element_text(
      size = 16, face = "bold", family = fonts$title,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 10, family = fonts$text, color = "gray40",
      margin = margin(b = 16)
    ),
    plot.caption = element_markdown(
      size = 7, family = fonts$text, color = "gray55",
      hjust = 0, margin = margin(t = 12)
    ),
    plot.margin = margin(20, 20, 12, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(
  alluvial_data,
  aes(
    axis1 = origin_region,
    axis2 = product_group,
    y  = flow_value,
    fill = product_group
   )
  ) +
  # Geoms
  geom_alluvium(
    aes(fill = product_group),
    alpha = 0.70,
    width = 1 / 4,
    knot.pos = 0.4
  ) +
  geom_stratum(
    width = 1 / 4,
    fill = "gray92",
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    stat = "stratum",
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    size = 3,
    fontface = "bold",
    family = fonts$text,
    color = "gray20"
  ) +
  # Scales
  scale_fill_manual(
    values = colors$palette,
    name = "Product Group",
    breaks = product_order
  ) +
  scale_y_continuous(
    labels = label_comma(scale = 1e-3, suffix = "B"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0.5, 0.8))
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    y = "Average Annual Exports (USD billions)"
  )

### |- right-side text labels + insight annotation ----
p_final <- p +
  geom_text(
    stat = "stratum",
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), "")),
    size = 2.8,
    fontface = "plain",
    family = fonts$text,
    color = "gray40",
    hjust = -0.15
  ) +
  annotate(
    "text",
    x = 2.18, y = max(alluvial_data$flow_value) * 0.08,
    label = "Manufactured goods dominate\nexports across all regions",
    fontface = "italic",
    size = 2.8, family = fonts$text, color = "gray40",
    hjust = 0, lineheight = 1.3
  )

snap(p_final)


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

