
## Challenge: #30DayChartChallenge 2025 day 26
## Topic:     Uncertainties | monochrome
## Author:    Steven Ponce
## Date:      2024-04-26

## Data:      CDC’s Behavioral Risk Factor Surveillance System (BRFSS)
##            BRFSS: Prevalence of Cardiovascular Disease
##            https://data.cdc.gov/Behavioral-Risk-Factors/BRFSS-Graph-of-Current-Prevalence-of-Cardiovascula/gfhd-2f5y
##            https://www.cdc.gov/brfss/data_tools.htm

## 1. LOAD PACKAGES & SETUP ----  
if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  janitor,        # Simple Tools for Examining and Cleaning Dirty Data
  skimr,          # Compact and Flexible Summaries of Data
  scales,         # Scale Functions for Visualization
  lubridate,      # Make Dealing with Dates a Little Easier
  ggrepel         # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
)
  
### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
  
cdc_prevalence_cardiovascular_disease <- read_csv("2025/data/Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present__20250409.csv") |> 
  clean_names()

## 3. EXAMINING THE DATA ----
glimpse(cdc_prevalence_cardiovascular_disease)
skim(cdc_prevalence_cardiovascular_disease)


## 4. TIDYDATA ----

# Prepare state-level data
state_cvd <- cdc_prevalence_cardiovascular_disease |>
  filter(year == 2023) |>
  filter(break_out == "Overall" | is.na(break_out)) |>
  filter(
    str_detect(locationdesc, "^[A-Z]") & 
      !str_detect(locationdesc, "median|Median|average|Average")
  ) |>
  select(
    location = locationdesc,
    prevalence = data_value,
    lower_bound = confidence_limit_low,
    upper_bound = confidence_limit_high,
    sample_size,
    year
  ) |>
  mutate(
    ci_width = upper_bound - lower_bound,
    location = str_replace(location, " State$", ""),
    location = fct_reorder(location, prevalence)
  ) |>
  filter(!is.na(prevalence))

# Get national median estimate (US-level)
us_median <- cdc_prevalence_cardiovascular_disease |>
  filter(year == 2023, break_out == "Overall", locationabbr == "US") |> 
  pull(data_value)


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "black", "gray40", "gray50", "gray70", "gray95", "white", "gray40"
    )
  )

### |-  titles and caption ----
# text
title_text    <- str_wrap("Uncertainty in Cardiovascular Disease Prevalence Across the U.S.",
                          width = 70) 

subtitle_text <- str_wrap("95% confidence intervals for states and territories reveal varying levels of statistical precision (CDC, 2023)",
                          width = 80)

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 26,
  source_text =  "CDC’s Behavioral Risk Factor Surveillance System (BRFSS)" 
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    
    # Text styling 
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
    
    # Axis elements
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = colors$text, size = rel(0.8), 
                                hjust = 0.5, margin = margin(t = 10)),
    
    axis.line.x = element_line(color = "gray50", linewidth = .2),
    
    # Grid elements
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray65", linewidth = 0.05),
    panel.grid.major.x = element_line(color = "gray65", linewidth = 0.05),
    
    # Plot margins 
    plot.background = element_rect(fill = colors$palette[6], color = colors$palette[6]),
    panel.background = element_rect(fill = colors$palette[6], color = colors$palette[6]),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

# Plot
ggplot(state_cvd, aes(x = prevalence, y = location)) +
  # Geoms
  geom_segment(
    aes(x = lower_bound, xend = upper_bound, yend = location),
    color = colors$palette[4], linewidth = 0.9, alpha = 0.9
  ) +
  geom_point(
    size = 3, color = colors$palette[1], 
    fill = colors$palette[1],
  ) +
  geom_text(
    aes(x = upper_bound, label = sprintf("%.1f%% [%.1f–%.1f]", prevalence, lower_bound, upper_bound)),
    nudge_x = 0.25, hjust = 0, vjust = 0.3, size = 2.7, color = colors$palette[2]
  ) +
  geom_vline(
    xintercept = us_median, color = colors$palette[2], 
    linetype = "dashed", linewidth = 0.5
  ) + 
  # Annotate
  annotate(
    "text", x = us_median, y = 44, 
    label = sprintf("National median: %.1f%%", us_median),
    hjust = 0, vjust = -1, size = 3, color = colors$palette[1], 
    fontface = "italic", angle = 90
  ) +
  # Scales
  scale_x_continuous(
    limits = c(0, max(state_cvd$upper_bound) + 0.7),
    labels = label_percent(scale = 1, suffix = "%"),
    breaks = seq(0, 8, by = 2),
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  scale_y_discrete(
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Cardiovascular Disease Prevalence (%)",
    y = NULL
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.45),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 14)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color = colors$caption,
      lineheight = 0.65,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 10, b = 5)
    ),
  ) 
  
 
# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-10
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# bit           4.6.0    2025-03-06 [1] CRAN (R 4.4.3)
# bit64         4.6.0-1  2025-01-16 [1] CRAN (R 4.4.3)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# P cli           3.6.4    2025-02-13 [?] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark    1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# curl          6.2.2    2025-03-24 [1] CRAN (R 4.4.3)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P dplyr       * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap       1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats     * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics      0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggplot2     * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggrepel     * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# glue          1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable        0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# P here        * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite      1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P knitr         1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick        2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6            2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P ragg          1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp          1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv          1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
# P rprojroot     2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# rstudioapi    0.17.1   2024-10-22 [1] CRAN (R 4.4.3)
# P rsvg          2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales      * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.7    2025-03-27 [1] CRAN (R 4.4.3)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts   1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping   0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidyr       * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse   * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# tzdb          0.5.0    2025-03-15 [1] CRAN (R 4.4.3)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr         3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] RSPM (R 4.4.0)
# xml2          1.3.8    2025-03-14 [1] CRAN (R 4.4.3)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────
# > 