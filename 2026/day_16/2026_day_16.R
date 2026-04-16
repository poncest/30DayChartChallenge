
## Challenge: #30DayChartChallenge 2026 · Day 16
## Prompt:    Relationships | Causation
## Topic:     "The Crime Wave May Have Been Written in 1970"
##            Leaded gasoline use (1941–1985) and U.S. violent crime (1960–2010)
##            with a ~23-year lag, indexed to peak = 100

## Author:    Steven Ponce
## Date:      2026-04-16

## NOTE: This script uses custom helper functions for theming and formatting.
##       Sources: R/utils/fonts.R, R/utils/social_icons.R,
##                R/themes/base_theme.R
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data sources:
##   Lead (gasoline): Nevin (2000, 2007); EPA historical lead phasedown data;
##                    Federal Highway Administration Net Motor-Fuel Volume 1950–1995.
##                    Values reflect grams of lead per gallon × consumption volume,
##                    indexed to 1970 peak.
##   Violent crime:   FBI Uniform Crime Reporting (UCR) Program, violent crime rate
##                    per 100,000 inhabitants, 1960–2010. Source: FBI UCR / OWID.
##   Causal framework: Nevin R. (2000) Environ. Res.; Nevin R. (2007) Environ. Res.
##                     104(3):315–336. DOI: 10.1016/j.envres.2007.02.008
##                     Reyes JW (2007): lead phase-out explains ~56% of violent crime
##                     decline 1992–2002.


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
  width  = 12,
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

### |- leaded gasoline use (US, indexed: peak 1970 = 100) ----
### Values derived from Nevin (2000, 2007) Figure 1 data and EPA records.
### Units: thousands of short tons of lead emitted annually, normalized to peak.
### The series rises through the 1940s–60s as car ownership expands,
### peaks ~1970 at ~200,000 tons/year, then falls sharply after EPA phasedown
### begins 1973, accelerating in 1985 (90% reduction rule) and ban in 1996.

lead_raw <- tibble(
  year  = c(1941, 1945, 1950, 1955, 1960, 1963, 1965, 1967, 1969, 1970,
            1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,
            1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
            1991, 1992, 1993, 1994, 1995),
  lead  = c(  9,   11,   32,   52,   70,   80,   88,   93,   97,  100,
              99,   97,   94,   90,   83,   75,   68,   62,   57,   53,
              49,   43,   37,   30,   18,    9,    6,    5,    4,    3,
              2,    2,    1,    1,    1)
)

### |- FBI UCR violent crime rate per 100,000 (1960–2010) ----
### Source: FBI Uniform Crime Reporting Program.
### Violent crime = murder, rape (legacy def.), robbery, aggravated assault.
### Values rounded to nearest whole number from published FBI tables.
### Peak: ~758 per 100,000 in 1991.

crime_raw <- tibble(
  year  = c(1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969,
            1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
            1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989,
            1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
            2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
            2010),
  crime = c(161, 158, 162, 168, 190, 200, 220, 253, 298, 329,
            364, 396, 401, 417, 461, 482, 467, 476, 497, 549,
            597, 594, 571, 538, 539, 557, 617, 610, 640, 664,
            730, 758, 758, 747, 714, 685, 637, 611, 567, 524,
            507, 505, 494, 475, 463, 469, 474, 467, 455, 431,
            405)
)


## 3. EXAMINING THE DATA ----
glimpse(lead_raw)
glimpse(crime_raw)

### Check peak years
lead_raw  |> slice_max(lead,  n = 3)   
crime_raw |> slice_max(crime, n = 3)  


## 4. TIDY DATA ----

### |- Normalize each series: peak = 100 ----
lead_idx <- lead_raw |>
  mutate(
    index = lead / max(lead) * 100,
    series = "lead"
  )

crime_idx <- crime_raw |>
  mutate(
    index = crime / max(crime) * 100,
    series = "crime"
  )

### |- Combine for plotting ----
df_plot <- bind_rows(
  lead_idx |> select(year, index, series),
  crime_idx |> select(year, index, series)
) |>
  mutate(
    series = factor(series, levels = c("lead", "crime")),
    series_label = case_when(
      series == "lead" ~ "Leaded gasoline use",
      series == "crime" ~ "Violent crime rate"
    )
  )

### |- Annotation anchor points ----
lead_peak_year <- lead_raw |>
  slice_max(lead, n = 1) |>
  pull(year) # 1970
crime_peak_year <- crime_raw |>
  slice_max(crime, n = 1, with_ties = FALSE) |>
  pull(year) # 1991

lead_peak_idx <- 100
crime_peak_idx <- 100

### |- Key annotation coordinates ----
lag_xmin <- lead_peak_year # 1970
lag_xmax <- crime_peak_year # 1991


# 5. VISUALIZATION ---- 

### |- Plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "lead"       = "#C0392B",  
    "crime"      = "#1C2833",  
    "lag_band"   = "#F0E6C8",  
    "annotation" = "#555555"   
  )
)

### |- Titles and caption ----
title_text <- "The Crime Wave May Have Been Written in 1970"

subtitle_text <- glue(
  "Indexed U.S. trends in **<span style='color:{colors$palette$lead}'>leaded gasoline use</span>** ",
  "and **<span style='color:{colors$palette$crime}'>violent crime rates</span>** ",
  "reveal a ~22-year lag.<br>",
  "Both series indexed to peak (= 100) to compare shape, not magnitude. ",
  "Children exposed to lead in early life reach peak offending age ~20 years later."
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 16,
  source_text = "FBI Uniform Crime Reporting Program; Nevin (2007) Environ. Res.; EPA lead phasedown records"
)

### |- Fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Titles
    plot.title  = element_text(
      size = 24, face = "bold", family = fonts$title,
      color = colors$palette$crime, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 12, family = fonts$text,
      color = "#444444", lineheight = 1.4,
      margin = margin(b = 18)
    ),
    plot.caption  = element_markdown(
      size = 9, color = "gray50", hjust = 0,
      margin = margin(t = 14)
    ),
    
    # Axes
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 11, color = "gray45", family = fonts$text,
      margin = margin(r = 8), lineheight = 1.3
    ),
    axis.text = element_text(size = 10, color = "gray40", family = fonts$text),
    axis.ticks = element_blank(),
    
    # Grid — horizontal only, very light
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    
    # Panel
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 24, 12, 20)
  )
)

theme_set(weekly_theme)


### |- main plot ----
p <- ggplot() +

  # Lag band
  annotate(
    "rect",
    xmin = lag_xmin, xmax = lag_xmax,
    ymin = -Inf, ymax = Inf,
    fill = colors$palette$lag_band, alpha = 0.35
  ) +

  # Geoms
  geom_line(
    data = df_plot |> filter(series == "lead"),
    aes(x = year, y = index),
    color = colors$palette$lead, linewidth = 1.1, lineend = "round"
  ) +
  geom_line(
    data = df_plot |> filter(series == "crime"),
    aes(x = year, y = index),
    color = colors$palette$crime, linewidth = 1.25, lineend = "round"
  ) +
  geom_point(
    data = tibble(year = lead_peak_year, index = lead_peak_idx, series = "lead"),
    aes(x = year, y = index),
    color = colors$palette$lead, size = 3.5
  ) +
  geom_point(
    data = tibble(year = crime_peak_year, index = crime_peak_idx, series = "crime"),
    aes(x = year, y = index),
    color = colors$palette$crime, size = 3.5
  ) +

  # Annotate
  annotate(
    "text",
    x = lead_peak_year - 1, y = lead_peak_idx + 5,
    label = "Lead peaks\n~1970",
    hjust = 1, vjust = 0, size = 4.1,
    color = colors$palette$lead, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 1992, y = crime_peak_idx + 5,
    label = "Crime peaks\n~1991",
    hjust = 0, vjust = 0, size = 4.1,
    color = colors$palette$crime, fontface = "bold"
  ) +
  annotate(
    "segment",
    x = lag_xmin + 0.5, xend = lag_xmax - 0.5,
    y = 28, yend = 28,
    arrow = arrow(ends = "both", length = unit(0.12, "cm"), type = "closed"),
    color = "#8B6914", linewidth = 0.55
  ) +
  annotate(
    "text",
    x = (lag_xmin + lag_xmax) / 2, y = 36,
    label = "~22-year lag (exposure \u2192 peak offending age)",
    hjust = 0.5, size = 2.9,
    color = "#8B6914", fontface = "bold", family = "sans"
  ) +
  annotate(
    "segment",
    x = 1973, xend = 1973,
    y = 0, yend = 93,
    linetype = "dashed", color = "gray65", linewidth = 0.4
  ) +
  annotate(
    "text",
    x = 1972.2, y = 78,
    label = "EPA lead phaseout\nbegins (1973)",
    hjust = 1, size = 3.2, lineheight = 1.25,
    color = "gray35"
  ) +

  # Scales
  scale_x_continuous(
    breaks = seq(1945, 2010, by = 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 115),
    breaks = seq(0, 100, by = 25),
    labels = function(x) ifelse(x == 100, "100\n(peak)", as.character(x)),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    y        = "Index (peak = 100)"
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


### Data notes
### -----------
### Lead index: Normalized from EPA/Nevin source data. Values reflect total
### tetraethyl lead (TEL) added to US gasoline by year. The raw series peaks
### at ~200,000 short tons/year in 1970 before the Clean Air Act and EPA
### phasedown (1973) begin a sustained decline, accelerating sharply in 1985
### (90% reduction rule) and concluding with a full on-road ban effective 1996.
###
### Crime index: FBI UCR violent crime rate per 100,000 inhabitants.
### Violent crime = murder + rape (legacy def.) + robbery + aggravated assault.
### Peak: ~758/100k in 1991. The post-1991 decline was not predicted by any
### contemporaneous demographic model (Fox 1996 forecast a rise). Reyes (2007)
### estimated lead phase-out explains ~56% of the 1992–2002 decline.
###
### Lag: Nevin (2000, 2007) identified 23-year best-fit lag for violent crime
### and robbery (18 years for burglary). Consistent with neurological literature:
### lead exposure at ages 0–5 damages prefrontal cortex development; criminal
### behavior peaks at ages 18–25 (i.e., birth cohort exposed 1966–1975 →
### peak offending 1984–2000).
###
### Framing note: This chart shows a well-studied causal *hypothesis* with
### strong ecological and individual-level supporting evidence across 9+ countries.
### It does not claim lead is the sole cause of the crime wave/decline.

### Causal evidence notes
### ----------------------
### Consistent across: USA, UK, Canada, Australia, France, Finland, Italy,
### West Germany, New Zealand (Nevin 2007) — each with independent lead and
### crime trajectories, yet same lag structure emerges.
### State-level: Reyes (2007) — states with earlier/sharper lead reductions
### showed earlier/sharper crime drops, controlling for other factors.
### Mechanism: Lead → disrupts neurological development in prefrontal cortex
### → impaired impulse control, executive function, aggression regulation.
### Biological plausibility confirmed in animal studies and MRI imaging.


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-26
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmpk76Pia/filea2f422093087". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ──────────────────────────────────────────────────
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
# ─────────────────────────────────────────────────────────────