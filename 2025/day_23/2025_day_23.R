
## Challenge: #30DayChartChallenge 2025 day 23
## Topic:     Timeseries | log scale
## Author:    Steven Ponce+
## Date:      2024-04-23

## Data:      tidyquant
##            Yahoo Finance via { tidyquant }
##            https://business-science.github.io/tidyquant/

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
  tidyquant,      # Tidy Quantitative Financial Analysis 
  ggrepel         # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

# Define timeframe
end_date <- Sys.Date()
start_date <- end_date - years(7)

# Pharmaceutical companies
pharma_symbols <- c("PFE", "MRK", "ABBV", "LLY", "BMY")

# Get the stock data
pharma_data <- tq_get(
  pharma_symbols, 
  from = start_date,
  to = end_date,
  get = "stock.prices"
  )

## 3. EXAMINING THE DATA ----
glimpse(pharma_data)
skim(pharma_data)


## 4. TIDYDATA ----

# Get the last data point for each company for labeling
label_data <- pharma_data |>  
  group_by(symbol) |>
  filter(date == max(date)) |>
  ungroup()


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "PFE" = "#0000c6",           
    "MRK" = "#00857c",            
    "ABBV" = "#061d49",          
    "LLY" = "#d52b1e",        
    "BMY" = "#be2bba"
  )
)

### |-  titles and caption ----
# text
title_text    <- str_wrap("Pharmaceutical Giants Stock Performance (2018-2025)",
                          width = 55) 

subtitle_text <- str_wrap("Log scale reveals growth patterns during pre-pandemic, pandemic, and recovery periods",
                          width = 85)

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 23,
  source_text =  "Yahoo Finance via { tidyquant }" 
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
    axis.title = element_text(color = colors$text, size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    
    axis.line.x = element_line(color = "gray50", linewidth = .2),
    
    # Grid elements
    panel.grid.minor = element_line(color = "gray65", linewidth = 0.05),
    panel.grid.major = element_line(color = "gray65", linewidth = 0.05),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

# Plot
ggplot() +
  # Geoms
  geom_line(
    data = pharma_data, 
    aes(x = date, y = adjusted, color = symbol),
    linewidth = 0.6
  ) +
  geom_text_repel(
    data = label_data,
    aes(x = date, y = adjusted, label = symbol, color = symbol),
    nudge_x = 70,  
    hjust = 0,
    segment.size = 0.5,
    direction = "y",
    box.padding = 0.5,
    segment.alpha = 0.6,
    fontface = "bold",
    size = 3.5
  ) +
  # Annotate
  annotate(
    "rect", 
    xmin = as.Date("2020-03-01"), 
    xmax = as.Date("2020-12-01"),
    ymin = 10, 
    ymax = 1000,
    alpha = 0.1, 
    fill = "red"
  ) +
  annotate(
    "text", 
    x = as.Date("2020-07-01"), 
    y = 15, 
    label = "COVID-19\nPandemic", 
    color = "darkred",
    size = 3
  ) +
  annotate(
    "label", 
    x = as.Date("2018-04-01"), 
    y = 900, 
    label = "LLY = Eli Lilly\nABBV = AbbVie\nMRK = Merck\nBMY = Bristol Myers Squibb\nPFE = Pfizer",
    hjust = 0, 
    vjust = 1,
    size = 3,
    color = "gray30",
    fill = "white",
    alpha = 0.8,
    label.size = 0.5
  ) +
  # Scale
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  scale_y_log10(
    labels = scales::dollar_format(accuracy = 1)
  ) +
  scale_color_manual(values = colors$palette) +
  coord_cartesian(
    xlim = c(start_date, end_date + days(10))
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Price (log scale)",
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.6),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.85),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.2,
      margin = margin(t = 5, b = 15)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
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

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-06
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package              * version    date (UTC) lib source
# V base                 * 4.4.1      2024-04-24 [2] local (on disk 4.4.0)
# P base64enc              0.1-3      2015-07-28 [?] RSPM (R 4.4.0)
# P camcorder              0.1.0      2022-10-03 [?] RSPM (R 4.4.0)
# P class                  7.3-22     2023-05-03 [?] CRAN (R 4.4.0)
# P cli                    3.6.4      2025-02-13 [?] RSPM (R 4.4.0)
# P codetools              0.2-20     2024-03-31 [?] CRAN (R 4.4.0)
# colorspace             2.1-1      2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark             1.9.1      2024-01-30 [?] RSPM (R 4.4.0)
# P compiler               4.4.0      2024-04-24 [?] local
# curl                   6.2.2      2025-03-24 [1] CRAN (R 4.4.3)
# P data.table             1.15.4     2024-03-30 [?] RSPM (R 4.4.0)
# P datasets             * 4.4.0      2024-04-24 [?] local
# digest                 0.6.37     2024-08-19 [1] CRAN (R 4.4.3)
# P dplyr                * 1.1.4      2023-11-17 [?] RSPM (R 4.4.0)
# farver                 2.1.2      2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap                1.2.0      2024-05-15 [?] RSPM (R 4.4.0)
# P forcats              * 1.0.0      2023-01-29 [?] RSPM (R 4.4.0)
# P furrr                  0.3.1      2022-08-15 [?] CRAN (R 4.4.3)
# P future                 1.34.0     2024-07-29 [?] CRAN (R 4.4.3)
# P future.apply           1.11.3     2024-10-27 [?] CRAN (R 4.4.3)
# P generics               0.1.3      2022-07-05 [?] RSPM (R 4.4.0)
# P ggplot2              * 3.5.1      2024-04-23 [?] CRAN (R 4.4.0)
# P ggrepel              * 0.9.5      2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext               * 0.1.2      2022-09-16 [?] RSPM (R 4.4.0)
# P gifski                 1.12.0-2   2023-08-12 [?] RSPM (R 4.4.0)
# P globals                0.16.3     2024-03-08 [?] CRAN (R 4.4.0)
# glue                   1.8.0      2024-09-30 [1] CRAN (R 4.4.3)
# P gower                  1.0.2      2024-12-17 [?] CRAN (R 4.4.2)
# P graphics             * 4.4.0      2024-04-24 [?] local
# P grDevices            * 4.4.0      2024-04-24 [?] local
# P grid                   4.4.0      2024-04-24 [?] local
# P gridtext               0.1.5      2022-09-16 [?] RSPM (R 4.4.0)
# gtable                 0.3.6      2024-10-25 [1] CRAN (R 4.4.3)
# P hardhat                1.4.1      2025-01-31 [?] CRAN (R 4.4.3)
# P here                 * 1.0.1      2020-12-13 [?] RSPM (R 4.4.0)
# P hms                    1.1.3      2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools              0.5.8.1    2024-04-04 [?] RSPM (R 4.4.0)
# P ipred                  0.9-15     2024-07-18 [?] CRAN (R 4.4.3)
# P janitor              * 2.2.0      2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite               1.9.1      2025-03-03 [1] CRAN (R 4.4.3)
# P knitr                  1.46       2024-04-06 [?] RSPM (R 4.4.0)
# P lattice                0.22-6     2024-03-20 [?] CRAN (R 4.4.3)
# P lava                   1.8.1      2025-01-12 [?] CRAN (R 4.4.3)
# P lifecycle              1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P listenv                0.9.1      2024-01-29 [?] CRAN (R 4.4.3)
# P lubridate            * 1.9.3      2023-09-27 [?] RSPM (R 4.4.0)
# P magick                 2.8.3      2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr               2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown               1.12       2023-12-06 [?] RSPM (R 4.4.0)
# P MASS                   7.3-65     2025-02-28 [?] CRAN (R 4.4.3)
# P Matrix                 1.7-3      2025-03-11 [?] CRAN (R 4.4.3)
# P methods              * 4.4.0      2024-04-24 [?] local
# munsell                0.5.1      2024-04-01 [1] CRAN (R 4.3.3)
# P nnet                   7.3-19     2023-05-03 [?] CRAN (R 4.4.0)
# P pacman               * 0.5.1      2019-03-11 [?] RSPM (R 4.4.0)
# P parallel               4.4.0      2024-04-24 [?] local
# P parallelly             1.42.0     2025-01-30 [?] CRAN (R 4.4.3)
# P PerformanceAnalytics * 2.0.8      2024-12-09 [?] CRAN (R 4.4.3)
# pillar                 1.10.1     2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig              2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P prodlim                2024.06.25 2024-06-24 [?] CRAN (R 4.4.3)
# P purrr                * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P quadprog               1.5-8      2019-11-20 [?] RSPM (R 4.4.0)
# P quantmod             * 0.4.26     2024-02-14 [?] CRAN (R 4.4.0)
# R6                     2.6.1      2025-02-15 [1] CRAN (R 4.4.3)
# P ragg                   1.3.1      2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp                   1.0.12     2024-01-09 [1] CRAN (R 4.3.3)
# P readr                * 2.1.5      2024-01-10 [?] RSPM (R 4.4.0)
# P recipes                1.2.1      2025-03-25 [?] CRAN (R 4.4.3)
# renv                   1.0.5      2024-02-29 [1] CRAN (R 4.3.3)
# P repr                   1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# rlang                  1.1.5      2025-01-17 [1] CRAN (R 4.4.3)
# P RobStatTM              1.0.11     2024-10-17 [?] CRAN (R 4.4.3)
# P rpart                  4.1.23     2023-12-05 [?] CRAN (R 4.4.0)
# P rprojroot              2.0.4      2023-11-05 [?] RSPM (R 4.4.0)
# P rsample                1.3.0      2025-04-02 [?] CRAN (R 4.4.3)
# rstudioapi             0.17.1     2024-10-22 [1] CRAN (R 4.4.3)
# P rsvg                   2.6.0      2023-10-08 [?] RSPM (R 4.4.0)
# P scales               * 1.3.0      2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo            1.2.2      2021-12-06 [?] RSPM (R 4.4.0)
# P showtext             * 0.9-7      2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb           * 3.0        2020-06-04 [?] RSPM (R 4.4.0)
# P skimr                * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase              0.11.1     2023-08-27 [?] RSPM (R 4.4.0)
# P splines                4.4.0      2024-04-24 [?] local
# P stats                * 4.4.0      2024-04-24 [?] local
# stringi                1.8.7      2025-03-27 [1] CRAN (R 4.4.3)
# P stringr              * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P survival               3.5-8      2024-02-14 [?] CRAN (R 4.4.0)
# P svglite                2.1.3      2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts             * 0.8.9      2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts            1.0.6      2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping            0.3.7      2023-10-09 [?] RSPM (R 4.4.0)
# P tibble               * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# P tidyquant            * 1.0.11     2025-02-13 [?] CRAN (R 4.4.3)
# P tidyr                * 1.3.1      2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect             1.2.1      2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse            * 2.0.0      2023-02-22 [?] RSPM (R 4.4.0)
# P timechange             0.3.0      2024-01-18 [?] RSPM (R 4.4.0)
# P timeDate               4032.109   2023-12-14 [?] CRAN (R 4.4.0)
# P timetk                 2.9.0      2023-10-31 [?] CRAN (R 4.4.3)
# P tools                  4.4.0      2024-04-24 [?] local
# P TTR                  * 0.24.4     2023-11-28 [?] CRAN (R 4.4.0)
# tzdb                   0.5.0      2025-03-15 [1] CRAN (R 4.4.3)
# P utf8                   1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils                * 4.4.0      2024-04-24 [?] local
# P vctrs                  0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P withr                  3.0.2      2024-10-28 [?] RSPM (R 4.4.0)
# P xfun                   0.43       2024-03-25 [?] RSPM (R 4.4.0)
# xml2                   1.3.8      2025-03-14 [1] CRAN (R 4.4.3)
# P xts                  * 0.13.2     2024-01-21 [?] CRAN (R 4.4.0)
# P zoo                  * 1.8-12     2023-04-13 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────
# > 
