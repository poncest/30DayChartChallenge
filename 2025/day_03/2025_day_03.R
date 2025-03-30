
## Challenge: #30DayChartChallenge 2025 day 03
## Topic:     Comparison | circular
## Author:    Steven Ponce
## Date:      2024-04-03

## Data:      NOAA - National Centers for Environmental Information
##            Global Surface Summary of the Day - GSOD
## Link:      https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-day
## Link:      https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-day?dataTypes=TEMP&pageNum=1&bbox=18.516,-67.951,17.883,-65.221&place=Country:241&startDate=2014-01-01T00:00:00&endDate=2024-12-03T23:59:59&stations=78526011641

## Citation:
#' NOAA National Centers of Environmental Information. 1999. Global Surface Summary of the Day - GSOD. 1.0.
#' [SAN JUAN L M MARIN INTERNATIONAL AIRPORT, PR US (78526011641.csv), 2014 - 2024]. NOAA National Centers for Environmental Information. 
#' Accessed [2025-03-21].


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
  viridis,        # Color palettes
  fs              # For file handling
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 7,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
# Directory where all CSV files are stored
data_dir <- here::here('2025/data')

# Get a list of all CSV files for San Juan from 2014-2024
sj_files <- fs::dir_ls(data_dir, regexp = "78526011641_20[0-9]{2}\\.csv$")

# Function to read and process each file
process_file <- function(file_path) {
  # Extract year from filename
  year <- str_extract(file_path, "20[0-9]{2}")
  
  # Read file and add year column
  read_csv(file_path) |>
    janitor::clean_names() |>
    mutate(file_year = year)
}

# Read and combine all files
pri_temperature_all <- map_df(sj_files, process_file)


## 3. EXAMINING THE DATA ----
glimpse(pri_temperature_all)
skim(pri_temperature_all)


## 4. TIDYDATA ----

### |- Tidy ----
# Process temperature data
san_juan_temp <- pri_temperature_all |>
  # Extract date information
  mutate(
    year = year(date),
    month = month(date),
    month_name = month(date, label = TRUE, abbr = TRUE)
  ) |>
  # Select relevant columns
  select(date, year, month, month_name, temp)

# Calculate monthly averages
monthly_temp <- san_juan_temp |>
  group_by(year, month, month_name) |>
  summarize(
    avg_temp = mean(temp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Round temperature
  mutate(avg_temp = round(avg_temp, 1))

# Filter for complete years
years_to_include <- 2014:2024
monthly_temp <- monthly_temp |>
  filter(year %in% years_to_include)


# Prepare data for visualization
monthly_temp <- monthly_temp |>
  mutate(
    # Convert to factors to ensure proper ordering
    month_name = factor(month_name, levels = month.abb),
    year = as.factor(year)
  )

# Set temperature scale
temp_min <- floor(min(monthly_temp$avg_temp, na.rm = TRUE))
temp_max <- ceiling(max(monthly_temp$avg_temp, na.rm = TRUE))


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors <- get_theme_colors(palette = NULL)

### |-  titles and caption ----
# text
title_text    <- str_glue("Radial Warmth: San Juan's Temperature Profile") 
subtitle_text <- str_glue("Monthly Temperature Patterns in San Juan, Puerto Rico (2014-2024)")

# Create caption
caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 03,
  source_text =  "NOAA" 
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
    axis.title.y = element_text(color = colors$text, size = rel(0.8),
                              hjust = 1, vjust = 0.5, angle = 90),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = colors$text, size = rel(0.7)),
    axis.text.y = ggtext::element_markdown(),
  
    # Grid elements
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
 
    # Legend elements
    legend.position = "bottom",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot(monthly_temp, aes(x = month_name, y = year, fill = avg_temp)) +
  # Geoms
  geom_tile(color = "white", size = 0.1) +
  # Scales
  coord_polar() +
  scale_fill_viridis(
    option = "plasma",
    name = "Temperature (°F)",
    limits = c(temp_min, temp_max),
    breaks = seq(temp_min, temp_max, by = 2)
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size          = rel(1.6),
      family        = fonts$title,
      face          = "bold",
      color         = colors$title,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(0.9),
      family        = fonts$subtitle,
      color         = colors$subtitle,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 20)
    ),
    plot.caption    = element_markdown(
      size          = rel(.6),
      family        = fonts$caption,
      color         = colors$caption,
      lineheight    = 0.65,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 10, b = 5)
    ),
  )
  

# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-21
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P annotater      0.2.3    2024-01-26 [?] RSPM (R 4.4.0)
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] RSPM (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] RSPM (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# colorspace     2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark     1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# P curl           6.2.1    2025-02-19 [?] RSPM (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P dplyr        * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap        1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats      * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# fs           * 1.6.5    2024-10-30 [1] CRAN (R 4.4.3)
# P generics       0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggplot2      * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# glue           1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridExtra      2.3      2017-09-09 [?] CRAN (R 4.4.0)
# P gridtext       0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# P here         * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite       1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P knitr          1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P labeling       0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick         2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman       * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# pillar         1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P ragg           1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# P RColorBrewer   1.1-3    2022-04-03 [?] CRAN (R 4.4.0)
# Rcpp           1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr        * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv           1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
# P rprojroot      2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] RSPM (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales       * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# P stringi        1.8.4    2024-05-06 [?] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts    1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping    0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidyr        * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse    * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] RSPM (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P viridis      * 0.6.5    2024-01-29 [?] CRAN (R 4.4.0)
# P viridisLite  * 0.4.2    2023-05-02 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun           0.43     2024-03-25 [?] RSPM (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] RSPM (R 4.4.0)
# 
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────
# > 