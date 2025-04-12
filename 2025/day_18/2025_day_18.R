
## Challenge: #30DayChartChallenge 2025 day 18
## Topic:     Relationships | El Paus (theme day)
## Author:    Steven Ponce
## Date:      2024-04-18

## Data:      California Annual Average Daily Traffic Volumes (AADT)
##            Metadata Updated: November 27, 2024
##            https://catalog.data.gov/dataset/traffic-volumes-aadt-ee8d6


## 1. LOAD PACKAGES & SETUP ----  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  janitor,        # Simple Tools for Examining and Cleaning Dirty Data
  skimr,          # Compact and Flexible Summaries of Data
  scales,         # Scale Functions for Visualization
  lubridate       # Make Dealing with Dates a Little Easier
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions6
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

traffic_volumnes_raw <- read_csv('2025/data/Traffic_Volumes_AADT.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(traffic_volumnes_raw)
skim(traffic_volumnes_raw)


## 4. TIDYDATA ----

# Tidy
simple_traffic <- traffic_volumnes_raw |>                   
  select(county, route, back_peak_hour, back_aadt) |>
  filter(!is.na(back_peak_hour) & !is.na(back_aadt)) |>
  # Major counties with substantial data
  filter(county %in% c("LA", "SD", "ORA", "SF", "SCL", "ALA", "CC")) |>
  # Routes with higher traffic
  filter(back_aadt > 50000) |>
  # Remove extreme outliers (values beyond 3x the standard deviation)
  filter(
    back_peak_hour < mean(back_peak_hour) + 3*sd(back_peak_hour),
    back_aadt < mean(back_aadt) + 3*sd(back_aadt)
  ) |>
  mutate(
    peak_ratio = back_peak_hour / (back_aadt/24),
    county_group = case_when(
      county == "LA" ~ "Los Angeles",
      county == "ORA" ~ "Orange",
      TRUE ~ "Other Bay Area Counties"
    )
  )


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Los Angeles" = "#1D3557",  
    "Orange" = "#457B9D",      
    "Other Bay Area Counties" = "#A8DADC" ,
    NULL = "#A8DADC40"
  )
)

### |-  titles and caption ----
# text
title_text    <- str_glue("Relationship Between Peak Hour and Daily Traffic")

subtitle_text <- str_glue("Major corridors in Los Angeles and Orange counties show higher traffic volumes than Bay Area routes")

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 18,
  source_text =  "California Department of Transportation via data.gov" 
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# El País theme (at least my interpretation)
el_pais_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      # Typography
      text = element_text(family = "Helvetica", color = "#333333"),
      plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(0.79), color = "#666666", hjust = 0, margin = margin(b = 20)),
      
      # Axis styling
      axis.title = element_text(size = rel(0.71), color = "#666666"),
      axis.text = element_text(size = rel(0.64), color = "#333333"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      
      # Grid styling
      panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      
      # Legend styling
      legend.title = element_text(size = rel(0.71)),
      legend.text = element_text(size = rel(0.64)),
      legend.position = c(0.01, 1),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key.size = unit(1.2, "lines"),
      legend.margin = margin(t = 0, r = 10, b = 5, l = 0),
      
      # Margins & Others
      plot.margin = margin(t = 20, r = 20, b = 15, l = 20),
      plot.background = element_rect(fill = 'white', color = 'white'),
      panel.background = element_rect(fill = 'white', color = 'white')
    )
}

# Plot
ggplot(simple_traffic, aes(x = back_peak_hour, y = back_aadt)) +
  # Geoms
  geom_point(
    aes(color = county_group),
    alpha = 0.8,
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = colors$palette[1],  
    fill = colors$palette[5],  
    size = 1,
    fullrange = FALSE  # Only draw the trend line within the range of actual data
  ) +
  # Annotate
  annotate(
    "text",
    x = max(simple_traffic$back_peak_hour) * 0.3,
    y = max(simple_traffic$back_aadt) * 0.9,
    label = "Daily Traffic ≈ 10.5 × Peak Hour Volume",
    color = colors$palette[1], 
    fontface = "italic",
    size = 3.5
  ) +
  # Scales
  scale_color_manual(
    values = colors$palette,
    name = "County"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x/1000, "K"),
    name = "Annual Average Daily Traffic (vehicles)",
    limits = c(0, max(simple_traffic$back_aadt) * 1.05)
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x/1000, "K"),
    name = "Peak Hour Traffic Volume (vehicles)",
    limits = c(0, max(simple_traffic$back_peak_hour) * 1.05)
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  # Theme
  el_pais_theme() +
  theme(
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    )
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-01
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# bit           4.6.0    2025-03-06 [1] CRAN (R 4.4.3)
# bit64         4.6.0-1  2025-01-16 [1] CRAN (R 4.4.3)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli           3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
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
# P labeling      0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.3)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick        2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P Matrix        1.7-3    2025-03-11 [?] CRAN (R 4.4.3)
# P methods     * 4.4.0    2024-04-24 [?] local
# P mgcv          1.9-1    2023-12-21 [?] CRAN (R 4.4.3)
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P nlme          3.1-167  2025-01-27 [?] CRAN (R 4.4.3)
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
# P splines       4.4.0    2024-04-24 [?] local
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
# ────────────────────────────────────────────────────────────────────────────
# > 