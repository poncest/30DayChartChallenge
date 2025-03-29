
## Challenge: #30DayChartChallenge 2025 day 02
## Topic:     Comparison | slope
## Author:    Steven Ponce
## Date:      2024-04-02

## Article:   Internet, Broadband Fact Sheet
##            Fact Sheets: Tech Adoption Trends
##            Pew Research Center, published November 13, 2024

## Data:      Internet use by age 
## Link:      https://www.pewresearch.org/internet/fact-sheet/internet-broadband/ 


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
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
internet_raw <- read_delim(
  here::here('2025/data/internet_use_by_age_data_2024-11-13.csv'),
  skip = 3, ) |> clean_names() |> 
    head(-2) |>   # remove the last two rows
    glimpse()



## 3. EXAMINING THE DATA ----
glimpse(internet_raw)
skim(internet_raw)


## 4. TIDYDATA ----

### |- Tidy ----
internet_clean <- internet_raw |>
  # Remove whitespace from year and convert to numeric
  mutate(
    year = as.numeric(year),
    across(
      c(ages_18_29, x30_49, x50_64, x65),
      ~ as.numeric(str_remove(., "%")) / 100
    )
  ) |>
  rename(
    "18-29" = ages_18_29,
    "30-49" = x30_49,
    "50-64" = x50_64,
    "65+" = x65
  ) |>
  pivot_longer(
    cols = c("18-29", "30-49", "50-64", "65+"),
    names_to = "age_group",
    values_to = "internet_usage"
  ) |>
  mutate(
    age_group = factor(age_group,
                       levels = c("18-29", "30-49", "50-64", "65+")
    )
  ) |>
  filter(year %in% c(2000, 2024))


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors <- get_theme_colors(palette = c(
  "18-29" = "gray", 
  "30-49" = "gray", 
  "50-64" = "gray", 
  "65+" = "#8856a7"      
  ))  

### |-  titles and caption ----
# text
title_text    <- str_glue("Digital Divide Narrows: Internet Adoption Across Generations") 
subtitle_text <- str_glue("The __65+ age group__ showed the steepest adoption curve, dramatically narrowing what was<br> 
                          once a 56-point divide")

# Create caption
caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 02,
  source_text =  "Pew Research Center" 
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
    legend.position = "top",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 20, r = 20, b = 20, l = 60),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot(data = internet_clean) +
  # Geoms
  geom_hline(
    yintercept = seq(0, 1, 0.25), 
    color = "gray90", 
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = c(2000, 2024),
    color = '#333333',
    linewidth = 0.4
  ) +
  geom_line(                            # gray lines
    data = internet_clean %>% filter(age_group != "65+"), 
    aes(x = year, y = internet_usage, group = age_group),
    color = "gray80", 
    linewidth = 0.4
  ) + 
  geom_line(                           # selected line
    data = internet_clean %>% filter(age_group == "65+"), 
    aes(x = year, y = internet_usage, group = age_group, color = age_group),
    linewidth = 1.0
  ) +
  geom_point(
    aes(x = year, y = internet_usage, color = age_group),
    size = 2
  ) +
  geom_text(                           # 2000 labels         
    data = internet_clean %>% filter(year == 2000),
    aes(x = year, y = internet_usage, 
        label = paste0(age_group, ": ", percent(internet_usage, accuracy = 1)),
        color = age_group),
    hjust = 1.2,
    fontface = "bold",
    size = 4
  ) +
  geom_text_repel(                     # 2024 labels 
    data = internet_clean %>% filter(year == 2024),
    aes(x = year, y = internet_usage, 
        label = paste0(age_group, ": ", percent(internet_usage, accuracy = 1)),
        color = age_group),
    hjust = 0,
    direction = "y",
    nudge_x = 1,
    segment.size = 0.2,
    segment.color = "gray70",
    min.segment.length = 0,
    fontface = "bold",
    size = 3.5,
    box.padding = 0.4,
    point.padding = 0.1,
    force = 2
  ) +
  annotate(                           # note
    "text", 
    x = 2012, 
    y = 0.2, 
    label = "65+ group shows\nthe steepest slope",
    color = colors$palette[4], 
    fontface = "bold", 
    size = 4.5,
    alpha = 0.8
  ) +
  # Scales
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = scales::label_percent(
      suffix = '<span style="font-size:6pt;"> %</span>'
    )
  ) +
  scale_x_continuous(
    breaks = c(2000, 2024),
    labels = c("2000", "2024"),
    limits = c(1992, 2032),
    expand = c(0, 0),  
  ) +
  scale_color_manual(
    values = colors$palette, 
    guide = "none"
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Percentage of Adults Using the Internet"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size          = rel(1.75),
      family        = fonts$title,
      face          = "bold",
      color         = colors$title,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(1),
      family        = fonts$subtitle,
      color         = colors$subtitle,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 20)
    ),
    plot.caption    = element_markdown(
      size          = rel(.65),
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

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-19
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# P annotater     0.2.3    2024-01-26 [?] RSPM (R 4.4.0)
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] RSPM (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] RSPM (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli           3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace    2.1-0    2023-01-23 [?] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] RSPM (R 4.4.0)
# curl          5.2.1    2024-03-01 [1] CRAN (R 4.3.3)
# P datasets    * 4.4.0    2024-04-24 [?] local
# P digest        0.6.35   2024-03-11 [?] RSPM (R 4.4.0)
# P dplyr       * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# P farver        2.1.1    2022-07-06 [?] CRAN (R 4.4.0)
# P fastmap       1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats     * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics      0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggplot2     * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggrepel     * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# P glue          1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.3.3)
# P here        * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] RSPM (R 4.4.0)
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
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp          1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv          1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.3    2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot     2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] RSPM (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales      * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# P stringi       1.8.4    2024-05-06 [?] CRAN (R 4.4.0)
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
# P tzdb          0.4.0    2023-05-12 [?] RSPM (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr         3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] RSPM (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# > 