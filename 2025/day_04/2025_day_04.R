
## Challenge: #30DayChartChallenge 2025 day 04
## Topic:     Comparison | big or small
## Author:    Steven Ponce
## Date:      2024-04-04

## Data:      TidyTuesday 2025 week 06
##            CDC Datasets


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
  treemapify      # Draw Treemaps in 'ggplot2'
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
tt <- tidytuesdayR::tt_load(2025, week = 6) 

cdc_datasets <- tt$cdc_datasets |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(cdc_datasets)
skim(cdc_datasets)


## 4. TIDYDATA ----

### |- Tidy ----
# Count datasets by category
category_counts <- cdc_datasets |>
  count(category) |>    
  arrange(desc(n)) |>
  filter(!is.na(category)) |>
  mutate(
    category = if_else(is.na(category), "Uncategorized", category),
    percent = n / sum(n) * 100,
    label = paste0(category, "\n", n, " (", round(percent, 1), "%)")
    )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors <- get_theme_colors(palette =  colorRampPalette(c("#FFAF7B", "#D76D77", "#3A1C71", "black"))(30)
  )

### |-  titles and caption ----
# text
title_text    <- str_glue("Preserving Public Health Knowledge: CDC Dataset Categories at Risk") 
subtitle_text <- str_glue("Large repositories like NNDSS and NCHS contain essential health surveillance data being backed up")

# Create caption
caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 04,
  source_text =  "archive.org" 
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
    legend.position = "right",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot(category_counts, aes(area = n, fill = n, label = label)) +
  # Geoms
  geom_treemap() +
  geom_treemap_text(
    colour = "white", 
    place = "centre", 
    grow = TRUE,
    min.size = 4,         
    fontface = "bold",    
    family = "sans"       
  ) +
  # Scales
  scale_fill_gradientn(
    colors = colors$palette, 
    trans = "log10",
    labels = comma_format(),  
    breaks = c(1, 10, 100, 300),  
    guide = guide_colorbar(
      direction = "vertical",
      barwidth = 1,
      barheight = 10,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    fill = "Number of datasets",
    caption = caption_text
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.7),
      family = fonts$title,
      face  = "bold",
      color = colors$title,
      margin  = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size  = rel(1),
      family  = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin  = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size  = rel(.65),
      family  = fonts$caption,
      color = colors$caption,
      lineheight  = 0.65,
      hjust = 0.5,
      halign  = 0.5,
      margin  = margin(t = 10, b = 5)
    ),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────────────────────────────────
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
# ─ Packages ─────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
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
# P generics       0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggfittext      0.10.2   2024-02-01 [?] CRAN (R 4.4.0)
# P ggplot2      * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# glue           1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# P here         * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P httr2          1.1.1    2025-03-08 [?] CRAN (R 4.4.3)
# P janitor      * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite       1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P knitr          1.46     2024-04-06 [?] RSPM (R 4.4.0)
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
# P rappdirs       0.3.3    2021-01-31 [?] RSPM (R 4.4.0)
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
# P tidytuesdayR   1.1.2    2024-09-09 [?] RSPM (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P treemapify   * 2.5.6    2023-09-30 [?] CRAN (R 4.4.0)
# P tzdb           0.4.0    2023-05-12 [?] RSPM (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr          3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun           0.43     2024-03-25 [?] RSPM (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────
# > 