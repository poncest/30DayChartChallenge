
## Challenge: #30DayChartChallenge 2025 day 29
## Topic:     Uncertainties | extraterrestrial
## Author:    Steven Ponce
## Date:      2024-04-29

## Data:      Meteorite Landings
##            Pandit, Aabha; Romanowski, Alois; Owen, Heather (2024). 
##            Meteorite Landings. University of Rochester. 
##            Dataset. https://doi.org/10.60593/ur.d.26462452.v1

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
  ggdist          # Visualizations of Distributions and Uncertainty 
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

meteorite_landings <- read_csv('2025/data/Meteorite_Landings_20240731.csv') |> 
  clean_names()

  
## 3. EXAMINING THE DATA ----
glimpse(meteorite_landings)
skim(meteorite_landings)


## 4. TIDYDATA ----

# Tidy
meteorites <- meteorite_landings |> 
  mutate(
    fall = if_else(tolower(fall) == "fell", "Observed Fall", "Found Later"),
    mass_g = as.numeric(mass_g),
    year = as.numeric(year),
    class_simplified = case_when(
      str_detect(recclass, "^H") ~ "H Chondrite",
      str_detect(recclass, "^L") ~ "L Chondrite",
      str_detect(recclass, "^LL") ~ "LL Chondrite",
      str_detect(recclass, "Iron") ~ "Iron",
      str_detect(recclass, "Pallasite") ~ "Pallasite",
      TRUE ~ "Other"
    )
  ) |>
  filter(
    !is.na(mass_g) & !is.na(year) & !is.na(recclass),
    class_simplified %in% c("H Chondrite", "L Chondrite", "Iron", "Pallasite"),
    mass_g >= 10 & mass_g <= 100000
  ) 

# Class statistics 
class_stats <- meteorites |>
  group_by(class_simplified) |>
  summarize(
    median_mass = median(mass_g),
    count = n(),
    .groups = "drop"
  ) |>
  arrange(desc(median_mass))

# Reorder class factor levels based on median mass
meteorites <- meteorites |>
  mutate(
    class_simplified = factor(
      class_simplified, 
      levels = class_stats$class_simplified)
    )


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Observed Fall" = "#593C8F", "Found Later" = "#1B9AAA", "Other" = 'gray'
    )
  )          
 
### |-  titles and caption ----
# text
title_text    <- str_glue("Meteorite Mass Distribution and Uncertainty")

subtitle_text <- str_glue("Observed falls show less uncertainty than found meteorites across all classes")

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 29,
  source_text =  "Meteorite Landings. University of Rochester" 
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
    
    # Legend
    legend.position = "bottom",
    
    # Axis elements
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.title.y = element_text(color = colors$text, size = rel(0.8), 
                                hjust = 0.5, margin = margin(r = 10)),
    axis.title.x = element_text(color = colors$text, size = rel(0.8), 
                                hjust = 0.5, margin = margin(t = 10)),
    
    axis.line.x = element_line(color = "gray50", linewidth = .2),
    
    # Grid elements
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray65", linewidth = 0.05),
    panel.grid.major.x = element_line(color = "gray65", linewidth = 0.05),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme) 

# Plot
ggplot(meteorites, aes(x = class_simplified, y = mass_g, fill = fall)) +
  # Geoms
  stat_halfeye(
    aes(fill = fall),
    .width = c(0.50, 0.80, 0.95),
    interval_alpha = 0.8,
    slab_alpha = 0.7,
    point_alpha = 1.0,
    scale = 0.8,
    position = position_dodge(width = 0.6),
    color = "black"
  ) +
  # Scales
  scale_y_log10(
    labels = function(x) paste0(x / 1000, " kg"),
    breaks = c(10, 100, 1000, 10000, 100000)
  ) +
  scale_fill_manual(
    values = colors$palette,
    name = "Discovery Type"
  ) +
  scale_x_discrete(
    labels = function(x) {
      counts <- class_stats$count[match(x, class_stats$class_simplified)]
      paste0(x, "\n(n= ", scales::comma(counts), ")")
    }
  ) +
  coord_flip() +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Mass (kilograms, log scale)"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.9),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.85),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 0.8,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.6),
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

# ─ Session info ────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-13
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────
# ! package        * version  date (UTC) lib source
# V base           * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc        0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# bit              4.6.0    2025-03-06 [1] CRAN (R 4.4.3)
# bit64            4.6.0-1  2025-01-16 [1] CRAN (R 4.4.3)
# P camcorder        0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# P cli              3.6.4    2025-02-13 [?] RSPM (R 4.4.0)
# colorspace       2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark       1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler         4.4.0    2024-04-24 [?] local
# crayon           1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# curl             6.2.2    2025-03-24 [1] CRAN (R 4.4.3)
# P datasets       * 4.4.0    2024-04-24 [?] local
# digest           0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P distributional   0.4.0    2024-02-07 [?] RSPM (R 4.4.0)
# P dplyr          * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# farver           2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap          1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats        * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics         0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggdist         * 3.3.2    2024-03-05 [?] RSPM (R 4.4.0)
# P ggplot2        * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggtext         * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gifski           1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# glue             1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# P graphics       * 4.4.0    2024-04-24 [?] local
# P grDevices      * 4.4.0    2024-04-24 [?] local
# P grid             4.4.0    2024-04-24 [?] local
# P gridtext         0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable           0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# P here           * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms              1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools        0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P janitor        * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite         1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P knitr            1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P labeling         0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P lifecycle        1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate      * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick           2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr         2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown         1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods        * 4.4.0    2024-04-24 [?] local
# munsell          0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman         * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel         4.4.0    2024-04-24 [?] local
# pillar           1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig        2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr          * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6               2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P ragg             1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp             1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr          * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv             1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr             1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang            1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
# P rprojroot        2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# rstudioapi       0.17.1   2024-10-22 [1] CRAN (R 4.4.3)
# P rsvg             2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales         * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo      1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext       * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb     * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr          * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase        0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats          * 4.4.0    2024-04-24 [?] local
# stringi          1.8.7    2025-03-27 [1] CRAN (R 4.4.3)
# P stringr        * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite          2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts       * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts      1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping      0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P tibble         * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidyr          * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect       1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse      * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange       0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools            4.4.0    2024-04-24 [?] local
# tzdb             0.5.0    2025-03-15 [1] CRAN (R 4.4.3)
# P utf8             1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils          * 4.4.0    2024-04-24 [?] local
# P vctrs            0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom            1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr            3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun             0.43     2024-03-25 [?] RSPM (R 4.4.0)
# xml2             1.3.8    2025-03-14 [1] CRAN (R 4.4.3)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────
# > 