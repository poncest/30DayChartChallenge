
## Challenge: #30DayChartChallenge 2025 day 21
## Topic:     Timeseries | Fossils
## Author:    Steven Ponce
## Date:      2024-04-21

## Data:      paleobioDB
##            package for downloading, visualizing and processing data from the Paleobiology Database.
##            https://github.com/ropensci/paleobioDB
##            https://docs.ropensci.org/paleobioDB/


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
  paleobioDB,     # Download and Process Data from the Paleobiology Database # Download and Process Data from the Paleobiology Database
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

# Search for fossil occurrences of trilobites 
trilobites <- pbdb_occurrences(
  base_name = "Trilobita", 
  show = c("time", "coords", "phylo"), 
  limit = "all"
  )
  

## 3. EXAMINING THE DATA ----
glimpse(trilobites)
skim(trilobites)


## 4. TIDYDATA ----

trilo_time_binned <- trilobites |>
  filter(!is.na(eag), !is.na(lag)) |>      
  mutate(
    mid_ma = (eag + lag) / 2,
    bin_5ma = floor(mid_ma / 5) * 5 ,
    bin_10ma = floor(mid_ma / 10) * 10
  ) |>
  count(bin_5ma) |>
  arrange(desc(bin_5ma)) 


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "#D95F02" ,
    "gray20"
  )
)

### |-  titles and caption ----
# text
title_text    <- str_wrap("Rise and Fall of Trilobites: Fossil Records Over Time",
                          width = 55) 

subtitle_text <- str_wrap("Exploring the dramatic rise and fall of one of Earth's most successful early arthropod groups",
                          width = 85)

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 21,
  source_text =  "{ paleobioDB } R package" 
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

    # Axis elements
    axis.title.y = element_text(color = colors$text, size = rel(0.8), margin = margin(r = 10)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),

    # Grid elements
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92"),

    # Legend elements
    legend.position = "plot",
    legend.title = element_blank(),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Facet elements
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = rel(1), margin = margin(10, 0, 10, 0)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

# Plot
ggplot(trilo_time_binned, aes(x = bin_5ma, y = n)) +
  # Geoms
  geom_area(fill = colors$palette[1], alpha = 0.2) +
  geom_line(color = colors$palette[1], linewidth = 1.5) +
  geom_point(color = colors$palette[1], size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 444, linetype = "dashed", color = colors$palette[2], alpha = 0.4, linewidth = 0.7) +
  geom_vline(xintercept = 372, linetype = "dashed", color = colors$palette[2], alpha = 0.4, linewidth = 0.7) +
  # Annotate
  annotate("text",
    x = 444, y = 5000,
    label = "Ordovician Extinction",
    color = colors$palette[2], hjust = 1.1, size = 3.5
  ) +
  annotate("text",
    x = 372, y = 5000,
    label = "Devonian Extinction",
    color = colors$palette[2], hjust = 1.1, size = 3.5
  ) +
  annotate("text",
    x = 475, y = -283.8,
    label = "Ordovician", fontface = "italic", size = 4
  ) +
  annotate("text",
    x = 425, y = -283.8,
    label = "Silurian", fontface = "italic", size = 4
  ) +
  annotate("text",
    x = 390, y = -283.8,
    label = "Devonian", fontface = "italic", size = 4
  ) +
  # Scales
  scale_x_reverse(
    limits = c(520, 250),
    breaks = seq(500, 250, by = -50),
    minor_breaks = seq(500, 250, by = -10)
  ) +
  scale_y_continuous(
    limits = c(-max(trilo_time_binned$n) * 0.1, max(trilo_time_binned$n) * 1.1),
    expand = c(0, 0)
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Millions of Years Ago (Ma)",
    y = "Number of Recorded Fossil Occurrences",
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.8),
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

# ─ Session info ─────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-04
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli           3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P codetools     0.2-20   2024-03-31 [?] CRAN (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark    1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
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
# P gtools        3.9.5    2023-11-20 [?] CRAN (R 4.4.3)
# P here        * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite      1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P knitr         1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P labeling      0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick        2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P paleobioDB  * 1.0.0    2024-02-29 [?] CRAN (R 4.4.3)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6            2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P ragg          1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp          1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv          1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rjson         0.2.23   2024-09-16 [?] RSPM (R 4.4.0)
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
# P terra         1.7-71   2024-01-31 [?] CRAN (R 4.4.0)
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
# P withr         3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] RSPM (R 4.4.0)
# xml2          1.3.8    2025-03-14 [1] CRAN (R 4.4.3)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────
# > 
