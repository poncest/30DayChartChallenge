
## Challenge: #30DayChartChallenge 2024 day 30
## Topic:     Uncertainties | FiveThirtyEight (theme day)
## Author:    Steven Ponce
## Date:      2024-04-30

## Data:      Solar/Wind utilities
##            TidyTuesday 2022 week 07 (2022-05-03)
## Link:      https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-03/readme.md


## 1. LOAD PACKAGES & SETUP ---- 
pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  janitor,        # Simple Tools for Examining and Cleaning Dirty Data
  skimr,          # Compact and Flexible Summaries of Data
  scales,         # Scale Functions for Visualization
  lubridate,      # Make Dealing with Dates a Little Easier
  MetBrewer,      # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors,     # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  glue,           # Interpreted String Literals
  ggthemes        # Extra Themes, Scales and Geoms for 'ggplot2'
  )

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  =  6,           
  height =  5,           
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----

# Data from tidytuesday 2022 week 18 "Solar/Wind utilities" (2022-05-03)
tt <- tidytuesdayR::tt_load(2022, week = 18)

wind <- tt$wind |> clean_names() |> glimpse()
solar <- tt$solar |> clean_names() |> glimpse()

rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(solar)
skim(solar)
colnames(solar)

glimpse(wind)
skim(wind)
colnames(solar)


## 4. TIDYDATA ----

### |- plot data ---
plot_data <-  wind |> 
  full_join(y = solar)  |> 
  select(date, wind_mwh, solar_mwh) |> 
  rename(
    wind = wind_mwh,
    solar = solar_mwh
    ) |> 
  pivot_longer(
    cols = !date, 
    names_to = 'energy_type', 
    values_to = 'projected_price'
    ) |> 
  drop_na()


# 5. VISUALIZATION ----  
### |- plot aesthetics ---- 
bkg_col      <- "#ebebeb"
title_col    <- "#3B3B3B"        
subtitle_col <- "#3B3B3B"    
caption_col  <- "#656565"    
text_col     <- "#656565"   
col_palette  <- c("#D9961A", "#2F5973")  

### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 30 } &bull; Source: 2022 TidyTuesday wk 18<br>") 
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
solar         <- str_glue("<span style='color:{ col_palette[1] }'>**Solar**</span>")
wind          <- str_glue("<span style='color:{ col_palette[2] }'>**Wind**</span>")

title_text    <- str_glue("Power Purchase Agreement (PPA) Projected Price in<br> 
                          Megawatt hour for { solar } and { wind }")

subtitle_text <- str_glue("A Power Purchase Agreement (PPA) is a contract between buyers and sellers to<br>
                          buy and sell renewable energy for an extended period of 10 to 20 years.")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_fivethirtyeight(base_size = 13))

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
)

### |-  main plot ----
ggplot(plot_data, 
       aes(x = date, y = projected_price, color = energy_type)) +
  
  # Geoms
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'loess', se = TRUE, alpha = 0.3) +

  # Scale
  scale_x_date(
    breaks = '3 year', 
    date_labels = "%Y"
    ) + 
  scale_y_continuous(
    breaks = seq(0, 350, by = 100),
    limits = c(0, 350),
    expand = c(0.05, 0.05)
    ) + 
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = 'off', expand = FALSE) +

  # Labs
  labs(
    x = "",                                     # Year", 
    y = "",                                     # Projected Price (USD / MWh)",
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  # Theme
  theme(
    plot.title      = element_markdown(
      size          = rel(1.2),
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(.85), 
      color         = subtitle_col,
      lineheight    = 1.1, 
      margin        = margin(t = 5, b = 10)
    ),
    plot.caption    = element_markdown(
      size          = rel(.55),
      family        = "caption",
      color         = caption_col,
      lineheight    = 0.65,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 10, b = 5)
    ),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-04-26
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────
# ! package      * version    date (UTC) lib source
# base         * 4.3.3      2024-02-29 [2] local
# P base64enc      0.1-3      2015-07-28 [?] CRAN (R 4.3.0)
# P bit            4.0.5      2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5      2020-08-30 [?] CRAN (R 4.3.0)
# P camcorder      0.1.0      2022-10-03 [?] CRAN (R 4.3.0)
# P cellranger     1.1.0      2016-07-27 [?] CRAN (R 4.3.0)
# cli            3.6.2      2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace     2.1-0      2023-01-23 [?] CRAN (R 4.3.0)
# P commonmark     1.9.0      2023-03-17 [?] CRAN (R 4.3.0)
# P compiler       4.3.3      2024-02-29 [?] local
# cowplot        1.1.3      2024-01-22 [1] CRAN (R 4.3.3)
# P crayon         1.5.2      2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.2.0      2023-12-08 [?] RSPM (R 4.3.0)
# P datasets     * 4.3.3      2024-02-29 [?] local
# P digest         0.6.33     2023-07-07 [?] CRAN (R 4.3.1)
# P dplyr        * 1.1.4      2023-11-17 [?] RSPM (R 4.3.0)
# P fansi          1.0.6      2023-12-08 [?] RSPM (R 4.3.0)
# P farver         2.1.1      2022-07-06 [?] CRAN (R 4.3.0)
# P fastmap        1.1.1      2023-02-24 [?] CRAN (R 4.3.0)
# P forcats      * 1.0.0      2023-01-29 [?] CRAN (R 4.3.0)
# P fs             1.6.3      2023-07-20 [?] CRAN (R 4.3.1)
# P generics       0.1.3      2022-07-05 [?] CRAN (R 4.3.0)
# P ggplot2      * 3.5.0      2024-02-23 [?] CRAN (R 4.3.2)
# ggstream       0.1.0      2021-05-06 [1] CRAN (R 4.3.3)
# P ggtext       * 0.1.2      2022-09-16 [?] CRAN (R 4.3.0)
# P ggthemes     * 5.1.0      2024-02-10 [?] CRAN (R 4.3.3)
# P gifski         1.12.0-2   2023-08-12 [?] CRAN (R 4.3.1)
# P glue         * 1.7.0      2024-01-09 [?] CRAN (R 4.3.2)
# P graphics     * 4.3.3      2024-02-29 [?] local
# P grDevices    * 4.3.3      2024-02-29 [?] local
# P grid           4.3.3      2024-02-29 [?] local
# P gridtext       0.1.5      2022-09-16 [?] CRAN (R 4.3.0)
# P gtable         0.3.4      2023-08-21 [?] CRAN (R 4.3.1)
# P here           1.0.1      2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3      2023-03-21 [?] CRAN (R 4.3.0)
# P htmltools      0.5.7      2023-11-03 [?] RSPM (R 4.3.0)
# P httr           1.4.7      2023-08-15 [?] CRAN (R 4.3.1)
# P janitor      * 2.2.0      2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite       1.8.8      2023-12-04 [?] RSPM (R 4.3.0)
# P knitr          1.45       2023-10-30 [?] RSPM (R 4.3.0)
# P lattice        0.22-5     2023-10-24 [?] CRAN (R 4.3.3)
# P lifecycle      1.0.4      2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate    * 1.9.3      2023-09-27 [?] RSPM (R 4.3.0)
# P magick         2.8.3      2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr       2.0.3      2022-03-30 [?] CRAN (R 4.3.0)
# P markdown       1.12       2023-12-06 [?] CRAN (R 4.3.2)
# P Matrix         1.6-5      2024-01-11 [?] CRAN (R 4.3.3)
# P MetBrewer    * 0.2.0      2022-03-21 [?] CRAN (R 4.3.1)
# P methods      * 4.3.3      2024-02-29 [?] local
# P mgcv           1.9-1      2023-12-21 [?] CRAN (R 4.3.3)
# MoMAColors   * 0.0.0.9000 2024-03-31 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# P munsell        0.5.0      2018-06-12 [?] CRAN (R 4.3.0)
# P nlme           3.1-164    2023-11-27 [?] CRAN (R 4.3.3)
# P pacman         0.5.1      2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.3      2024-02-29 [?] local
# P pillar         1.9.0      2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3      2019-09-22 [?] CRAN (R 4.3.0)
# P purrr        * 1.0.2      2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1      2021-08-19 [?] CRAN (R 4.3.0)
# P ragg           1.2.7      2023-12-11 [?] RSPM (R 4.3.0)
# Rcpp           1.0.12     2024-01-09 [1] CRAN (R 4.3.3)
# P readr        * 2.1.5      2024-01-10 [?] CRAN (R 4.3.2)
# P readxl         1.4.3      2023-07-06 [?] CRAN (R 4.3.1)
# renv           1.0.5      2024-02-29 [1] CRAN (R 4.3.3)
# P repr           1.1.6      2023-01-26 [?] CRAN (R 4.3.0)
# P rlang          1.1.3      2024-01-10 [?] CRAN (R 4.3.2)
# P rprojroot      2.0.4      2023-11-05 [?] RSPM (R 4.3.0)
# P rstudioapi     0.15.0     2023-07-07 [?] CRAN (R 4.3.1)
# P rsvg           2.6.0      2023-10-08 [?] RSPM (R 4.3.0)
# P rvest          1.0.4      2024-02-12 [?] CRAN (R 4.3.2)
# P scales       * 1.3.0      2023-11-28 [?] CRAN (R 4.3.2)
# P selectr        0.4-2      2019-11-20 [?] CRAN (R 4.3.0)
# P sessioninfo    1.2.2      2021-12-06 [?] CRAN (R 4.3.0)
# P showtext     * 0.9-7      2024-03-02 [?] CRAN (R 4.3.3)
# P showtextdb   * 3.0        2020-06-04 [?] CRAN (R 4.3.0)
# P skimr        * 2.1.5      2022-12-23 [?] CRAN (R 4.3.0)
# P snakecase      0.11.1     2023-08-27 [?] CRAN (R 4.3.1)
# P splines        4.3.3      2024-02-29 [?] local
# P stats        * 4.3.3      2024-02-29 [?] local
# P stringi        1.8.3      2023-12-11 [?] RSPM (R 4.3.0)
# P stringr      * 1.5.1      2023-11-14 [?] RSPM (R 4.3.0)
# P svglite        2.1.1      2023-01-10 [?] CRAN (R 4.3.0)
# P sysfonts     * 0.8.9      2024-03-02 [?] CRAN (R 4.3.3)
# P systemfonts    1.0.5      2023-10-09 [?] RSPM (R 4.3.0)
# P textshaping    0.3.6      2021-10-13 [?] CRAN (R 4.3.0)
# P tibble       * 3.2.1      2023-03-20 [?] CRAN (R 4.3.0)
# P tidyr        * 1.3.1      2024-01-24 [?] CRAN (R 4.3.2)
# tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.3.3)
# P tidytuesdayR   1.0.3      2023-12-13 [?] CRAN (R 4.3.2)
# P tidyverse    * 2.0.0      2023-02-22 [?] CRAN (R 4.3.0)
# P timechange     0.2.0      2023-01-11 [?] CRAN (R 4.3.0)
# P tools          4.3.3      2024-02-29 [?] local
# P tzdb           0.4.0      2023-05-12 [?] CRAN (R 4.3.0)
# P usethis        2.2.3      2024-02-19 [?] CRAN (R 4.3.3)
# P utf8           1.2.4      2023-10-22 [?] CRAN (R 4.3.2)
# P utils        * 4.3.3      2024-02-29 [?] local
# P vctrs          0.6.5      2023-12-01 [?] CRAN (R 4.3.2)
# P vroom          1.6.5      2023-12-05 [?] RSPM (R 4.3.0)
# P withr          3.0.0      2024-01-16 [?] CRAN (R 4.3.2)
# P xfun           0.41       2023-11-01 [?] CRAN (R 4.3.2)
# P xml2           1.3.6      2023-12-04 [?] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────
# > 