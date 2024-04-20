
## Challenge: #30DayChartChallenge 2024 day 20
## Topic:     Timeseries | correlations
## Author:    Steven Ponce
## Date:      2024-04-20

## Data:      NOAA - National Centers for Environmental Information
##            Global Surface Summary of the Day - GSOD
## Link:      https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-day 

## Citation:
#' NOAA National Centers of Environmental Information. 1999. Global Surface Summary of the Day - GSOD. 1.0.
#' [SAN JUAN L M MARIN INTERNATIONAL AIRPORT, PR US (78526011641.csv)]. NOAA National Centers for Environmental Information. 
#' Date Range [2017 - 2019], Accessed [2024-04-20].


## 1. LOAD PACKAGES & SETUP ---- 
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  skimr,       # Compact and Flexible Summaries of Data
  scales,      # Scale Functions for Visualization
  lubridate,   # Make Dealing with Dates a Little Easier
  MetBrewer,   # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors,  # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  glue,        # Interpreted String Literals
  forecast     # Forecasting Functions for Time Series and Linear Models
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----

# List files of interest
my_file_path <- list.files(
  path = "2024/data/",
  pattern = "78526011641_",
  full.names = "TRUE"
)

# read the data and add data source column
weather_data_2017_2019 <- map(
  .x = my_file_path,
  \(my_file_path) read_csv(my_file_path) |>                                     # lambda function
    mutate(file = my_file_path)                                                 # add data source
  ) |> 
  list_rbind() |>                                                               # convert the list to a dataframe
  clean_names() |> 
  glimpse()


## 3. EXAMINING THE DATA ----
glimpse(weather_data_2017_2019)
skim(weather_data_2017_2019)
colnames(weather_data_2017_2019)


## 4. TIDYDATA ----

### |- tidy data ---

# Calculate Cross-Correlation
ccf_result <- stats::ccf(
  weather_data_2017_2019$temp,
  weather_data_2017_2019$wdsp,
  lag.max = 24,
  plot = FALSE,
  type = "correlation"
)

# Extract Cross-Correlation Data
ccf_data <- broom::tidy(ccf_result)


### |- plot data ---

# Add highlight column based on CCF value
ccf_data <- ccf_data %>%
  mutate(highlight = ifelse(acf > 0, "up", "down"))


# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('brown', 0.75) 
title_col    <- "gray10"             
subtitle_col <- "gray10"     
caption_col  <- "gray20"   
text_col     <- "gray20"     
col_palette  <- MoMAColors::moma.colors("Abbott", n = 5, type = "discrete") 

### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 20 } &bull; Source: NOAA Global Surface Summary of the Day - GSOD<br>") 
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Cross-Correlation Function (CCF) of Temperature and Wind Speed") 

subtitle_text <- str_glue("San Juan, PR, 2017 - 2019")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("PT Sans", regular.wt = 400, family = "title")                 
font_add_google("Noto Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Noto Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text")) 

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "plot",
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),               
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
  panel.grid.minor      = element_line(linetype = "dashed", linewidth = 0.1, color = "gray"),
  panel.grid.major      = element_line(linetype = "dashed", linewidth = 0.2, color = "gray")
) 

### |-  final plot ----  
ccf_data  |> 
  ggplot(aes(x = lag, y = acf, fill = highlight)) +
  
  # Geom
  geom_col(show.legend = FALSE) +  
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = 'solid', color = 'black') +
  geom_hline(yintercept = 0.06, linewidth = 0.2, linetype = 'dashed', color = 'blue') +
  geom_hline(yintercept = -0.06, linewidth = 0.2, linetype = 'dashed', color = 'blue') +
  
  # Scale
  scale_x_continuous() + 
  scale_y_continuous() + 
  scale_fill_manual(values = col_palette) +
  coord_cartesian(clip = 'off') +
  
  # Labs
  labs(
    x = "Lag", 
    y = "Cross-Correlation", 
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  # Theme
  theme(
    plot.title      = element_text(
      size          = rel(1.1),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(.9), 
      family        = 'subtitle',
      color         = subtitle_col,
      lineheight    = 1.1, 
      margin        = margin(t = 0, b = 1)
    ),
    plot.caption    = element_markdown(
      size          = rel(.5),
      family        = "caption",
      color         = caption_col,
      lineheight    = 1.1,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 5, b = 5)
    )
  )
  

# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ───────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-04-18
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# P annotater     0.2.3      2024-01-26 [?] CRAN (R 4.3.3)
# P backports     1.4.1      2021-12-13 [?] CRAN (R 4.3.0)
# base        * 4.3.3      2024-02-29 [2] local
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.3.0)
# P bit           4.0.5      2022-11-15 [?] CRAN (R 4.3.0)
# P bit64         4.0.5      2020-08-30 [?] CRAN (R 4.3.0)
# P broom         1.0.5      2023-06-09 [?] CRAN (R 4.3.1)
# P camcorder     0.1.0      2022-10-03 [?] CRAN (R 4.3.0)
# cli           3.6.2      2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace    2.1-0      2023-01-23 [?] CRAN (R 4.3.0)
# P commonmark    1.9.0      2023-03-17 [?] CRAN (R 4.3.0)
# P compiler      4.3.3      2024-02-29 [?] local
# cowplot       1.1.3      2024-01-22 [1] CRAN (R 4.3.3)
# P crayon        1.5.2      2022-09-29 [?] CRAN (R 4.3.0)
# P curl          5.2.0      2023-12-08 [?] RSPM (R 4.3.0)
# P datasets    * 4.3.3      2024-02-29 [?] local
# P digest        0.6.33     2023-07-07 [?] CRAN (R 4.3.1)
# P dplyr       * 1.1.4      2023-11-17 [?] RSPM (R 4.3.0)
# P fansi         1.0.6      2023-12-08 [?] RSPM (R 4.3.0)
# P farver        2.1.1      2022-07-06 [?] CRAN (R 4.3.0)
# P fastmap       1.1.1      2023-02-24 [?] CRAN (R 4.3.0)
# P forcats     * 1.0.0      2023-01-29 [?] CRAN (R 4.3.0)
# P forecast    * 8.22.0     2024-03-04 [?] CRAN (R 4.3.3)
# P fracdiff      1.5-3      2024-02-01 [?] CRAN (R 4.3.3)
# P generics      0.1.3      2022-07-05 [?] CRAN (R 4.3.0)
# P ggplot2     * 3.5.0      2024-02-23 [?] CRAN (R 4.3.2)
# ggstream      0.1.0      2021-05-06 [1] CRAN (R 4.3.3)
# P ggtext      * 0.1.2      2022-09-16 [?] CRAN (R 4.3.0)
# P ggthemes    * 5.1.0      2024-02-10 [?] CRAN (R 4.3.3)
# P gifski        1.12.0-2   2023-08-12 [?] CRAN (R 4.3.1)
# P glue        * 1.7.0      2024-01-09 [?] CRAN (R 4.3.2)
# P graphics    * 4.3.3      2024-02-29 [?] local
# P grDevices   * 4.3.3      2024-02-29 [?] local
# P grid          4.3.3      2024-02-29 [?] local
# P gridtext      0.1.5      2022-09-16 [?] CRAN (R 4.3.0)
# P gtable        0.3.4      2023-08-21 [?] CRAN (R 4.3.1)
# P here          1.0.1      2020-12-13 [?] CRAN (R 4.3.0)
# P hms           1.1.3      2023-03-21 [?] CRAN (R 4.3.0)
# P htmltools     0.5.7      2023-11-03 [?] RSPM (R 4.3.0)
# P janitor     * 2.2.0      2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite      1.8.8      2023-12-04 [?] RSPM (R 4.3.0)
# P knitr         1.45       2023-10-30 [?] RSPM (R 4.3.0)
# P labeling      0.4.3      2023-08-29 [?] CRAN (R 4.3.1)
# P lattice       0.22-5     2023-10-24 [?] CRAN (R 4.3.3)
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.3.2)
# P lmtest        0.9-40     2022-03-21 [?] CRAN (R 4.3.0)
# P lubridate   * 1.9.3      2023-09-27 [?] RSPM (R 4.3.0)
# P magick        2.8.3      2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr      2.0.3      2022-03-30 [?] CRAN (R 4.3.0)
# P markdown      1.12       2023-12-06 [?] CRAN (R 4.3.2)
# P Matrix        1.6-5      2024-01-11 [?] CRAN (R 4.3.3)
# P MetBrewer   * 0.2.0      2022-03-21 [?] CRAN (R 4.3.1)
# P methods     * 4.3.3      2024-02-29 [?] local
# P mgcv          1.9-1      2023-12-21 [?] CRAN (R 4.3.3)
# MoMAColors  * 0.0.0.9000 2024-03-31 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# P munsell       0.5.0      2018-06-12 [?] CRAN (R 4.3.0)
# P nlme          3.1-164    2023-11-27 [?] CRAN (R 4.3.3)
# P nnet          7.3-19     2023-05-03 [?] CRAN (R 4.3.3)
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.3.0)
# P parallel      4.3.3      2024-02-29 [?] local
# P patchwork     1.2.0      2024-01-08 [?] CRAN (R 4.3.2)
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.3.0)
# P pkgload       1.3.4      2024-01-16 [?] RSPM (R 4.3.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.3.1)
# P quadprog      1.5-8      2019-11-20 [?] CRAN (R 4.3.0)
# P quantmod      0.4.26     2024-02-14 [?] CRAN (R 4.3.3)
# P R.cache       0.16.0     2022-07-21 [?] CRAN (R 4.3.2)
# P R.methodsS3   1.8.2      2022-06-13 [?] CRAN (R 4.3.1)
# P R.oo          1.26.0     2024-01-24 [?] CRAN (R 4.3.2)
# P R.utils       2.12.3     2023-11-18 [?] CRAN (R 4.3.2)
# P R6            2.5.1      2021-08-19 [?] CRAN (R 4.3.0)
# P ragg          1.2.7      2023-12-11 [?] RSPM (R 4.3.0)
# Rcpp          1.0.12     2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5      2024-01-10 [?] CRAN (R 4.3.2)
# renv          1.0.5      2024-02-29 [1] CRAN (R 4.3.3)
# P repr          1.1.6      2023-01-26 [?] CRAN (R 4.3.0)
# P rlang         1.1.3      2024-01-10 [?] CRAN (R 4.3.2)
# P rprojroot     2.0.4      2023-11-05 [?] RSPM (R 4.3.0)
# P rstudioapi    0.15.0     2023-07-07 [?] CRAN (R 4.3.1)
# P rsvg          2.6.0      2023-10-08 [?] RSPM (R 4.3.0)
# P scales      * 1.3.0      2023-11-28 [?] CRAN (R 4.3.2)
# P sessioninfo   1.2.2      2021-12-06 [?] CRAN (R 4.3.0)
# P showtext    * 0.9-7      2024-03-02 [?] CRAN (R 4.3.3)
# P showtextdb  * 3.0        2020-06-04 [?] CRAN (R 4.3.0)
# P skimr       * 2.1.5      2022-12-23 [?] CRAN (R 4.3.0)
# P snakecase     0.11.1     2023-08-27 [?] CRAN (R 4.3.1)
# P splines       4.3.3      2024-02-29 [?] local
# P stats       * 4.3.3      2024-02-29 [?] local
# P stringi       1.8.3      2023-12-11 [?] RSPM (R 4.3.0)
# P stringr     * 1.5.1      2023-11-14 [?] RSPM (R 4.3.0)
# P styler        1.10.2     2023-08-29 [?] CRAN (R 4.3.2)
# P svglite       2.1.1      2023-01-10 [?] CRAN (R 4.3.0)
# P sysfonts    * 0.8.9      2024-03-02 [?] CRAN (R 4.3.3)
# P systemfonts   1.0.5      2023-10-09 [?] RSPM (R 4.3.0)
# P textshaping   0.3.6      2021-10-13 [?] CRAN (R 4.3.0)
# P tibble      * 3.2.1      2023-03-20 [?] CRAN (R 4.3.0)
# P tidyr       * 1.3.1      2024-01-24 [?] CRAN (R 4.3.2)
# tidyselect    1.2.1      2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse   * 2.0.0      2023-02-22 [?] CRAN (R 4.3.0)
# P timechange    0.2.0      2023-01-11 [?] CRAN (R 4.3.0)
# P timeDate      4032.109   2023-12-14 [?] CRAN (R 4.3.2)
# P tools         4.3.3      2024-02-29 [?] local
# P tseries       0.10-55    2023-12-06 [?] CRAN (R 4.3.3)
# P TTR           0.24.4     2023-11-28 [?] CRAN (R 4.3.3)
# P tzdb          0.4.0      2023-05-12 [?] CRAN (R 4.3.0)
# P urca          1.3-3      2022-08-29 [?] CRAN (R 4.3.0)
# P utf8          1.2.4      2023-10-22 [?] CRAN (R 4.3.2)
# P utils       * 4.3.3      2024-02-29 [?] local
# P vctrs         0.6.5      2023-12-01 [?] CRAN (R 4.3.2)
# P vroom         1.6.5      2023-12-05 [?] RSPM (R 4.3.0)
# P withr         3.0.0      2024-01-16 [?] CRAN (R 4.3.2)
# P xfun          0.41       2023-11-01 [?] CRAN (R 4.3.2)
# P xml2          1.3.6      2023-12-04 [?] RSPM (R 4.3.0)
# P xts           0.13.2     2024-01-21 [?] CRAN (R 4.3.3)
# P zoo           1.8-12     2023-04-13 [?] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────
# > 