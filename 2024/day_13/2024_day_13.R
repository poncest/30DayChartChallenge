
## Challenge: #30DayChartChallenge 2024 day 13
## Topic:     Relationships | family
## Author:    Steven Ponce
## Date:      2024-04-13

## Data:      Family Guy Dataset (via Sourav Banerjee) 
##            Navigating the Quirky Universe of Family Guy: A Comprehensive Dataset
## Link:      https://www.kaggle.com/datasets/iamsouravbanerjee/family-guy-dataset


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  skimr,       # Compact and Flexible Summaries of Data
  scales,      # Scale Functions for Visualization
  lubridate,   # Make Dealing with Dates a Little Easier
  glue,        # Interpreted String Literals
  MetBrewer,   # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors   # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
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
family_guy_data <- read_csv(here::here("2024/data/Family_Guy_Dataset.csv")) |>
  clean_names() |>
  glimpse()


## 3. EXAMINING THE DATA ----
glimpse(family_guy_data)
skim(family_guy_data)
colnames(family_guy_data)


## 4. TIDYDATA ----

### |- Tidy ----
family_guy_tidy <- family_guy_data |>
  # Rename columns
  rename(
    imdb_rating = im_db_rating,
    us_viewers_millions = u_s_viewers_millions
  ) |>
  # Change column type
  mutate(
    imdb_rating = as.numeric(imdb_rating),
    us_viewers_millions = as.numeric(us_viewers_millions)
  ) |>
  # Add year column
  mutate(
    original_air_date = mdy(original_air_date),
    year = year(original_air_date)
  ) |>
  # Add a logical column for highlighting based on viewership
  mutate(highlight = case_when(
    us_viewers_millions >= 10.0 ~ "yes",
    TRUE ~ "no"
  )) |>
  # Remove rows with missing year data
  filter(!is.na(year))

### |- Plot data ----
plot_data <- family_guy_tidy |> 
  select(original_air_date, year, season, no_of_episode_season, imdb_rating,
         us_viewers_millions, highlight)


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#F2DC6D', 0.2)    
title_col    <- "#0499DB"            
subtitle_col <- "gray10"     
caption_col  <- "gray20"   
text_col     <- colorspace::darken("gray20" , 0.15)    
col_palette  <- c("no" = "#2f662f", "yes" = "#D96690")

### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 13 } &bull; Source: Family Guy Dataset (via Sourav Banerjee)<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text   <- str_glue("Family Guy") 

ten <- str_glue("<span style='color:{ col_palette[2] }'>**10 or more**</span>")

subtitle_text <- str_glue("IMDb Rating vs. Air Date by Viewership, 1999 - 2023<br>
                          Highlighting episodes with { ten } million views")

caption_text <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("Titan One", regular.wt = 400, family = "title")                 
font_add_google("Noto Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Noto Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "top",
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
  axis.line             = element_line(color = "gray", linewidth = .5),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "solid", linewidth = 0.1, color = "gray"),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
)

 
### |-  final plot ----  
plot_data |> 
  # Geoms
  ggplot(aes(
    x = original_air_date, y = imdb_rating, size = us_viewers_millions,
    color = highlight
  )) +
  geom_point(
    data = plot_data |> filter(highlight == "yes"),
    alpha = 0.65, shape = 16, color = col_palette[2]
  ) +
  geom_point(
    data = plot_data |> filter(highlight == "yes"),
    alpha = 0.65, shape = 21, color = col_palette[1], stroke = 0.5
  ) +
  geom_point(
    data = plot_data |> filter(highlight == "no"),
    alpha = .7, shape = 16, color = col_palette[1]
  ) +

  # Scales
  scale_x_date(position = "top") +
  scale_y_continuous(
    breaks = seq(4, 10, by = 1),
    limits = c(4, 10),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_size(
    name = "Viewership: ",
    breaks = c(5, 10, 15, 20),
    range  = c(.5, 5),
    labels = c("5M", "10M", "15M", "20M")
  ) +
  coord_cartesian(clip = "off")+
  
  # Labs
  labs(
    x        = "Original Air Date",
    y        = "IMDb Rating",
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  # Theme
  theme(
    axis.title.x.top = element_markdown(
      margin = margin(0, 0, 10, 0), 
      size   = rel(.9),
      color  = text_col, 
      family = "text", 
      face   = "bold"
    ),
    legend.title = element_markdown(
      size   = rel(.75), 
      hjust  = 0.5,
      family = "text",
      face   = "bold",
      color  = text_col
    ),
    legend.text = element_text(
      size   = rel(0.7),
      family = "text",
      face   = "bold",
      color  = text_col
    ),

    legend.spacing.y = unit(-0.5, "cm"),                                        # Negative value to move up
    legend.key.size  = unit(0.5, "cm"),                                         # Adjust size of the legend keys if necessary
    legend.margin    = margin(t = 5, b = -5, unit = "pt"),                      # Use negative top margin to move up

    plot.title      = element_text(
      size          = rel(3.1),
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
      lineheight    = 1.4, 
      margin        = margin(t = 5, b = 1)
    ),
    plot.caption    = element_markdown(
      size          = rel(.5),
      family        = "caption",
      color         = caption_col,
      lineheight    = 0.65,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 5, b = 5)
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
# date     2024-04-11
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# base        * 4.3.3      2024-02-29 [2] local
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.3.0)
# P bit           4.0.5      2022-11-15 [?] CRAN (R 4.3.0)
# P bit64         4.0.5      2020-08-30 [?] CRAN (R 4.3.0)
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
# P generics      0.1.3      2022-07-05 [?] CRAN (R 4.3.0)
# P ggplot2     * 3.5.0      2024-02-23 [?] CRAN (R 4.3.2)
# ggstream      0.1.0      2021-05-06 [1] CRAN (R 4.3.3)
# P ggtext      * 0.1.2      2022-09-16 [?] CRAN (R 4.3.0)
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
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate   * 1.9.3      2023-09-27 [?] RSPM (R 4.3.0)
# P magick        2.8.3      2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr      2.0.3      2022-03-30 [?] CRAN (R 4.3.0)
# P markdown      1.12       2023-12-06 [?] CRAN (R 4.3.2)
# P MetBrewer   * 0.2.0      2022-03-21 [?] CRAN (R 4.3.1)
# P methods     * 4.3.3      2024-02-29 [?] local
# MoMAColors  * 0.0.0.9000 2024-03-31 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# P munsell       0.5.0      2018-06-12 [?] CRAN (R 4.3.0)
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.3.0)
# P parallel      4.3.3      2024-02-29 [?] local
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.3.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.3.1)
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
# P tools         4.3.3      2024-02-29 [?] local
# P tzdb          0.4.0      2023-05-12 [?] CRAN (R 4.3.0)
# P utf8          1.2.4      2023-10-22 [?] CRAN (R 4.3.2)
# P utils       * 4.3.3      2024-02-29 [?] local
# P vctrs         0.6.5      2023-12-01 [?] CRAN (R 4.3.2)
# P vroom         1.6.5      2023-12-05 [?] RSPM (R 4.3.0)
# P withr         3.0.0      2024-01-16 [?] CRAN (R 4.3.2)
# P xfun          0.41       2023-11-01 [?] CRAN (R 4.3.2)
# P xml2          1.3.6      2023-12-04 [?] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────
# > 
