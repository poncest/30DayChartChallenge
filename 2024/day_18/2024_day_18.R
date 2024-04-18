
## Challenge: #30DayChartChallenge 2024 day 18
## Topic:     Relationships | Asian Development Bank (data day)
## Author:    Steven Ponce
## Date:      2024-04-18

## Data:      ABD - Agriculture, Value Added (% of GDP) in Asia and the Pacific
##            Agriculture, Value Added (% of GDP) values by regional member and subregion. 
##            This dataset shows the pattern of growth per decade from 1966 to 2016.
## Link:      https://data.adb.org/dataset/agriculture-value-added-gdp-asia-and-pacific 

## Citation:   
#' Asian Development Bank. 2017. 
#' [Agriculture, Value Added (% of GDP) in Asia and the Pacific)]. 
#' Accessed [2024-04-18].


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
  patchwork,   # The Composer of Plots
  gghighlight  # Highlight Lines and Points in 'ggplot2'
)


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 5,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
agriculture_data <- read_csv(here::here("2024/data/adb_agriculture-value-added.csv")) |>
  clean_names() |>
  glimpse()


## 3. EXAMINING THE DATA ----
glimpse(agriculture_data)
skim(agriculture_data)
colnames(agriculture_data)

agriculture_data |> count(year, sort = T)
agriculture_data |> count(regional_member, sort = T)
agriculture_data |> count(subregion, sort = T)            


## 4. TIDYDATA ----

### |- tidy data ---- 

# Subregion df
subregion_df <- agriculture_data |>
  summarise(
    agriculture_value_added_percent_of_gdp = mean(agriculture_value_added_percent_of_gdp),
    .by = c(year, subregion)
  ) |>
  drop_na(subregion)

# World df
world_df <- agriculture_data |>
  filter(regional_member == "World") |>
  mutate(subregion = "World") |>
  select(year, subregion, agriculture_value_added_percent_of_gdp)

### |- combined data ---
combined_df <- bind_rows(subregion_df, world_df)

### |- plot data ---
data_plot <- combined_df |>
  mutate(
    name = subregion,
    agriculture_value_added_percent_of_gdp = agriculture_value_added_percent_of_gdp / 100,
    subregion = fct_reorder(subregion, agriculture_value_added_percent_of_gdp, .desc = TRUE),
  ) |>
  rename(
    value = agriculture_value_added_percent_of_gdp
  )


# 5. VISUALIZATION ---- 
bkg_col      <- colorspace::lighten('#020100', 0.05)   
title_col    <- "#FDFFFC"           
subtitle_col <- "#FDFFFC"                 
caption_col  <- "#FDFFFC"                
text_col     <- "#FDFFFC"  
gray_line_color      <- "grey65"
highlight_line_color <- "#F1D302"


### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 18 } &bull; Source: ADB - Agriculture, Value Added (% of GDP) in Asia and the Pacific<br>") 
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Slowdown in Agriculture Production, 1966 - 2015") 

subtitle_text <- str_glue("In agriculture, value added (% of GDP) is a metric that represents the net output of an agricultural sector after subtracting<br>
                          intermediate inputs and adding up all outputs. It is calculated without taking into account asset depreciation or depletion<br>
                          and degradation of natural resources. ")

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
  axis.line.x           = element_line(color = "#fafafa", linewidth = .2),
  panel.grid.major.y    = element_line(linetype = "solid", linewidth = 0.02, color = "#fafafa"),
  panel.grid.minor.y    = element_blank(),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
  strip.text            = element_textbox(size     = rel(.85),
                                          face     = 'bold',
                                          color    = text_col,
                                          hjust    = 0.5,
                                          halign   = 0.5,
                                          fill     = "transparent"),
  panel.spacing         = unit(1, 'lines')
)


### |-  plot function ----
plot_agriculture_value_added <- function(data, highlight_line_color, gray_line_color) {
  
  p <- data  |> 
    ggplot(aes(x = year, y = value, group = subregion)) +
    
    # Highlighted lines with specified color
    geom_line(linewidth = 1, color = highlight_line_color) +
    
    # Background data with gghighlight for gray lines
    gghighlight::gghighlight(
      unhighlighted_params = list(
        linewidth = 0.5,
        color = alpha(gray_line_color, 0.4)
      ),
      use_direct_label = FALSE
    ) +
    
    # Scales
    scale_x_continuous(limits = c(1966, 2015)) +
    scale_y_continuous(labels = scales::percent) +
    coord_cartesian(clip = 'off') +
 
    # Labels
    labs(
      x = element_blank(),
      y = "Agriculture value added (% of GDP)"
    ) +
    
    # Facets
    facet_wrap(vars(subregion), ncol = 2) 
  
  return(p)
}

### |-  world plot ---- 
world_plot <- data_plot |>
  filter(subregion == "World") |>
  plot_agriculture_value_added(
    highlight_line_color = highlight_line_color,
    gray_line_color = gray_line_color
  ) +
  scale_y_continuous(
    breaks = seq(0.03, .06, by = .01),
    limits = c(.03, .06),
    labels = scales::percent
  )

### |-  region plot ----  
region_plot <- data_plot |>
  filter(subregion != "World") |>
  plot_agriculture_value_added(
    highlight_line_color = highlight_line_color,
    gray_line_color = gray_line_color
  ) +
  scale_y_continuous(
    breaks = seq(0.03, .4, by = .1),
    limits = c(.03, .4),
    labels = scales::percent
  )

### |-  main plot ----  
main_plot <- world_plot + region_plot +
  patchwork::plot_layout(
    guides = "collect",
    widths = c(1, 2)
  )

### |-  title plot ----  
title_plot <- ggplot() + 
  theme_void() + 
  
  # labs
  labs(
    title    = title_text,
    subtitle = subtitle_text
  ) +
  
  # Theme
  theme(
    plot.title      = element_text(
      size          = rel(2.4),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(.85), 
      family        = 'subtitle',
      color         = subtitle_col,
      lineheight    = 1.1, 
      margin        = margin(t = 0, b = 1)
    ))

### |-  caption plot ---- 
caption_plot <- ggplot() + 
  theme_void() + 
  
  # labs
  labs(caption = caption_text) +
  
  # Theme
  theme(
    plot.caption = element_markdown(
      size       = rel(.5),
      family     = "caption",
      color      = caption_col,
      lineheight = 1.1,
      hjust      = 0.5,
      halign     = 0.5,
      margin     = margin(t = 5, b = 5)
    ))

### |-  final plot ----  
final_plot <- (title_plot /
  (main_plot) /
  caption_plot) +
  patchwork::plot_layout(
    guides = "collect",
    heights = c(0, 1, 0)
  )

final_plot 


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
# date     2024-04-16
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
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
# P gghighlight * 0.4.1      2023-12-16 [?] RSPM (R 4.3.0)
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
# P patchwork   * 1.2.0      2024-01-08 [?] CRAN (R 4.3.2)
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.3.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.3.1)
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
# ──────────────────────────────────────────────────────────────────────────────
# > 