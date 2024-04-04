
## Challenge: #30DayChartChallenge 2024 day 04
## Topic:     Comparisons | waffle
## Author:    Steven Ponce
## Date:      2024-04-04

## Data:      Social Media Users Dataset (via Arindam Sahoo) 
## Link:      https://www.kaggle.com/datasets/arindamsahoo/social-media-users


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
tidyverse, # Easily Install and Load the 'Tidyverse'
ggtext,    # Improved Text Rendering Support for 'ggplot2'
showtext,  # Using Fonts More Easily in R Graphs
janitor,   # Simple Tools for Examining and Cleaning Dirty Data
skimr,     # Compact and Flexible Summaries of Data
scales,    # Scale Functions for Visualization
lubridate, # Make Dealing with Dates a Little Easier
glue,      # Interpreted String Literals
readxl,    # Read Excel Files,
MetBrewer, # Color Palettes Inspired by Works at the Metropolitan Museum of Art
waffle     # Create Waffle Chart Visualizations
)



### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 8,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
worldwide_hydrogen_projects <- read_xlsx(here::here("2024/data/Hydrogen_production_projects_corrected.xlsx"),
  sheet = "Projects", skip = 3, trim_ws = TRUE
) |>
  clean_names() |>
  glimpse()


## 3. EXAMINING THE DATA ----
glimpse(worldwide_hydrogen_projects)
skim(worldwide_hydrogen_projects)
colnames(worldwide_hydrogen_projects)


## 4. TIDYDATA ----

### |- Tidy ----
h2_usa_tbl <- worldwide_hydrogen_projects |>
  # Filter for hydrogen and USA
  filter(
    product == "H2",
    country == "USA",
  ) |> 
  # Summary
  summarise(
    .by   = c(status, technology),    
    count = n(),
  ) |> 
  # Add pct
  mutate(
    total = sum(count),
    pct = count / total * 100,
    pct = round(pct, digits = 0)
  ) |> 
  ungroup() |> 
  arrange(desc(pct)) |> 
  # Format and reorder
  mutate(
    technology = case_when(
      technology == "ALK" ~ "Alkaline",
      TRUE ~ as_factor(technology)
    ),
    technology = fct_reorder(technology, pct)
  )
  


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#dedfdb', 0.85)      
title_col    <- "#1a1a1a"              
subtitle_col <- "gray30"    
caption_col  <- "gray30"   
text_col     <- "gray30"     
col_palette  <- met.brewer("Derain", n = 6, type = "discrete", direction = -1)


### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 04 } &bull; Source: The Hydrogen Production Projects Database<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Hydrogen Production Technologies in the USA") 

subtitle_text <- str_glue("1 square equals to 1%\n")

fill_text     <- "Project Status: "

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Kanit", regular.wt = 400, family = "title")       
font_add_google("Mulish", family = "subtitle")  
font_add_google("Mulish", regular.wt = 400, family = "text")        
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_void(base_size = 12, base_family = "text"))

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "top",
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  legend.margin         = margin(t = 10, r = 10, b = 20, l = 10),
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  strip.text            = element_textbox(size     = rel(1),
                                          face     = 'bold',
                                          color    = text_col,
                                          hjust    = 0.5,
                                          halign   = 0.5,
                                          fill     = "transparent"),
  panel.spacing         = unit(1, 'lines'),
)


### |-  final plot ----  

# Reference: https://github.com/hrbrmstr/waffle

h2_usa_tbl |> 
  ggplot(aes(fill = status, values = pct)) +
  
  # Geoms
  waffle::geom_waffle(
    color  = "#fafafa", 
    size   = 0.33,
    n_rows = 10,
    flip   = TRUE,
    make_proportional = TRUE, 
  ) +
  
  # Scales
  coord_equal(expand = FALSE, clip = 'off') +
  scale_fill_manual(values = col_palette) +
  
  # Facets
  facet_wrap(vars(technology)) +
  
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    fill     = fill_text
  ) +
  
  # Theme
  theme(
    legend.title    = element_markdown(size = rel(.85), hjust = 0.5),
    legend.text     = element_text(size = rel(0.8)),
    
    plot.title      = element_text(
      size          = rel(1.4),
      family        = "title",
      face          = "bold",
      color         = title_col,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_text(
      size          = rel(1),
      family        = "subtitle",
      color         = title_col,
      lineheight    = 1.1,
      hjust         = 0.5,
      margin        = margin(t = 10, b = 5)
    ),
    plot.caption    = element_markdown(
      size          = rel(.65),
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

# ─ Session info ─────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-04-02
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.3    2024-02-29 [2] local
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.3.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.3.0)
# P cellranger     1.1.0    2016-07-27 [?] CRAN (R 4.3.0)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace     2.1-0    2023-01-23 [?] CRAN (R 4.3.0)
# P commonmark     1.9.0    2023-03-17 [?] CRAN (R 4.3.0)
# P compiler       4.3.3    2024-02-29 [?] local
# P curl           5.2.0    2023-12-08 [?] RSPM (R 4.3.0)
# P datasets     * 4.3.3    2024-02-29 [?] local
# P digest         0.6.33   2023-07-07 [?] CRAN (R 4.3.1)
# P dplyr        * 1.1.4    2023-11-17 [?] RSPM (R 4.3.0)
# P DT             0.32     2024-02-19 [?] CRAN (R 4.3.3)
# P extrafont      0.19     2023-01-18 [?] CRAN (R 4.3.0)
# P extrafontdb    1.0      2012-06-11 [?] CRAN (R 4.3.0)
# P fansi          1.0.6    2023-12-08 [?] RSPM (R 4.3.0)
# P farver         2.1.1    2022-07-06 [?] CRAN (R 4.3.0)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.3.0)
# P forcats      * 1.0.0    2023-01-29 [?] CRAN (R 4.3.0)
# P generics       0.1.3    2022-07-05 [?] CRAN (R 4.3.0)
# P ggplot2      * 3.5.0    2024-02-23 [?] CRAN (R 4.3.2)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.3.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.3.1)
# P glue         * 1.7.0    2024-01-09 [?] CRAN (R 4.3.2)
# P graphics     * 4.3.3    2024-02-29 [?] local
# P grDevices    * 4.3.3    2024-02-29 [?] local
# P grid           4.3.3    2024-02-29 [?] local
# P gridExtra      2.3      2017-09-09 [?] CRAN (R 4.3.0)
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.3.0)
# P gtable         0.3.4    2023-08-21 [?] CRAN (R 4.3.1)
# P here           1.0.1    2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.3.0)
# P htmltools      0.5.7    2023-11-03 [?] RSPM (R 4.3.0)
# P htmlwidgets    1.6.4    2023-12-06 [?] RSPM (R 4.3.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite       1.8.8    2023-12-04 [?] RSPM (R 4.3.0)
# P knitr          1.45     2023-10-30 [?] RSPM (R 4.3.0)
# P labeling       0.4.3    2023-08-29 [?] CRAN (R 4.3.1)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.3.0)
# P magick         2.8.3    2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.3.0)
# P markdown       1.12     2023-12-06 [?] CRAN (R 4.3.2)
# P MetBrewer    * 0.2.0    2022-03-21 [?] CRAN (R 4.3.1)
# P methods      * 4.3.3    2024-02-29 [?] local
# P munsell        0.5.0    2018-06-12 [?] CRAN (R 4.3.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.3.0)
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.3.0)
# P plyr           1.8.9    2023-10-02 [?] CRAN (R 4.3.2)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.3.0)
# P ragg           1.2.7    2023-12-11 [?] RSPM (R 4.3.0)
# P RColorBrewer   1.1-3    2022-04-03 [?] CRAN (R 4.3.0)
# P Rcpp           1.0.11   2023-07-06 [?] CRAN (R 4.3.1)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.3.2)
# P readxl       * 1.4.3    2023-07-06 [?] CRAN (R 4.3.1)
# renv           1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr           1.1.6    2023-01-26 [?] CRAN (R 4.3.0)
# P rlang          1.1.3    2024-01-10 [?] CRAN (R 4.3.2)
# P rprojroot      2.0.4    2023-11-05 [?] RSPM (R 4.3.0)
# P rstudioapi     0.15.0   2023-07-07 [?] CRAN (R 4.3.1)
# P rsvg           2.6.0    2023-10-08 [?] RSPM (R 4.3.0)
# P Rttf2pt1       1.3.12   2023-01-22 [?] CRAN (R 4.3.0)
# P scales       * 1.3.0    2023-11-28 [?] CRAN (R 4.3.2)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.3.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.3.3)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.3.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.3.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.3.1)
# P stats        * 4.3.3    2024-02-29 [?] local
# P stringi        1.8.3    2023-12-11 [?] RSPM (R 4.3.0)
# P stringr      * 1.5.1    2023-11-14 [?] RSPM (R 4.3.0)
# P svglite        2.1.1    2023-01-10 [?] CRAN (R 4.3.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.3.3)
# P systemfonts    1.0.5    2023-10-09 [?] RSPM (R 4.3.0)
# P textshaping    0.3.6    2021-10-13 [?] CRAN (R 4.3.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.3.0)
# P tidyr        * 1.3.1    2024-01-24 [?] CRAN (R 4.3.2)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.3.0)
# P timechange     0.2.0    2023-01-11 [?] CRAN (R 4.3.0)
# P tools          4.3.3    2024-02-29 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.3.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.3.2)
# P utils        * 4.3.3    2024-02-29 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.3.2)
# P waffle       * 1.0.2    2023-09-30 [?] CRAN (R 4.3.3)
# P withr          3.0.0    2024-01-16 [?] CRAN (R 4.3.2)
# P xfun           0.41     2023-11-01 [?] CRAN (R 4.3.2)
# P xml2           1.3.6    2023-12-04 [?] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────
# > 
