
## Challenge: #30DayChartChallenge 2024 day 07
## Topic:     Distributions | hazard
## Author:    Steven Ponce
## Date:      2024-04-07

## Data:      Department of Transportation Pipeline Incidents (via data.world)
##            accident hazardous liquid 1986 - 2002
## Link:      https://data.world/dot/pipeline-incidents


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
  MetBrewer, # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  ggdist     # Visualizations of Distributions and Uncertainty
)


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 6,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
liquid_hazards_1986_2002 <- read_tsv(here::here("2024/data/accident_hazardous_liquid_1986_jan2002.txt")) |>
  clean_names() |>
  glimpse()


## 3. EXAMINING THE DATA ----
glimpse(liquid_hazards_1986_2002)
skim(liquid_hazards_1986_2002)
colnames(liquid_hazards_1986_2002)

liquid_hazards_1986_2002 |>  count(name, sort = T)
liquid_hazards_1986_2002 |>  count(csys, sort = T)  #  Part of system involved
liquid_hazards_1986_2002 |>  count(caus, sort = T)  #  Apparent Cause of accident  


## 4. TIDYDATA ----

### |- Tidy ----
data_plot <- liquid_hazards_1986_2002 |> 
  # Extract year
  mutate(
    year = str_extract(rptid, "^\\d{4}"),
    year = as.integer(year)
  ) |> 
  # Select and rename columns
  select(year, system = csys , cause = caus) |> 
  # Format to title case
  mutate(
    across(c(system, cause), str_to_title)
  ) |> 
  # Sum by month
  group_by(system, cause, year) |> 
  summarise(count = n(), .groups = 'drop') |> 
  # Format and convert to factors 
  mutate(
    cause  = fct_reorder(cause, count, .desc = TRUE, .na_rm = TRUE),
    cause  = str_wrap(cause, width = 30, indent = 0),
    system = factor(system)
  )
  

# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#fafafa'
title_col    <- "gray10"              
subtitle_col <- "gray20"   
caption_col  <- "gray20"  
text_col     <- "gray20"    
col_palette  <- met.brewer("Johnson", n = 5, type = "discrete")

### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 07 } &bull; Source: DOT via data.world<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Distribution of Pipeline Incidents Causes by System Type,\n1985 - 2002") 

caption_text <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Poppins", regular.wt = 400, family = "title")       
font_add_google("Poppins", family = "subtitle")  
font_add_google("Inter", regular.wt = 400, family = "text")        
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 10, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'top',
  
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  
  axis.text             = element_text(size = rel(.8), color = text_col, family = 'text'),
  
  axis.line.x           = element_line(color = "black", linewidth = .2),
  
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.3, color = 'gray'),
)


### |-  final plot ----  

# Reference: https://mjskay.github.io/ggdist/

data_plot |> 
  ggplot(aes(x = count, y = cause, fill = system, color = system)) +    
  
  # Geoms
  ggdist::geom_dots(smooth = "bar", group = NA, dotsize = 0.9) +
  annotate("text",
    x = 0, y = "Outside Force Damage", label = "Cause", vjust = -2.5, hjust = 2,
    size = 3.5, color = text_col, fontface = "bold", family = "text"
  ) +

  # Scales
  scale_x_log10() + 
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.0))) +
  scale_fill_manual(values = col_palette, guide = "none") +
  scale_color_manual(values = col_palette, guide = "none") +
  coord_cartesian(clip = "off") +
  
  guides(fill = guide_legend(
    title = "Part of System Involved:",
    title.position = "top",
    nrow     = 2,
    keywidth = 1,
    byrow    = TRUE
      )
    ) +
  
  # Labs
  labs(
    x       = "Incidents Count (Log Scale)",
    y       = element_blank(),
    title   = title_text,
    caption = caption_text,
  ) +
  
  # Theme
  theme(
    legend.title       = element_text(size = rel(.85), hjust = 0.5),
    legend.text        = element_text(size = rel(0.8), lineheight = .5),
    legend.box.spacing = unit(0.1, "lines"),  
    legend.box.margin  = margin(0, 0, 5, 0),
    legend.key.height  = unit(.5, "lines"),
    legend.margin      = margin(t = 0, r = 0, b = 0, l = 0),

    plot.title      = element_text(
      size          = rel(1.35),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 10)
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

# ─ Session info ───────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-04-03
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# base        * 4.3.3    2024-02-29 [2] local
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.3.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.3.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.3.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.3.0)
# cli           3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace    2.1-0    2023-01-23 [?] CRAN (R 4.3.0)
# P commonmark    1.9.0    2023-03-17 [?] CRAN (R 4.3.0)
# P compiler      4.3.3    2024-02-29 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.3.0)
# P curl          5.2.0    2023-12-08 [?] RSPM (R 4.3.0)
# P datasets    * 4.3.3    2024-02-29 [?] local
# P digest        0.6.33   2023-07-07 [?] CRAN (R 4.3.1)
# P dplyr       * 1.1.4    2023-11-17 [?] RSPM (R 4.3.0)
# P fansi         1.0.6    2023-12-08 [?] RSPM (R 4.3.0)
# P farver        2.1.1    2022-07-06 [?] CRAN (R 4.3.0)
# P fastmap       1.1.1    2023-02-24 [?] CRAN (R 4.3.0)
# P forcats     * 1.0.0    2023-01-29 [?] CRAN (R 4.3.0)
# P fs            1.6.3    2023-07-20 [?] CRAN (R 4.3.1)
# P generics      0.1.3    2022-07-05 [?] CRAN (R 4.3.0)
# P ggplot2     * 3.5.0    2024-02-23 [?] CRAN (R 4.3.2)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.3.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.3.1)
# P glue        * 1.7.0    2024-01-09 [?] CRAN (R 4.3.2)
# P graphics    * 4.3.3    2024-02-29 [?] local
# P grDevices   * 4.3.3    2024-02-29 [?] local
# P grid          4.3.3    2024-02-29 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.3.0)
# P gtable        0.3.4    2023-08-21 [?] CRAN (R 4.3.1)
# P gutenbergr  * 0.2.4    2023-11-12 [?] CRAN (R 4.3.3)
# P here          1.0.1    2020-12-13 [?] CRAN (R 4.3.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.3.0)
# P htmltools     0.5.7    2023-11-03 [?] RSPM (R 4.3.0)
# P janeaustenr   1.0.0    2022-08-26 [?] CRAN (R 4.3.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite      1.8.8    2023-12-04 [?] RSPM (R 4.3.0)
# P knitr         1.45     2023-10-30 [?] RSPM (R 4.3.0)
# P labeling      0.4.3    2023-08-29 [?] CRAN (R 4.3.1)
# P lattice       0.22-5   2023-10-24 [?] CRAN (R 4.3.3)
# P lazyeval      0.2.2    2019-03-15 [?] CRAN (R 4.3.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate   * 1.9.3    2023-09-27 [?] RSPM (R 4.3.0)
# P magick        2.8.3    2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.3.0)
# P markdown      1.12     2023-12-06 [?] CRAN (R 4.3.2)
# P Matrix        1.6-5    2024-01-11 [?] CRAN (R 4.3.3)
# P MetBrewer   * 0.2.0    2022-03-21 [?] CRAN (R 4.3.1)
# P methods     * 4.3.3    2024-02-29 [?] local
# P munsell       0.5.0    2018-06-12 [?] CRAN (R 4.3.0)
# P pacman        0.5.1    2019-03-11 [?] CRAN (R 4.3.0)
# P parallel      4.3.3    2024-02-29 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.3.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.3.1)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.3.0)
# P ragg          1.2.7    2023-12-11 [?] RSPM (R 4.3.0)
# P rappdirs      0.3.3    2021-01-31 [?] CRAN (R 4.3.0)
# Rcpp          1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.3.2)
# renv          1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr          1.1.6    2023-01-26 [?] CRAN (R 4.3.0)
# P rlang         1.1.3    2024-01-10 [?] CRAN (R 4.3.2)
# P rprojroot     2.0.4    2023-11-05 [?] RSPM (R 4.3.0)
# P rstudioapi    0.15.0   2023-07-07 [?] CRAN (R 4.3.1)
# P rsvg          2.6.0    2023-10-08 [?] RSPM (R 4.3.0)
# P scales      * 1.3.0    2023-11-28 [?] CRAN (R 4.3.2)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.3.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.3.3)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.3.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.3.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.3.1)
# P SnowballC     0.7.1    2023-04-25 [?] CRAN (R 4.3.0)
# P stats       * 4.3.3    2024-02-29 [?] local
# P stringi       1.8.3    2023-12-11 [?] RSPM (R 4.3.0)
# P stringr     * 1.5.1    2023-11-14 [?] RSPM (R 4.3.0)
# P svglite       2.1.1    2023-01-10 [?] CRAN (R 4.3.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.3.3)
# P systemfonts   1.0.5    2023-10-09 [?] RSPM (R 4.3.0)
# P textdata      0.4.4    2022-09-02 [?] CRAN (R 4.3.3)
# P textshaping   0.3.6    2021-10-13 [?] CRAN (R 4.3.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.3.0)
# P tidyr       * 1.3.1    2024-01-24 [?] CRAN (R 4.3.2)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidytext    * 0.4.1    2023-01-07 [?] CRAN (R 4.3.0)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.3.0)
# P timechange    0.2.0    2023-01-11 [?] CRAN (R 4.3.0)
# P tokenizers    0.3.0    2022-12-22 [?] CRAN (R 4.3.0)
# P tools         4.3.3    2024-02-29 [?] local
# P triebeard     0.4.1    2023-03-04 [?] CRAN (R 4.3.2)
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.3.0)
# P urltools      1.7.3    2019-04-14 [?] CRAN (R 4.3.2)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.3.2)
# P utils       * 4.3.3    2024-02-29 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.3.2)
# P vroom         1.6.5    2023-12-05 [?] RSPM (R 4.3.0)
# P withr         3.0.0    2024-01-16 [?] CRAN (R 4.3.2)
# P xfun          0.41     2023-11-01 [?] CRAN (R 4.3.2)
# P xml2          1.3.6    2023-12-04 [?] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────
# > 