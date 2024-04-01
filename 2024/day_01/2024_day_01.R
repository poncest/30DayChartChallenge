
## Challenge: #30DayChartChallenge 2024 day 01
## Topic:     Comparison | part-to-whole
## Author:    Steven Ponce
## Date:      2024-04-01

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
treemapify # Draw Treemaps in 'ggplot2'
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
social_media <- read_csv(here::here("2024/data/SocialMediaUsersDataset.csv")) |> 
            clean_names() |> glimpse()


## 3. EXAMINING THE DATA ----
glimpse(social_media)
skim(social_media)
colnames(social_media)


## 4. TIDYDATA ----

### |- Tidy ----
social_media_longer <- social_media |>
  # Separate wider
  separate_wider_delim(
    cols      = interests,
    delim     = ", ",
    names_sep = "_",
    too_few   = "align_end"
  ) |>
  # Pivot longer
  pivot_longer(
    cols = c(-user_id, -name, -gender, -dob, -city, -country),
    names_to  = "temp",
    values_to = "interests"
  ) |>
  # Format interest column
  mutate(interests = str_remove_all(string = interests, pattern = "'")) |>
  # Drop temp column
  select(-temp) |>
  # Remove NAs
  filter(!is.na(interests))

social_media_longer |> count(country, sort = TRUE) 
social_media_longer |> count(interests, sort = TRUE) 


# Filtered data
data_plot <- social_media_longer |>
  # Filter for PR only
  filter(country == "Puerto Rico") |>
  # Summary
  summarise(
    .by   = c(gender, city, interests),
    count = n()
  ) |>
  mutate(pct = count / sum(count)) |>
  ungroup()



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::darken('#dedfdb', 0.1)      
title_col    <- "#1a1a1a"              
subtitle_col <- "gray30"    
caption_col  <- "gray30"   
text_col     <- "gray30"     
col_palette  <- c("#af8889", "#8d8da7")


### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 01 } &bull; Source: Kaggle via Arindam Sahoo<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("Social Media Interests in Puerto Rico") 

subtitle_text <- str_glue("The major quadrants represent specific interests, and the subgroups correspond to\nmunicipalities in Puerto Rico.")

fill_text     <- "Gender: "

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Russo One", regular.wt = 400, family = "title")       
font_add_google("Abel", family = "subtitle")  
font_add_google("DM Sans", regular.wt = 400, family = "text")        
font_add_google("Roboto Condensed", family = "caption")
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
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = "text", face = "bold"),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = "text", face = "bold"),
  axis.text             = element_text(size = rel(1), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "gray90", linewidth = .2),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.05, color = "gray"),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = "gray"),
)


### |-  final plot ----  

# Reference: https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html

data_plot |>
  ggplot(aes(
    area     = count,
    label    = city,
    subgroup = interests,
    fill     = gender)
    ) +
  # Geoms
  geom_treemap(
    layout = "fixed",
    start  = "bottomleft",
    color  = bkg_col
    ) +          
  geom_treemap_subgroup_border(
    layout = "fixed", 
    size   = 3,
    color  = bkg_col
  ) +          
  geom_treemap_subgroup_text(
    layout    = "fixed",
    place     = "centre",
    grow      = TRUE,
    alpha     = 0.5,
    color     = "black",
    family    = "text_",
    fontface  = "italic",
    min.size  = 0,
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm")
  ) +
  
  # Scales
  scale_fill_manual(values = col_palette) +
  coord_cartesian(expand = FALSE, clip = 'off') +

  # Labs
  labs(
    x        = element_blank(),
    y        = element_blank(),
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    fill     = fill_text
    ) +
  
  # Theme
theme(
  legend.title = element_markdown(size = rel(.85), hjust = 0.5),
  legend.text = element_text(size = rel(0.8)),
  
  plot.title = element_text(
    size          = rel(1.80),
    family        = "title",
    face          = "bold",
    color         = title_col,
    margin        = margin(t = 5, b = 5)
  ),
  plot.subtitle = element_text(
    size          = rel(.92),
    family        = "subtitle",
    color         = title_col,
    lineheight    = 1.1,
    margin        = margin(t = 5, b = 5)
  ),
  plot.caption = element_markdown(
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

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-03-31
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# P annotater     0.2.3    2024-01-26 [?] CRAN (R 4.3.3)
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
# P generics      0.1.3    2022-07-05 [?] CRAN (R 4.3.0)
# P ggfittext     0.10.2   2024-02-01 [?] CRAN (R 4.3.3)
# P ggplot2     * 3.5.0    2024-02-23 [?] CRAN (R 4.3.2)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.3.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.3.1)
# P glue          1.7.0    2024-01-09 [?] CRAN (R 4.3.2)
# P graphics    * 4.3.3    2024-02-29 [?] local
# P grDevices   * 4.3.3    2024-02-29 [?] local
# P grid          4.3.3    2024-02-29 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.3.0)
# P gtable        0.3.4    2023-08-21 [?] CRAN (R 4.3.1)
# P here          1.0.1    2020-12-13 [?] CRAN (R 4.3.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.3.0)
# P htmltools     0.5.7    2023-11-03 [?] RSPM (R 4.3.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite      1.8.8    2023-12-04 [?] RSPM (R 4.3.0)
# P knitr         1.45     2023-10-30 [?] RSPM (R 4.3.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate   * 1.9.3    2023-09-27 [?] RSPM (R 4.3.0)
# P magick        2.8.3    2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.3.0)
# P markdown      1.12     2023-12-06 [?] CRAN (R 4.3.2)
# P methods     * 4.3.3    2024-02-29 [?] local
# P munsell       0.5.0    2018-06-12 [?] CRAN (R 4.3.0)
# P pacman        0.5.1    2019-03-11 [?] CRAN (R 4.3.0)
# P parallel      4.3.3    2024-02-29 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.3.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.3.1)
# P R.cache       0.16.0   2022-07-21 [?] CRAN (R 4.3.2)
# P R.methodsS3   1.8.2    2022-06-13 [?] CRAN (R 4.3.1)
# P R.oo          1.26.0   2024-01-24 [?] CRAN (R 4.3.2)
# P R.utils       2.12.3   2023-11-18 [?] CRAN (R 4.3.2)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.3.0)
# P ragg          1.2.7    2023-12-11 [?] RSPM (R 4.3.0)
# P Rcpp          1.0.11   2023-07-06 [?] CRAN (R 4.3.1)
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
# P stats       * 4.3.3    2024-02-29 [?] local
# P stringi       1.8.3    2023-12-11 [?] RSPM (R 4.3.0)
# P stringr     * 1.5.1    2023-11-14 [?] RSPM (R 4.3.0)
# P styler        1.10.2   2023-08-29 [?] CRAN (R 4.3.2)
# P svglite       2.1.1    2023-01-10 [?] CRAN (R 4.3.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.3.3)
# P systemfonts   1.0.5    2023-10-09 [?] RSPM (R 4.3.0)
# P textshaping   0.3.6    2021-10-13 [?] CRAN (R 4.3.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.3.0)
# P tidyr       * 1.3.1    2024-01-24 [?] CRAN (R 4.3.2)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.3.0)
# P timechange    0.2.0    2023-01-11 [?] CRAN (R 4.3.0)
# P tools         4.3.3    2024-02-29 [?] local
# P treemapify  * 2.5.6    2023-09-30 [?] CRAN (R 4.3.3)
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.3.0)
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
# ────────────────────────────────────────────────────────────────────────────────────────
# >