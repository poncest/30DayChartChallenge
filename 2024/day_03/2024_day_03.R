
## Challenge: #30DayChartChallenge 2024 day 03
## Topic:     Comparisons | makeover
## Author:    Steven Ponce
## Date:      2024-04-03

## 0. DATA SOURCE ----

#' Dataset 1:
#' World Freedom Index
#' Link: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md
#'  
#' Dataset 2:
#' Freedom in the World
#' Link: https://freedomhouse.org/report/freedom-world
#' 
#' Data: All Data, FIW 2013-2024 (Excel Download)
#' Link: https://freedomhouse.org/sites/default/files/2024-02/All_data_FIW_2013-2024.xlsx


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
tidyverse, # Easily Install and Load the 'Tidyverse'
ggtext,    # Improved Text Rendering Support for 'ggplot2'
showtext,  # Using Fonts More Easily in R Graphs
camcorder, # Record Your Plot History
scales,    # Scale Functions for Visualization
janitor,   # Simple Tools for Examining and Cleaning Dirty Data
skimr,     # Compact and Flexible Summaries of Data
readxl,    # Read Excel Files
ggbump     # Bump Chart and Sigmoid Curves
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
# freedom_1995_2020

tt <- tidytuesdayR::tt_load(x = 2022, week = 8) 

freedom_1995_2020_raw <- tt$freedom |> clean_names() |> glimpse()

# |- freedom_2013_2024 

freedom_2013_2024_raw <- read_xlsx(
  path = "2024/data/All_data_FIW_2013-2024.xlsx",
  sheet = "FIW13-24",
  skip = 1,
  col_names = TRUE,
  trim_ws = TRUE,
  .name_repair = "unique"
) |>
  clean_names() |>
  glimpse()



## 3. EXAMINING THE DATA ----
glimpse(freedom_1995_2020_raw)
colnames(freedom_1995_2020_raw)
skim(freedom_1995_2020_raw)

glimpse(freedom_2013_2024_raw)
colnames(freedom_2013_2024_raw)
skim(freedom_2013_2024_raw)


## 4. TIDYDATA ----

# freedom_1995_2020 
freedom_1995_2020_df <- freedom_1995_2020_raw |>
  # Rename columns
  rename(
    cl_rating = cl,
    pr_rating = pr,
    region = region_name
  ) |> 
  # Remove columns
  select(-c(region_code, is_ldc)) |> 
  # 5-year column
  mutate(quinquennial = (year %/% 5) * 5) |>                                    
  group_by(country) |>
  # Calculate mean rating column
  mutate(mean_rating = rowMeans(cbind(cl_rating, pr_rating), na.rm = TRUE)) |> 
  ungroup() |> 
  # 5-year mean
  summarise(
    mean_rating = mean(mean_rating),
    .by = c('country', 'quinquennial')
  ) |>
  # Derive combined status
  mutate(
    combined_status = case_when(
      mean_rating   <= 2.5    ~ 'Free',
      mean_rating   > 2.5 &
        mean_rating < 5.1     ~ 'Partially Free',
      TRUE                    ~ 'Not Free'
    )) |>
  ungroup()


# freedom_2021_2024 
freedom_2021_2024_df <- freedom_2013_2024_raw |>
  select(country_territory:cl_rating) |>
  # Convert columns to factors
  mutate(across(country_territory:c_t, factor)) |>
  # 5-year  column
  mutate(quinquennial = (edition %/% 5) * 5) |>
  group_by(country_territory) |>
  # Calculate mean rating column
  mutate(mean_rating = rowMeans(cbind(cl_rating, pr_rating), na.rm = TRUE)) |>
  ungroup() |>
  # 5-year mean
  summarise(
    mean_rating = mean(mean_rating),
    .by = c("country_territory", "quinquennial")
  ) |>
  # Derive combined status
  mutate(
    combined_status = case_when(
      mean_rating <= 2.5  ~ "Free",
      mean_rating > 2.5 &
        mean_rating < 5.1 ~ "Partially Free",
      TRUE                ~ "Not Free"
    )) |>
  ungroup() |>
  # Filter years
  filter(quinquennial >= 2020) |>
  # Rename columns
  rename(country = country_territory)


# freedom_combined 
freedom_combined_df <- bind_rows(
  list(
    freedom_1995_2020_df,
    freedom_2021_2024_df
    ),
  .id = "id") |>
  arrange(country, quinquennial)


# World df
world_df <- freedom_combined_df |>          
  summarise(
    mean_rating = mean(mean_rating, na.rm = TRUE),
    combined_status = case_when(
      mean_rating   <= 2.5   ~ 'Free',
      mean_rating   > 2.5 & 
        mean_rating < 5.1    ~ 'Partially Free',
      TRUE                   ~ 'Not Free'
    ), 
    .by = quinquennial 
  ) |> 
  mutate(country = 'World')


# freedom_world df 
freedom_world_df <- bind_rows(
  freedom_combined_df, 
  world_df
  ) 


# check country names
freedom_world_df$country |> unique() |> sort()

# south american countries
south_america_countries <-  c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                              "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                              "Uruguay", "Venezuela", "World")


# South america df 
americas_df <- freedom_world_df |>
  # Format country names (south america)
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)"    ~ "Bolivia",
    country == "Venezuela (Bolivarian Republic of)"  ~ "Venezuela",
    TRUE ~ as.character(country)
  )) |> 
  # Select south america countries
  filter(country %in% south_america_countries)


# Country labels 
country_labels <- americas_df |>
  filter(
    country %in% south_america_countries,
    quinquennial == 2020
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#fbf7f0', 0.65)   
title_col    <- "gray10"              
subtitle_col <- "gray20"   
caption_col  <- "gray20"  
text_col     <- "gray20" 
col_palette  <- c("#BBBBBB", "#BB5566", "#004488") 

### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 03 } &bull; Source: freedomhouse.org<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text <- str_glue("Freedom in South America, 1995-2024")

venezuela  <- str_glue("<span style='color:{ col_palette[2] }'>**Venezuela's**</span>")

others     <- str_glue("<span style='color:{ colorspace::darken(col_palette[1], 0.2) }'>**other South American**</span>")

subtitle_text <- str_glue("{ venezuela } political rights and civil liberties are deteriorating compared<br>
                          to { others } countries.<br><br>
                          <span style='font-size:10pt'>Median combined rating</span>")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) {gh} poncest &bull; #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Russo One", regular.wt = 400, family = "title")       
font_add_google("Abel", family = "subtitle")  
font_add_google("Abel", regular.wt = 400, family = "text")        
font_add_google("Roboto Condensed", family = "caption")
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
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = "text", face = "bold"),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = "text", face = "bold"),
  axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "black", linewidth = .2),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = "gray30"),
)

# labels for y-axis 
label_text <- c(
  "", "", "", "", "Free", "",
  "", "", "Partially Free", "", "",
  "", "", "Not Free", ""
  )

### |-   final plot ----
americas_df |>
  ggplot(aes(
    x = quinquennial, y = mean_rating, color = country,
    group = country, label = country
  )) +
  
  # Geoms
  # Other south america countries
  geom_bump(smooth = 15, size = .2, color = col_palette[1]) +
  geom_point(size = 2, color = col_palette[1]) +
  
  # Venezuela
  geom_bump(
    data = americas_df |> filter(country == "Venezuela"),
    smooth = 15, size = .5, color = col_palette[2]
  ) +
  geom_point(
    data = americas_df |> filter(country == "Venezuela"),
    size = 3, color = col_palette[2]
  ) +
  geom_text(
    data = country_labels |> filter(country == "Venezuela"),
    hjust = 0, nudge_x = 0.5, check_overlap = TRUE,
    fontface = "bold", color = col_palette[2]
  ) +
  
  # World
  geom_bump(
    data = americas_df |> filter(country == "World"),
    smooth = 15, size = .5, color = col_palette[3]
  ) +
  geom_point(
    data = americas_df |> filter(country == "World"),
    size = 3, color = col_palette[3]
  ) +
  geom_text(
    data = country_labels |> filter(country == "World"),
    hjust = 0, nudge_x = .5, check_overlap = TRUE,
    fontface = "bold", color = col_palette[3]
  ) +
  
  # Segment on the y-axis
  annotate(geom = "segment", x = 1994, xend = 1994, y = 1, yend = 2.5, size = .2, color = "gray60") +
  annotate(geom = "segment", x = 1994, xend = 1994, y = 3, yend = 5, size = .2, color = "gray60") +
  annotate(geom = "segment", x = 1994, xend = 1994, y = 5.5, yend = 7, size = .2, color = "gray60") +
  
  # Scales
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.16))) +
  scale_y_reverse(
    breaks = seq(0, 7, by = .5),
    labels = label_text
  ) +
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = "off") +
  
  # Labs
  labs(
    x = element_blank(),
    y = element_blank(),
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  # Theme
  theme(
    plot.title    = element_text(
      size        = rel(1.7),
      family      = "title",
      face        = "bold",
      color       = title_col,
      margin      = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size        = rel(1.1),
      family      = "subtitle",
      color       = title_col,
      lineheight  = 1.1,
      margin      = margin(t = 5, b = 10)
    ),
    plot.caption  = element_markdown(
      size        = rel(.65),
      family      = "caption",
      color       = caption_col,
      lineheight  = 0.65,
      hjust       = 0.5,
      halign      = 0.5,
      margin      = margin(t = -10, b = 5)
    ),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────────────────────
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
# ─ Packages ──────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.3    2024-02-29 [2] local
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.3.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.3.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.3.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.3.0)
# P cellranger     1.1.0    2016-07-27 [?] CRAN (R 4.3.0)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace     2.1-0    2023-01-23 [?] CRAN (R 4.3.0)
# P commonmark     1.9.0    2023-03-17 [?] CRAN (R 4.3.0)
# P compiler       4.3.3    2024-02-29 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.3.0)
# P curl           5.2.0    2023-12-08 [?] RSPM (R 4.3.0)
# P datasets     * 4.3.3    2024-02-29 [?] local
# P digest         0.6.33   2023-07-07 [?] CRAN (R 4.3.1)
# P dplyr        * 1.1.4    2023-11-17 [?] RSPM (R 4.3.0)
# P fansi          1.0.6    2023-12-08 [?] RSPM (R 4.3.0)
# P farver         2.1.1    2022-07-06 [?] CRAN (R 4.3.0)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.3.0)
# P forcats      * 1.0.0    2023-01-29 [?] CRAN (R 4.3.0)
# P fs             1.6.3    2023-07-20 [?] CRAN (R 4.3.1)
# P generics       0.1.3    2022-07-05 [?] CRAN (R 4.3.0)
# P ggbump       * 0.1.0    2020-04-24 [?] CRAN (R 4.3.3)
# P ggplot2      * 3.5.0    2024-02-23 [?] CRAN (R 4.3.2)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.3.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.3.1)
# P glue           1.7.0    2024-01-09 [?] CRAN (R 4.3.2)
# P graphics     * 4.3.3    2024-02-29 [?] local
# P grDevices    * 4.3.3    2024-02-29 [?] local
# P grid           4.3.3    2024-02-29 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.3.0)
# P gtable         0.3.4    2023-08-21 [?] CRAN (R 4.3.1)
# P here           1.0.1    2020-12-13 [?] CRAN (R 4.3.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.3.0)
# P htmltools      0.5.7    2023-11-03 [?] RSPM (R 4.3.0)
# P httr           1.4.7    2023-08-15 [?] CRAN (R 4.3.1)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite       1.8.8    2023-12-04 [?] RSPM (R 4.3.0)
# P knitr          1.45     2023-10-30 [?] RSPM (R 4.3.0)
# P labeling       0.4.3    2023-08-29 [?] CRAN (R 4.3.1)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.3.0)
# P magick         2.8.3    2024-02-18 [?] CRAN (R 4.3.3)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.3.0)
# P markdown       1.12     2023-12-06 [?] CRAN (R 4.3.2)
# P methods      * 4.3.3    2024-02-29 [?] local
# P munsell        0.5.0    2018-06-12 [?] CRAN (R 4.3.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.3.0)
# P parallel       4.3.3    2024-02-29 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.3.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.3.1)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.3.0)
# P ragg           1.2.7    2023-12-11 [?] RSPM (R 4.3.0)
# P Rcpp           1.0.11   2023-07-06 [?] CRAN (R 4.3.1)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.3.2)
# P readxl       * 1.4.3    2023-07-06 [?] CRAN (R 4.3.1)
# renv           1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr           1.1.6    2023-01-26 [?] CRAN (R 4.3.0)
# P rlang          1.1.3    2024-01-10 [?] CRAN (R 4.3.2)
# P rprojroot      2.0.4    2023-11-05 [?] RSPM (R 4.3.0)
# P rstudioapi     0.15.0   2023-07-07 [?] CRAN (R 4.3.1)
# P rsvg           2.6.0    2023-10-08 [?] RSPM (R 4.3.0)
# P rvest          1.0.4    2024-02-12 [?] CRAN (R 4.3.2)
# P scales       * 1.3.0    2023-11-28 [?] CRAN (R 4.3.2)
# P selectr        0.4-2    2019-11-20 [?] CRAN (R 4.3.0)
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
# P tidytuesdayR   1.0.3    2023-12-13 [?] CRAN (R 4.3.2)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.3.0)
# P timechange     0.2.0    2023-01-11 [?] CRAN (R 4.3.0)
# P tools          4.3.3    2024-02-29 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.3.0)
# P usethis        2.2.3    2024-02-19 [?] CRAN (R 4.3.3)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.3.2)
# P utils        * 4.3.3    2024-02-29 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.3.2)
# P vroom          1.6.5    2023-12-05 [?] RSPM (R 4.3.0)
# P withr          3.0.0    2024-01-16 [?] CRAN (R 4.3.2)
# P xfun           0.41     2023-11-01 [?] CRAN (R 4.3.2)
# P xml2           1.3.6    2023-12-04 [?] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────
# >