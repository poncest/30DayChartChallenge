
## Challenge: #30DayChartChallenge 2024 day 21
## Topic:     Timeseries | green energy
## Author:    Steven Ponce
## Date:      2024-04-21

## Data:      TABLE 10.1 Renewable energy production and consumption by source (Trillion Btu)
##            U.S. Energy Information Administration (EIA)
## Link:      https://www.eia.gov/totalenergy/data/monthly/pdf/sec10_3.pdf
##            https://www.eia.gov/renewable/data.php

## Citation:
#' U.S. Energy Information Administration. Renewable energy production and consumption by source. 
#' [ Monthly Energy Review (March 2024)]. 
#' Accessed [2024-04-19].


## 1. LOAD PACKAGES & SETUP ---- 
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  skimr,       # Compact and Flexible Summaries of Data
  scales,      # Scale Functions for Visualization
  lubridate,   # Make Dealing with Dates a Little Easier
  readxl,      # Read Excel Files
  MetBrewer,   # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors,  # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  glue         # Interpreted String Literals
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
us_energy_1949_2023 <- read_xlsx(here::here("2024/data/Table_10.1_Renewable_Energy_Production_and_Consumption_by_Source.xlsx"),
  sheet = "Annual Data", skip = 10, trim_ws = TRUE
) |>
  clean_names() |>
  glimpse() |>
  na.omit(annual_total)


## 3. EXAMINING THE DATA ----
glimpse(us_energy_1949_2023)
skim(us_energy_1949_2023)
colnames(us_energy_1949_2023)


## 4. TIDYDATA ----

### |- tidy data ---
consumption <- us_energy_1949_2023 |>
  rename(year = annual_total) |>
  select(year, contains("_consumption")) |>
  pivot_longer(
    cols = -year,
    names_to = "consumption_type",
    values_to = "consumption_trillion_btu"
  ) |>
  filter(consumption_trillion_btu != "Not Available") |>
  mutate(
    consumption_trillion_btu = as.numeric(consumption_trillion_btu),
    consumption_type = str_remove(consumption_type, pattern = "_consumption"),
    consumption_type = str_remove(consumption_type, pattern = "_energy"),
    consumption_type = case_when(
      consumption_type == "hydroelectric_power" ~ "hydroelectric",
      TRUE ~ factor(consumption_type)
    ),
    consumption_type = str_to_title(consumption_type)
  )

# Review consumption energy
consumption$consumption_type |> unique() |> sort()

# Select specific energy consumption type
selected_energy <- c("Hydroelectric", "Geothermal", "Solar", "Wind", "Wood", "Waste", "Biofuels")

# Create a dataframe for labels with nudge adjustments
label_data <- consumption |>
  filter(
    consumption_type %in% selected_energy,
    year == 2023
  ) |>
  group_by(consumption_type) |>
  summarize(
    consumption_trillion_btu = last(consumption_trillion_btu),
    year = last(year)
  ) |>
  mutate(
    nudge_x = ifelse(consumption_type == "Solar", 0.5, ifelse(consumption_type == "Hydroelectric", -0.1, 1)),
    nudge_y = ifelse(consumption_type == "Solar", -100, ifelse(consumption_type == "Hydroelectric", 99, 0))
  ) |>
  ungroup()

### |- plot data ---
plot_data <- consumption |> 
  filter(consumption_type %in% selected_energy)


# 5. VISUALIZATION ---- 
### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('brown', 0.95) 
title_col    <- "gray10"             
subtitle_col <- "gray10"     
caption_col  <- "gray20"   
text_col     <- "gray20"     
col_palette  <- MoMAColors::moma.colors("Abbott", n = 7, type = "discrete") 


### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 21 } &bull; Source: U.S. Energy Information Administration<br>") 
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- str_glue("U.S. Energy Consumption by Type from 1949 to 2023") 

subtitle_text <- str_glue("Comparing various sources of energy consumption over time")

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
plot_data |> 
  ggplot(aes(x = year, y = consumption_trillion_btu, 
             group = consumption_type, color = consumption_type)) + 
  
  # Geoms
  geom_smooth(linewidth = 0.5) +
  geom_point(size = 0.3) +
  
  geom_text(
    data = label_data,
    aes(x = year, y = consumption_trillion_btu, label = consumption_type),
    nudge_x  = label_data$nudge_x,
    nudge_y  = label_data$nudge_y,
    hjust    = -0.05,
    vjust    = 0.5,
    fontface = 'bold',
    family   = 'text',
    size     = 2.5
  ) +
  
  # Scales
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_y_continuous(
    breaks = seq(0, 3000, by = 1000),
    limits = c(-200, 3000),
    labels = comma_format()
  ) +
  coord_cartesian(clip = 'off') + 
  scale_color_manual(values = col_palette) +
  guides(colour = guide_legend(title = "Energy Type")) +
  
  # Labs
  labs(x = "Year", 
       y = "Consumption (Trillion BTU)",
       title    = title_text,
       subtitle = subtitle_text,
       caption  = caption_text
  ) +
  
  # Theme
  theme(
    plot.title      = element_text(
      size          = rel(1.4),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(1), 
      family        = 'subtitle',
      color         = subtitle_col,
      lineheight    = 1.1, 
      margin        = margin(t = 0, b = 5)
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

# ─ Session info ──────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-04-19
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# base        * 4.3.3      2024-02-29 [2] local
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.3.0)
# P camcorder     0.1.0      2022-10-03 [?] CRAN (R 4.3.0)
# P cellranger    1.1.0      2016-07-27 [?] CRAN (R 4.3.0)
# cli           3.6.2      2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace    2.1-0      2023-01-23 [?] CRAN (R 4.3.0)
# P commonmark    1.9.0      2023-03-17 [?] CRAN (R 4.3.0)
# P compiler      4.3.3      2024-02-29 [?] local
# cowplot       1.1.3      2024-01-22 [1] CRAN (R 4.3.3)
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
# P lattice       0.22-5     2023-10-24 [?] CRAN (R 4.3.3)
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.3.2)
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
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.3.0)
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.3.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.3.1)
# P R6            2.5.1      2021-08-19 [?] CRAN (R 4.3.0)
# P ragg          1.2.7      2023-12-11 [?] RSPM (R 4.3.0)
# Rcpp          1.0.12     2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5      2024-01-10 [?] CRAN (R 4.3.2)
# P readxl      * 1.4.3      2023-07-06 [?] CRAN (R 4.3.1)
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
# P withr         3.0.0      2024-01-16 [?] CRAN (R 4.3.2)
# P xfun          0.41       2023-11-01 [?] CRAN (R 4.3.2)
# P xml2          1.3.6      2023-12-04 [?] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/30DayChartChallenge/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/07866f9d
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────
# > 