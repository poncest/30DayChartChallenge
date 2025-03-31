
## Challenge: #30DayChartChallenge 2025 day 05
## Topic:     Comparison | ranking
## Author:    Steven Ponce
## Date:      2024-04-05

## Data:      World Bank data in R
##            indicator =  GDP (current US$) ("NY.GDP.MKTP.CD")
##            https://github.com/vincentarelbundock/WDI
##            https://databank.worldbank.org/source/world-development-indicators#


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
  ggbump,         # Bump Chart and Sigmoid Curves
  ggrepel,        # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  WDI             # World Development Indicators and Other World Bank Data
)
  
### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
# Get GDP data (current US$) for top economies
gdp_data <- WDI(
  indicator = c("gdp" = "NY.GDP.MKTP.CD"),                # GDP in current US$
  country = c("US", "CN", "JP", "DE", "GB", "IN", "FR", "IT", 
              "BR", "CA", "KR", "RU", "AU", "ES", "MX"),
  start = 1960,
  end = 2020
)


## 3. EXAMINING THE DATA ----
glimpse(gdp_data)
skim(gdp_data)


## 4. TIDYDATA ----

### |- Tidy ----
# Filter parameters
countries_to_keep <- c("US", "CN", "JP", "DE", "GB", "IN", "FR", "IT", "KR", "BR")
years_to_keep <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)

# Filter the data
gdp_filtered <- gdp_data |>
  filter(iso2c %in% countries_to_keep) |>
  filter(year %in% years_to_keep) |>
  filter(!is.na(gdp))

# Calculate rankings for each year
gdp_ranked <- gdp_filtered |>
  group_by(year) |>
  mutate(rank = rank(-gdp, ties.method = "first")) |>
  ungroup()

# Classify countries into highlighted vs. background
gdp_ranked <- gdp_ranked |>
  mutate(
    # Only 3 highlighted countries (US, China, Japan)
    highlight_group = case_when(
      iso2c == "US" ~ "US",
      iso2c == "CN" ~ "China",
      iso2c == "JP" ~ "Japan",
      TRUE ~ "Other"
    ),
    # Create a size variable for lines and points
    line_size = case_when(
      iso2c == "CN" ~ 2.5,              # China gets thickest line
      iso2c %in% c("US", "JP") ~ 1.5,   # US and Japan medium
      TRUE ~ 0.8                        # Others thin
    ),
    point_size = case_when(
      iso2c == "CN" ~ 5,                # China gets largest points
      iso2c %in% c("US", "JP") ~ 3.5,   # US and Japan medium
      TRUE ~ 2                          # Others small
    ),
    # Alpha for background countries
    line_alpha = case_when(
      iso2c %in% c("US", "CN", "JP") ~ 1,
      TRUE ~ 0.5
    ),
    # Country labels
    country_label = case_when(
      iso2c == "US" ~ "United\nStates",
      iso2c == "CN" ~ "China",
      iso2c == "JP" ~ "Japan",
      iso2c == "DE" ~ "Germany",
      iso2c == "GB" ~ "United\nKingdom",
      iso2c == "IN" ~ "India",
      iso2c == "FR" ~ "France",
      iso2c == "IT" ~ "Italy",
      iso2c == "KR" ~ "South\nKorea",
      iso2c == "BR" ~ "Brazil",
      TRUE ~ country
    ),
    # Format GDP in trillions 
    gdp_trillion = round(gdp / 1e12, 2),
    # GDP label
    gdp_label = paste0("$", gdp_trillion, "T")
  )

# Left label dataset 
left_labels <- gdp_ranked |> 
  filter(year == 1960) |>
  mutate(
    # horizontal adjustments
    hjust = 1,
    nudge_x = -1.5,
    nudge_y = 0
   
  )

# Right label dataset 
right_labels <- gdp_ranked |> 
  filter(year == 2020) |>
  mutate(
    label_line = str_glue("{ country_label } ({ gdp_label })"),
    # horizontal adjustments
    hjust = 0,
    nudge_x = 1.5,
    nudge_y = 0
  ) 


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors <- get_theme_colors(palette = c(
  "US" = "#0066CC",      
  "China" = "#CC0000",   
  "Japan" = "#FF9900",   
  "Other" = "#999999"   
  )
)

### |-  titles and caption ----
# text
title_text    <- str_glue("China's Rise: GDP Ranking Changes (1960-2020)") 
subtitle_text <- str_glue("From 5th to 2nd: The WTO Effect on China's Economic Ascen<br>
                          Values shown in trillion USD (2020)")

# Create caption
caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 05,
  source_text =  "{ WDI } World Bank data in R" 
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
    # Text styling 
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
    
    # Axis elements
    axis.title.y = element_text(color = colors$text, size = rel(0.8),
                              hjust = 1, vjust = 0.95, angle = 0),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = colors$text, size = rel(0.7)),
    axis.line.x = element_line(color = "gray50", linewidth = .2),

    # Grid elements
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
 
    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot(gdp_ranked, aes(x = year, y = rank, group = country_label)) +
  # Add background shading
  annotate("rect",
    xmin = 1995, xmax = 2005, ymin = 0, ymax = 11,
    fill = "#FF8B8B", alpha = 0.15
  ) +
  # Geoms
  geom_bump(aes(color = highlight_group, size = line_size, alpha = line_alpha), smooth = 8) +
  geom_point(aes(color = highlight_group, size = point_size, alpha = line_alpha)) +
  geom_text(                                                                    # left side labels
    data = left_labels,
    aes(label = country_label, color = highlight_group, y = rank + nudge_y),
    hjust = 1,
    nudge_x = -2,
    lineheight = 0.9,
    size = 3.2,
  ) +
  geom_text(                                                                    # right side labels
    data = right_labels,
    aes(label = label_line, color = highlight_group, y = rank), 
    hjust = 0,
    nudge_x = 2,
    size = 2.8,
    fontface = "bold"
  ) +
  # Annotate
  annotate("text",
    x = 2001, y = 6.7, label = "China joins WTO", color = "gray20",
    size = 3.2, fontface = "italic"
  ) +
  annotate("segment",
    x = 2000, xend = 2000, y = 6.5, yend = 6.1,
    arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.5, color = "gray20"
  ) +
  # Scales
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(
    breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
    limits = c(1955, 2029)
  ) +
  scale_color_manual(values = colors$palette) +
  scale_size_identity() +
  scale_alpha_identity() +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "Year",
    y = "Rank",
    caption = caption_text
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
    plot.subtitle = element_markdown(
      size = rel(1),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.2,
      margin = margin(t = 5, b = 5)
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

# ─ Session info ───────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-22
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli           3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark    1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# P curl          6.2.1    2025-02-19 [?] RSPM (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P dplyr       * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap       1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats     * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics      0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggbump      * 0.1.0    2020-04-24 [?] CRAN (R 4.4.0)
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
# P here        * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite      1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P knitr         1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick        2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6            2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P ragg          1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp          1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr       * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv          1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
# P rprojroot     2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] RSPM (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales      * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# P stringi       1.8.4    2024-05-06 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts   1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping   0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidyr       * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse   * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] RSPM (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P WDI         * 2.7.8    2022-09-25 [?] CRAN (R 4.4.3)
# P withr         3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] RSPM (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────
# > 