
## Challenge: #30DayChartChallenge 2025 day 24
## Topic:     Timeseries | world health organization (WHO)
## Author:    Steven Ponce
## Date:      2024-04-24

## Data:      The Global Health Observatory
##            Data /GHO /Indicators
##            Diabetes prevalence, ID 3356
##            https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-diabetes-age-standardized

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
  countrycode,    # Convert Country Names and Country Codes
  ggrepel         # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
)
  
### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
diabetes_data_raw <- read_csv('2025/data/WHO_prevalence_of_diabetes_3356.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(diabetes_data_raw)
skim(diabetes_data_raw)


## 4. TIDYDATA ----

diabetes_processed <- diabetes_data_raw |>  
  filter(
    indicator == "Prevalence of diabetes, age-standardized",
    dim2 == "18+  years"
  ) |>
  select(location, period, dim1, prevalence = fact_value_numeric) |>
  mutate(
    year = period,
    region = case_when(
      location %in% c("Global", "World") ~ "Global",
      TRUE ~ countrycode(location, "country.name", "continent")
    )
  )

gender_gap_data <- diabetes_processed |>
  filter(dim1 %in% c("Male", "Female")) |>
  pivot_wider(names_from = dim1, values_from = prevalence) |>
  filter(!is.na(Male) & !is.na(Female)) |>
  mutate(relative_difference = (Male - Female) / Female * 100) |>
  filter(!is.na(region)) |>
  group_by(region, year) |>
  summarize(
    avg_relative_difference = mean(relative_difference, na.rm = TRUE), 
    .groups = "drop"
    ) |>
  group_by(region) |>
  mutate(
    last_value = if_else(year == max(year), avg_relative_difference, NA_real_),
    highlight = case_when(
      region %in% c("Asia", "Europe", "Africa") ~ "highlight",
      TRUE ~ "base",
    )
  ) |>
  ungroup()

label_colors <- c("Europe" = "#D62828", "Asia" = "#0077B6", "Africa" = "#2A9D8F")

gender_gap_labels <- gender_gap_data |> 
  filter(!is.na(last_value), region %in% names(label_colors)) |>
  mutate(label_color = label_colors[region])


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c("Europe" = "#D62828", "Asia" = "#0077B6", "Africa" = "#2A9D8F")
  )

### |-  titles and caption ----
# text
title_text    <- str_wrap("Gender Differences in Diabetes Rates Vary Sharply by Region",
                          width = 60) 

subtitle_text <- str_glue("Values represent the **percentage difference** between **men's** and **women's** <br>
                          diabetes prevalence (age-standardized adults 18+).<br><br>
                          Positive values mean higher rates for men; negative means higher for women.")

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 24,
  source_text =  "World Health Organization (WHO) Global Health Observatory, 2024" 
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
    axis.title = element_text(color = colors$text, size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    
    axis.line.x = element_line(color = "gray50", linewidth = .2),
    
    # Grid elements
    panel.grid.minor = element_line(color = "gray65", linewidth = 0.05),
    panel.grid.major = element_line(color = "gray65", linewidth = 0.05),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

# Plot
ggplot(gender_gap_data, aes(x = year, y = avg_relative_difference, group = region)) +
  
  # Geoms
  geom_line(
    data = gender_gap_data |> filter(region %in% c("Americas", "Oceania")),
    color = "gray80", linewidth = 1.2, lineend = "round"
  ) +
  geom_line(data = gender_gap_data |> filter(region == "Asia"), color = colors$palette[2], linewidth = 1.4) +
  geom_line(data = gender_gap_data |> filter(region == "Europe"), color = colors$palette[1], linewidth = 1.4) +
  geom_line(data = gender_gap_data |> filter(region == "Africa"), color = colors$palette[3], linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text_repel(
    data = gender_gap_labels,
    aes(label = region, color = label_color),
    xlim = c(2021, 2026),
    size = 3.8, fontface = "bold", hjust = 0,
    direction = "y", nudge_x = 1.5, segment.alpha = 0.4,
    box.padding = 0.4, show.legend = FALSE
  ) +
  # Add gray labels for context lines
  geom_text_repel(
    data = gender_gap_data |>
      filter(!is.na(last_value), region %in% c("Americas", "Oceania")),
    aes(label = region),
    color = "gray50",
    xlim = c(2021, 2026),
    size = 3.5, fontface = "plain", hjust = 0,
    direction = "y", nudge_x = 1.5, segment.alpha = 0.3,
    box.padding = 0.4, show.legend = FALSE
  ) +
  # Annotate
  annotate(
    "text", x = 1990, y = 6, label = "Men have higher prevalence ↑", 
     hjust = 0, size = 3, color = "gray30", fontface = "bold"
    ) +
  annotate(
    "text", x = 1990, y = -6, label = "Women have higher prevalence ↓", 
    hjust = 0, size = 3, color = "gray30", fontface = "bold"
    ) +
  annotate(
    "text", x = 2016, y = 42, 
    label = "In Europe, men have nearly\n40% higher diabetes rates", 
    size = 3, color = "#D62828", fontface = "bold", hjust = 0
    ) +
  annotate(
    "text", x = 1997, y = -21, 
    label = "In Africa, women historically had\nup to 20% higher diabetes rates", 
    size = 3, color = "#2A9D8F", fontface = "bold", hjust = 0
    ) +
  # Scales
  scale_x_continuous(
    breaks = seq(1990, 2020, by = 5),
    limits = c(1990, 2026),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(
    breaks = seq(-20, 40, by = 10),
    limits = c(-22, 42),
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")
  ) +
  scale_color_identity() +  
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Relative Difference in\nDiabetes Prevalence (Men vs Women)"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.55),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 15)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
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

# ─ Session info ─────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-08
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# bit           4.6.0    2025-03-06 [1] CRAN (R 4.4.3)
# bit64         4.6.0-1  2025-01-16 [1] CRAN (R 4.4.3)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# P cli           3.6.4    2025-02-13 [?] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark    1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P countrycode * 1.6.1    2025-03-31 [?] CRAN (R 4.4.3)
# crayon        1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# curl          6.2.2    2025-03-24 [1] CRAN (R 4.4.3)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P dplyr       * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap       1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats     * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics      0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
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
# P parallel      4.4.0    2024-04-24 [?] local
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
# rstudioapi    0.17.1   2024-10-22 [1] CRAN (R 4.4.3)
# P rsvg          2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales      * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.7    2025-03-27 [1] CRAN (R 4.4.3)
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
# tzdb          0.5.0    2025-03-15 [1] CRAN (R 4.4.3)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr         3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] RSPM (R 4.4.0)
# xml2          1.3.8    2025-03-14 [1] CRAN (R 4.4.3)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────
# > 