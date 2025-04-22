
## Challenge: #30DayChartChallenge 2025 day 28
## Topic:     Uncertainties | inclusion
## Author:    Steven Ponce
## Date:      2024-04-28

## Data:      Unesco Institute for Statistics
##            Gender Equality in Education
##            World Inequality Database on Education
##            Indicator: Less than 4 years of schooling
##            https://uis.unesco.org/en/topic/gender-equality-education
##            https://www.education-inequalities.org/indicators/edu4#maxYear=2023&minYear=2013&ageGroup=%22edu4_2024%22   

## 1. LOAD PACKAGES & SETUP ----  
if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  janitor,        # Simple Tools for Examining and Cleaning Dirty Data
  skimr,          # Compact and Flexible Summaries of Data
  scales,         # Scale Functions for Visualization
  lubridate       # Make Dealing with Dates a Little Easier
  )
  
### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

unesco_education_raw <- read_csv('2025/data/1699460825-wide_2023_sept.csv') |> 
    clean_names()


## 3. EXAMINING THE DATA ----
glimpse(unesco_education_raw)
skim(unesco_education_raw)


## 4. TIDYDATA ----

# Tidy
education_clean <- unesco_education_raw |>
  filter(!is.na(edu4_2024_m)) |>   # Focus on the key metric
  mutate(edu4_2024_m = as.numeric(edu4_2024_m)) |>
  filter(!is.na(sex)) |>
  filter(year >= 2010) |>
  select(country, region_group, year, sex, edu4_2024_m, edu4_2024_no)

# Gender gaps (inclusion)
gender_gaps <- education_clean |>
  group_by(country, region_group, year) |>
  summarize(
    female_rate = mean(edu4_2024_m[sex == "Female"], na.rm = TRUE),
    male_rate = mean(edu4_2024_m[sex == "Male"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Calculate the gender gap (female - male)
  mutate(
    gender_gap = female_rate - male_rate,
    abs_gap = abs(gender_gap)
  ) |>
  filter(!is.na(gender_gap))

# Regional averages with uncertainty
region_gaps <- gender_gaps |>
  group_by(region_group) |>
  summarize(
    mean_gap = mean(gender_gap, na.rm = TRUE),
    sd_gap = sd(gender_gap, na.rm = TRUE),
    countries = n_distinct(country),
    # Calculate uncertainty
    lower_ci = mean_gap - 1.96 * sd_gap / sqrt(countries),
    upper_ci = mean_gap + 1.96 * sd_gap / sqrt(countries),
    # Calculate mean rates for context
    mean_female = mean(female_rate, na.rm = TRUE),
    mean_male = mean(male_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    disadvantaged_gender = ifelse(mean_gap > 0, "Female", "Male"),
    region_label = paste0(region_group, " (n=", countries, ")")
  ) |>
  arrange(desc(abs(mean_gap)))


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Female" = "#D81B60", "Male" = "#1E88E5"
    )
  )

### |-  titles and caption ----
# text
title_text    <- str_glue("Gender Gap in Educational Exclusion by Region")

subtitle_text <- str_glue("Difference in percentage with less than **4 years of education** (female - male), 2010 - 2021.",
                          "<br>Higher values indicate greater educational exclusion.",
                          "<br><br>Error bars show uncertainty (95% confidence intervals)",
                          "<br>n = number of countries reporting (higher values indicate more complete regional data)")

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 28,
  source_text =  "Unesco Institute for Statistics" 
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
    
    # Legend
    legend.position = "bottom",
    
    # Axis elements
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.title.y = element_text(color = colors$text, size = rel(0.8), 
                                hjust = 0.5, margin = margin(r = 10)),
    axis.title.x = element_text(color = colors$text, size = rel(0.8), 
                                hjust = 0.5, margin = margin(t = 10)),
    
    axis.line.x = element_line(color = "gray50", linewidth = .2),
    
    # Grid elements
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray65", linewidth = 0.05),
    panel.grid.major.x = element_line(color = "gray65", linewidth = 0.05),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme) 

# Plot
ggplot(region_gaps, aes(x = reorder(region_label, mean_gap), y = mean_gap)) +
  # Geoms
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "gray50"
    ) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci), 
    width = 0.3, color = "gray50"
    ) +
  geom_point(
    aes(color = disadvantaged_gender), 
    size = 3.5, alpha = 0.8
    ) +
  geom_text(
    aes(label = sprintf("%+.1f%%", 100 * mean_gap)),
    color = "black", hjust = 0.5, vjust = -1, size = 3.5
    ) +
  # Scales
  scale_color_manual(
    values = colors$palette,
    name = "More Excluded Gender",
    labels = c("Female", "Male")
    ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(-0.1, 0.13),
    name = "Gender Gap (percentage points)"
    ) +
  coord_flip() +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = ""
  ) +
  # Annotate
  annotate(
    "text", y = 0.11, x = 3, 
    label = str_wrap("Positive values: Females have less education (more excluded)", width = 25),
    hjust = 1, size = 3, color = "#D81B60"
    ) +
  annotate(
    "text", y = -0.025, x = 2, 
    label = str_wrap("Negative values: Males have less education (more excluded)",
                     width = 22),
    hjust = 1, size = 3, color = "#1E88E5"
    ) +
  annotate(
    "text", y = 0.005, x = 1.1, 
    label = "Gender parity line\n(equal educational exclusion)", 
    vjust = -0.85, hjust = 0, size = 3, color = "gray50" 
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.9),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.85),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 0.8,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.6),
      family = fonts$caption,
      color = colors$caption,
      lineheight = 0.65,
      hjust = 0,
      halign = 0,
      margin = margin(t = 10, b = 5)
    ),
  )
  

# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-12
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────
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
# P labeling      0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
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
# ───────────────────────────────────────────────────────────────────────
# > 
