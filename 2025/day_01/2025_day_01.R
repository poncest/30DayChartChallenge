
## Challenge: #30DayChartChallenge 2025 day 01
## Topic:     Comparison | fractions
## Author:    Steven Ponce
## Date:      2025-04-01

## Data:      TidyTuesday 2023 week 30 Scurvy 
## Link:      https://github.com/rfordatascience/tidytuesday/tree/e0cda77e7b4ca3f7e201f6fe23d9ead080a5a19c/data/2023/2023-07-25


## 1. LOAD PACKAGES & SETUP ----  
if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
  tidyverse, # Easily Install and Load the 'Tidyverse'
  ggtext,    # Improved Text Rendering Support for 'ggplot2'
  showtext,  # Using Fonts More Easily in R Graphs
  janitor,   # Simple Tools for Examining and Cleaning Dirty Data
  skimr,     # Compact and Flexible Summaries of Data
  scales,    # Scale Functions for Visualization
  lubridate  # Make Dealing with Dates a Little Easier
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
tt <- tidytuesdayR::tt_load(2023, week = 30) 

scurvy <- tt$scurvy |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(scurvy)
skim(scurvy)


## 4. TIDYDATA ----

### |- Tidy ----
# Define constants upfront
symptom_names <- c(
  gum_rot_d6 = "Gum Rot",
  skin_sores_d6 = "Skin Sores", 
  weakness_of_the_knees_d6 = "Knee Weakness",
  lassitude_d6 = "Lassitude"
)

treatment_names <- c(
  dilute_sulfuric_acid = "Dilute Sulfuric Acid",
  purgative_mixture = "Purgative Mixture",
  sea_water = "Sea Water"
)

# Treatment effectiveness order 
treatment_order <- c(
  "Citrus", "Cider", "Dilute Sulfuric Acid", 
  "Vinegar", "Sea Water", "Purgative Mixture"
)

# Process scurvy data 
complete_diverging_data <- scurvy |>
  # Convert Likert scales to ordered factors
  mutate(across(
    names(symptom_names),
    \(x) factor(
      case_when(
        str_detect(x, "0_none") ~ "None (0)",
        str_detect(x, "1_mild") ~ "Mild (1)",
        str_detect(x, "2_moderate") ~ "Moderate (2)",
        str_detect(x, "3_severe") ~ "Severe (3)"
      ),
      levels = c("None (0)", "Mild (1)", "Moderate (2)", "Severe (3)")
    )
  )) |>
  # Clean treatment names
  mutate(
    treatment_clean = case_when(
      treatment %in% names(treatment_names) ~ treatment_names[treatment],
      TRUE ~ str_to_title(treatment)
    )
  ) |>
  # Convert to long format
  pivot_longer(
    cols = names(symptom_names),
    names_to = "symptom",
    values_to = "severity"
  ) |>
  # Map symptom codes to readable names
  mutate(symptom = symptom_names[symptom]) |>
  # Calculate proportions
  group_by(treatment_clean, symptom) |>
  count(severity) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  # Create diverging data structure
  mutate(
    position = if_else(
      severity %in% c("None (0)", "Mild (1)"), 
      "Improved", 
      "Problem"
    ),
    plot_proportion = if_else(position == "Improved", -proportion, proportion),
    severity_ordered = factor(
      severity, 
      levels = c("None (0)", "Mild (1)", "Moderate (2)", "Severe (3)")
    )
  ) |>
  # Ensure all combinations exist (handle missing values)
  complete(
    treatment_clean, 
    symptom, 
    severity_ordered,
    fill = list(proportion = 0, plot_proportion = 0, n = 0)
  ) |>
  # Recreate position for new rows
  mutate(
    position = case_when(
      severity_ordered %in% c("None (0)", "Mild (1)") ~ "Improved",
      severity_ordered %in% c("Moderate (2)", "Severe (3)") ~ "Problem",
      TRUE ~ NA_character_
    )
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors <- get_theme_colors(palette = c(
  "None (0)" = "#1d4e89",       
  "Mild (1)" = "#4b86c5",      
  "Moderate (2)" = "#e8996f",  
  "Severe (3)" = "#d95d33"      
  ))  

### |-  titles and caption ----
# text
title_text    <- str_glue("Citrus: The Only Effective Treatment for Scurvy (1757)") 
subtitle_text <- str_glue("Fractions of patients showing improvement vs. continuing symptoms after treatment")

# Create caption
caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 01,
  source_text =  "{ medicaldata } R package" 
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
    axis.text.x = element_text(color = colors$text, size = rel(0.7)),
    axis.text.y = element_text(color = colors$text, size = rel(0.75), face = "bold"),
    
    # Grid elements
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Legend elements
    legend.position = "top",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot(
  complete_diverging_data |>
    filter(symptom == "Gum Rot") |>
    mutate(treatment_clean = factor(treatment_clean, levels = treatment_order))
) +
  # Geoms
  geom_col(
    aes(
      x = treatment_clean,
      y = plot_proportion,
      fill = severity_ordered
    ),
    position = "stack"
  ) +
  geom_text(
    aes(
      x = treatment_clean,
      y = plot_proportion,
      label = ifelse(proportion >= 0.05,
        scales::percent(abs(proportion),
          accuracy = 1
        ), ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  # Scales
  scale_y_continuous(
    labels = function(x) paste0(abs(x) * 100, "%"),
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25),
    minor_breaks = NULL
  ) +
  scale_fill_manual(
    values = colors$palette
  ) +
  coord_flip(clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = NULL,
    y = NULL,
    fill = "Severity Level",
    caption = caption_text
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.75),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_markdown(
      size = rel(.65),
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

# ─ Session info ──────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-19
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] RSPM (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] RSPM (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P colorspace     2.1-0    2023-01-23 [?] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] RSPM (R 4.4.0)
# curl           5.2.1    2024-03-01 [1] CRAN (R 4.3.3)
# P datasets     * 4.4.0    2024-04-24 [?] local
# P digest         0.6.35   2024-03-11 [?] RSPM (R 4.4.0)
# P dplyr        * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# P fansi          1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# P farver         2.1.1    2022-07-06 [?] CRAN (R 4.4.0)
# P fastmap        1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# P forcats      * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics       0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggplot2      * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gh             1.4.1    2024-03-28 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# P gitcreds       0.1.2    2022-09-08 [?] CRAN (R 4.4.0)
# P glue           1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.3.3)
# P here         * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P httr2          1.0.1    2024-04-01 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# P jsonlite       1.8.8    2023-12-04 [?] RSPM (R 4.4.0)
# P knitr          1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick         2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman       * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# P pillar         1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# P rappdirs       0.3.3    2021-01-31 [?] RSPM (R 4.4.0)
# Rcpp           1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr        * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv           1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang          1.1.3    2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot      2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] RSPM (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales       * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# P stringi        1.8.4    2024-05-06 [?] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts    1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping    0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidyr        * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidytuesdayR   1.1.2    2024-09-09 [?] RSPM (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] RSPM (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] RSPM (R 4.4.0)
# P withr          3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun           0.43     2024-03-25 [?] RSPM (R 4.4.0)
# P xml2           1.3.6    2023-12-04 [?] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────
# > 