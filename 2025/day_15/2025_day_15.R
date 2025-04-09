
## Challenge: #30DayChartChallenge 2025 day 15
## Topic:     Relationships | complicated
## Author:    Steven Ponce
## Date:      2024-04-15

## Data:      { hoopR } an R package for working with men’s basketball data.
##            live play by play and box score data from ESPN
##            https://hoopr.sportsdataverse.org/
##            https://github.com/sportsdataverse/hoopR


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
  hoopR,          # Access Men's Basketball Play by Play Data
  paletteer,      # Comprehensive Collection of Color Palettes
  ggrepel         # Automatically Position Non-Overlapping Text Labels with ggplot2
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
  
# Get player stats for 2023-2024 season
nba_players <- load_nba_player_box(seasons = 2024)


## 3. EXAMINING THE DATA ----
glimpse(nba_players)
skim(nba_players)


## 4. TIDYDATA ----

# Calculate aggregate stats (assists and turnovers)
player_stats <- nba_players |>  
  group_by(athlete_display_name, athlete_position_name) |>
  summarize(
    games = n(),
    minutes = sum(minutes, na.rm = TRUE),
    assists = sum(assists, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    ast_per_36 = (assists / minutes) * 36,
    tov_per_36 = (turnovers / minutes) * 36,
    ast_to_ratio = assists / pmax(turnovers, 0.5),  
    .groups = "drop"
  ) |>
  filter(
    minutes >= 500,
    !is.na(ast_per_36),
    !is.na(tov_per_36),
    is.finite(ast_per_36),  
    is.finite(tov_per_36)
  ) |> 
  mutate(
    high_ast = ast_per_36 > median(ast_per_36),
    high_tov = tov_per_36 > median(tov_per_36),
    playmaker_type = case_when(
      high_ast & !high_tov ~ "Efficient Playmakers",
      high_ast & high_tov ~ "High-Risk Playmakers",
      !high_ast & !high_tov ~ "Low-Usage Ball Handlers",
      !high_ast & high_tov ~ "Turnover-Prone"
    )
  )

# Correlation
correlation <- cor(
  player_stats$ast_per_36, 
  player_stats$tov_per_36, 
  use = "complete.obs"
  )

# Label df
elite_players <- player_stats |>
  filter(
    minutes > 1500 & (
      # Elite point guards and playmakers
      ast_per_36 > quantile(player_stats$ast_per_36, 0.95) |
        # Players with unusual assist/turnover combos
        (ast_per_36 > 7 & tov_per_36 < 1.5) |
        tov_per_36 > quantile(player_stats$tov_per_36, 0.95) |
        # Super-efficient playmakers
        (ast_per_36 > 6 & ast_to_ratio > 4) |
        # manually selected a few stars
        athlete_display_name %in% c(
          "LeBron James", "Nikola Jokic", "Chris Paul", 
          "Victor Wembanyama", "Tyrese Haliburton", "Joel Embiid"
        )
    )
  ) |>
  # Top N 
  slice_max(order_by = minutes, n = 15)


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "Efficient Playmakers" = "#009E73", 
    "High-Risk Playmakers" = "#171738",  
    "Low-Usage Ball Handlers" = "#593C8F", 
    "Turnover-Prone" = "#DB5461" 
  )
)

### |-  titles and caption ----
# text
title_text    <- str_wrap("The Complicated Relationship Between Assists and Turnovers",
                          width = 60) 

subtitle_text <- str_glue(
  "NBA 2023-24 Season: Correlation =  { round(correlation, 2) }<br>
  Playmakers who create more assists also tend to commit more turnovers"
)

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 15,
  source_text =  "ESPN via { hoopR } package" 
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

    # Axis elements
    axis.title = element_text(color = colors$text, size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),

    # Grid elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),

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
ggplot(player_stats, 
       aes(x = ast_per_36, y = tov_per_36, color = playmaker_type)
) +
  # Geoms
  geom_point(
    aes(size = minutes), 
    alpha = 0.35
    ) +
  geom_hline(
    yintercept = median(player_stats$tov_per_36), 
    linetype = "dotted", 
    color = "gray50",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = median(player_stats$ast_per_36), 
    linetype = "dotted", 
    color = "gray50",
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x,
    se = FALSE, 
    color = "gray50", 
    linetype = "dashed",
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_text_repel(
    data = elite_players,
    aes(label = athlete_display_name),
    size = 3, 
    max.overlaps = 10,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.color = "gray30",
    min.segment.length = 0.2,
    force = 5, 
    seed = 123
  ) +
  # Scales
  scale_x_continuous(
    breaks = seq(0, 12, by = 2),
    limits = c(0, max(player_stats$ast_per_36) * 1.05)
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 1),
    limits = c(0, max(player_stats$tov_per_36) * 1.05)
  ) +
  scale_size_continuous(
    name = "Total Minutes Played", 
    range = c(1.5, 7),
    breaks = c(1000, 2000, 3000)
  ) +
  scale_color_manual(
    values = colors$palette
    ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Assists Per 36 Minutes",
    y = "Turnovers Per 36 Minutes",
  ) +
  # Annotate
  annotate(
    "text", 
    x = 11.5, 
    y = 1.8,
    label = "Median assists", 
    fontface = "italic", 
    size = 2.8, 
    color = "gray40",
    hjust = 0.5
  ) +
  annotate(
    "text", 
    x = 3, 
    y = 4.2,
    label = "Median turnovers", 
    fontface = "italic", 
    size = 2.8, 
    color = "gray40",
    hjust = 0,
    angle = 90
  ) +
  annotate(
    "text", 
    x = 11, 
    y = 3.65,
    label = "2:1 Assist-to-Turnover Ratio", 
    fontface = "italic", 
    size = 2.5, 
    color = "gray30",
    angle = 27 
  ) +
  annotate(
    "text", 
    x = 7.5,
    y = 0.2,
    label = "Efficient Playmakers", 
    fontface = "bold", 
    size = 3.5, 
    color = colors$palette[1]
  ) +
  annotate(
    "text", 
    x = 10,
    y = 4.05,
    label = "High-Risk Playmakers", 
    fontface = "bold", 
    size = 3.5, 
    color = colors$palette[2]
  ) +
  annotate(
    "text", 
    x = 1,
    y = 0.2,
    label = "Low-Usage Ball Handlers", 
    fontface = "bold", 
    size = 3.5, 
    color = colors$palette[3]
  ) +
  annotate(
    "text", 
    x = 1,
    y = 4.05,
    label = "Turnover-Prone", 
    fontface = "bold", 
    size = 3.5, 
    color = colors$palette[4]
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.5,
      margin = margin(t = 5, b = 15)
    ),
    plot.caption = element_markdown(
      size = rel(0.6),
      family = fonts$caption,
      color = colors$caption,
      lineheight = 0.65,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 5, b = 5)
    ),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ───────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-27
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# !  package      * version  date (UTC) lib source
# P  annotater      0.2.3    2024-01-26 [?] RSPM (R 4.4.0)
# V  base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P  base64enc      0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P  camcorder      0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P  codetools      0.2-20   2024-03-31 [?] CRAN (R 4.4.0)
# colorspace     2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P  compiler       4.4.0    2024-04-24 [?] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# P  curl           6.2.1    2025-02-19 [?] RSPM (R 4.4.0)
# P  data.table     1.15.4   2024-03-30 [?] RSPM (R 4.4.0)
# P  datasets     * 4.4.0    2024-04-24 [?] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P  dplyr        * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P  fastmap        1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P  forcats      * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P  furrr          0.3.1    2022-08-15 [?] CRAN (R 4.4.3)
# P  future         1.34.0   2024-07-29 [?] CRAN (R 4.4.3)
# P  generics       0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P  ggplot2      * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P  ggrepel      * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P  ggtext       * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P  gifski         1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# P  globals        0.16.3   2024-03-08 [?] CRAN (R 4.4.0)
# glue           1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# P  graphics     * 4.4.0    2024-04-24 [?] local
# P  grDevices    * 4.4.0    2024-04-24 [?] local
# P  grid           4.4.0    2024-04-24 [?] local
# P  gridtext       0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# P  here         * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P  hms            1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P  hoopR        * 2.1.0    2023-11-25 [?] CRAN (R 4.4.3)
# P  htmltools      0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P  httr           1.4.7    2023-08-15 [?] RSPM (R 4.4.0)
# P  janitor      * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite       1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P  knitr          1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P  lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.3)
# P  lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P  listenv        0.9.1    2024-01-29 [?] CRAN (R 4.4.3)
# P  lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P  magick         2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P  magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P  Matrix         1.7-3    2025-03-11 [?] CRAN (R 4.4.3)
# P  methods      * 4.4.0    2024-04-24 [?] local
# P  mgcv           1.9-1    2023-12-21 [?] CRAN (R 4.4.3)
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P  nlme           3.1-167  2025-01-27 [?] CRAN (R 4.4.3)
# P  pacman         0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# paletteer    * 1.6.0    2024-01-21 [1] CRAN (R 4.4.3)
# P  parallel       4.4.0    2024-04-24 [?] local
# P  parallelly     1.42.0   2025-01-30 [?] CRAN (R 4.4.3)
# pillar         1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P  pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P  progressr      0.15.1   2024-11-22 [?] CRAN (R 4.4.3)
# P  purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P  ragg           1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp           1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# PD RcppParallel   5.1.10   2025-01-24 [?] CRAN (R 4.4.3)
# P  readr        * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# P  rematch2       2.1.2    2020-05-01 [?] RSPM (R 4.4.0)
# renv           1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P  repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
# P  rprojroot      2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# P  rstudioapi     0.16.0   2024-03-24 [?] RSPM (R 4.4.0)
# P  rsvg           2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P  rvest          1.0.4    2024-02-12 [?] RSPM (R 4.4.0)
# P  scales       * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P  sessioninfo    1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P  showtext     * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P  showtextdb   * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P  skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P  snakecase      0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P  splines        4.4.0    2024-04-24 [?] local
# P  stats        * 4.4.0    2024-04-24 [?] local
# P  stringi        1.8.4    2024-05-06 [?] CRAN (R 4.4.0)
# P  stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P  svglite        2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P  sysfonts     * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P  systemfonts    1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P  textshaping    0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P  tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P  tidyr        * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P  tidyverse    * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P  timechange     0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P  tools          4.4.0    2024-04-24 [?] local
# P  tzdb           0.4.0    2023-05-12 [?] RSPM (R 4.4.0)
# P  utils        * 4.4.0    2024-04-24 [?] local
# P  vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P  withr          3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P  xfun           0.43     2024-03-25 [?] RSPM (R 4.4.0)
# P  xml2           1.3.6    2023-12-04 [?] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# D ── DLL MD5 mismatch, broken installation.
# 
# ──────────────────────────────────────────────────────────────────────────────
# > 