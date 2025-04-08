
## Challenge: #30DayChartChallenge 2025 day 14
## Topic:     Relationships | kinship
## Author:    Steven Ponce
## Date:      2024-04-14

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
  paletteer       # Comprehensive Collection of Color Palettes
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
# Get the lineup data
lineups_2man <- nba_leaguedashlineups(
  season = "2023-24",
  measure_type = "Advanced",
  group_quantity = 2, # 2-player combinations
  season_type = "Regular Season"
)

lineups_df <- lineups_2man$Lineups


## 3. EXAMINING THE DATA ----
glimpse(lineups_df)
skim(lineups_df)


## 4. TIDYDATA ----

lineups_clean <- lineups_df |>
  select(
    player_combo = GROUP_NAME,
    min = MIN,
    net_rating = NET_RATING,
    off_rating = OFF_RATING, 
    def_rating = DEF_RATING,
    gp = GP,
    w = W,
    l = L,
    win_pct = W_PCT
  ) |>
  mutate(
    across(c(min, net_rating, off_rating, def_rating, gp, w, l, win_pct), as.numeric),
    win_loss_diff = w - l,
    combo_effectiveness = (win_pct * 100) * (net_rating / 20) # Scale 
  ) |>
  # Filter for meaningful playing time
  filter(min >= 300) |>
  # Group player combinations based on their effectiveness
  mutate(
    playstyle_group = case_when(
      off_rating > median(off_rating) & def_rating < median(def_rating) ~ "Offensive-Minded",
      off_rating < median(off_rating) & def_rating > median(def_rating) ~ "Defensive-Minded",
      off_rating > median(off_rating) & def_rating > median(def_rating) ~ "Two-Way Elite",
      TRUE ~ "Developing Chemistry"
    )
  )

# Top player combinations by effectiveness
top_combos <- lineups_clean |>
  group_by(playstyle_group) |>
  arrange(desc(combo_effectiveness)) |>
  slice_max(order_by = combo_effectiveness, n = 8) |>
  ungroup()

# Facet order (levels)
playstyle_order <- top_combos |>
  group_by(playstyle_group) |>
  summarize(avg_win = mean(win_pct, na.rm = TRUE)) |>
  arrange(desc(avg_win)) |>
  pull(playstyle_group)

# Housekeeping
rm(lineups_2man)


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = paletteer:::paletteer_d(
    "ggsci::default_aaas",
    type = 'discrete', 
    n = 4)
)

### |-  titles and caption ----
# text
title_text    <- str_wrap("Win Percentage by Player Combinations and Playstyle in 2023-2024 Season",
                          width = 60) 
subtitle_text <- str_wrap("Top-performing duos organized by their playing chemistry, with team affiliations", 
                          width = 100)

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 14,
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
    axis.text.y = element_text(color = colors$text, size = rel(0.75)),

    # Grid elements
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Strip
    strip.text = element_text(family = fonts$text, color = colors$text, face = "bold", size = rel(0.92)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot(top_combos,
       aes(
         x = win_pct * 100, 
         y = fct_reorder(player_combo, win_pct))
) +
  # Geom
  geom_segment(aes(
    x = 0,
    xend = win_pct * 100,
    y = fct_reorder(player_combo, win_pct),
    yend = fct_reorder(player_combo, win_pct),
    color = playstyle_group
  ), 
  linewidth = 1, 
  alpha = 0.8
  ) +
  geom_point(aes(
    color = playstyle_group), 
    size = 3.5
  ) +
  geom_text(aes(
    label = sprintf("%.0f%%", win_pct * 100)), 
    hjust = -0.5, size = 3
  ) +
  # Scales
  scale_x_continuous(    
    limits = c(0, 100)
  ) +
  scale_y_discrete() +
  scale_color_manual(values = colors$palette) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Win Rate (%)",
    y = NULL,
    color = "Playstyle",
  ) +
  # Facets 
  facet_wrap(~ factor(
    playstyle_group, levels = playstyle_order), 
    scales = "free_y", ncol = 1
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.6),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.9),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.2,
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
# date     2025-03-26
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# !  package      * version  date (UTC) lib source
# V  base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P  base64enc      0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P  camcorder      0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.3)
# P  codetools      0.2-20   2024-03-31 [?] CRAN (R 4.4.0)
# colorspace     2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P  commonmark     1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P  compiler       4.4.0    2024-04-24 [?] local
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
# P  labeling       0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P  lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P  listenv        0.9.1    2024-01-29 [?] CRAN (R 4.4.3)
# P  lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P  magick         2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P  magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P  markdown       1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P  methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P  pacman       * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# paletteer    * 1.6.0    2024-01-21 [1] CRAN (R 4.4.3)
# P  parallel       4.4.0    2024-04-24 [?] local
# P  parallelly     1.42.0   2025-01-30 [?] CRAN (R 4.4.3)
# pillar         1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P  pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# prismatic      1.1.2    2024-04-10 [1] CRAN (R 4.4.3)
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
# P  utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
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