
## Challenge: #30DayChartChallenge 2025 day 11
## Topic:     Distribution | stripes
## Author:    Steven Ponce
## Date:      2024-04-11

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
  hoopR           # Access Men's Basketball Play by Play Data
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

# Get player stats for 2024 NBA season
nba_players <- load_nba_player_box(seasons = 2024)


## 3. EXAMINING THE DATA ----
glimpse(nba_players )
skim(nba_players )


## 4. TIDYDATA ----

### |- Tidy ----

# Aggregate to get season stats per player
player_season_stats <- nba_players |>
  filter(!did_not_play) |>
  group_by(athlete_id, athlete_display_name, team_id, team_name) |>
  summarize(
    games_played = n(),
    total_minutes = sum(minutes, na.rm = TRUE),
    avg_minutes = total_minutes / games_played,
    total_assists = sum(assists, na.rm = TRUE),
    total_turnovers = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  )

# Filter out players with minimal playing time
filtered_players <- player_season_stats |>             
  filter(games_played >= 20, avg_minutes >= 10)

# Calculate Assist-to-Turnover Ratio (capped at 5)
filtered_players <- filtered_players |>
  mutate(AST_TO_Ratio = total_assists / pmax(total_turnovers, 1)) |>
  mutate(AST_TO_Ratio = pmin(AST_TO_Ratio, 5))

# Get team names to use for grouping
filtered_players$Team <- filtered_players$team_name

# Create team summary stats
team_stats <- filtered_players |>
  group_by(Team) |>
  summarize(
    median_ratio = median(AST_TO_Ratio, na.rm = TRUE),
    mean_ratio = mean(AST_TO_Ratio, na.rm = TRUE),
    min_ratio = min(AST_TO_Ratio, na.rm = TRUE),
    max_ratio = max(AST_TO_Ratio, na.rm = TRUE),
    q25 = quantile(AST_TO_Ratio, 0.25),
    q75 = quantile(AST_TO_Ratio, 0.75),
    players = n(),
    .groups = "drop"
  ) |>
  arrange(desc(median_ratio))

# Order teams by median ratio
team_order <- team_stats$Team
filtered_players$Team <- factor(filtered_players$Team, levels = team_order)

# Create background stripes df
background_stripes <- data.frame(
  Team = team_order[seq(1, length(team_order), by = 2)],
  xmin = rep(-Inf, length(team_order[seq(1, length(team_order), by = 2)])),
  xmax = rep(Inf, length(team_order[seq(1, length(team_order), by = 2)])),
  ymin = seq(1, length(team_order), by = 2) - 0.5,
  ymax = seq(1, length(team_order), by = 2) + 0.5
)

# Prepare data for dot plot
dotplot_data <- filtered_players |>
  mutate(ratio_rounded = round(AST_TO_Ratio * 2) / 2) |>
  group_by(Team, ratio_rounded) |>
  summarize(count = n(), .groups = "drop")

dotplot_data$Team <- factor(dotplot_data$Team, levels = team_order)

# Create median indicators df
median_indicators <- team_stats |>
  select(Team, median_ratio) |>
  mutate(Team = factor(Team, levels = team_order))


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
colors <- get_theme_colors(
  palette = c("#6A0DAD")  
  )

### |-  titles and caption ----
# text
title_text    <- str_wrap("NBA Teams: Distribution of Assist-to-Turnover Ratios",
                          width = 60) 
subtitle_text <- str_wrap("2024 NBA Season | Teams sorted by median ratio | Circle size represents number of players", 
                          width = 100)

# Create caption
caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 11,
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
    # Text styling 
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
    
    # Axis elements
    axis.title = element_text(color = colors$text, size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    axis.text.y = element_text(color = colors$text, size = rel(0.68)),
    
    axis.line.x = element_line(color = "gray50", linewidth = .2),

    # Grid elements
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
 
    # Legend elements
    legend.position = "bottom",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),

    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  final plot ----  
ggplot() +
  # Geoms
  geom_rect(
    data = background_stripes,                                                  # Stripes
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray80", alpha = 0.4
    ) +
  geom_vline(
    xintercept = seq(0, 5, by = 0.5), 
    color = "gray90", linewidth = 0.3
    ) +
  geom_vline(xintercept = median(team_stats$median_ratio), 
             linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(
    data = dotplot_data,
    aes(x = ratio_rounded, y = as.numeric(Team), 
    size = count),
    color = colors$palette,
    alpha = 0.6
    ) +
  # Annotations
  annotate(
    "richtext", x = 4.6, y = 3, label = "Top-ranked<br>teams have<br>more players<br>with higher<br>assist-to-<br>turnover ratios", 
    hjust = 0, vjust = 0.5, size = 3, fill = NA, label.color = NA
    ) +
  annotate(
    "richtext", x = 0, y = 1, 
    label = "Most teams have <span style='color:#2D708EFF;'>clusters</span> of<br>players with ratios between 1.0-2.5",
    hjust = 0, vjust = 0.5, size = 3, fill = NA, label.color = NA
    ) +
  annotate(
    "richtext", x = 4.2, y = length(team_order) - 3.5,
    label = "Larger circles indicate<br>more players with that<br>specific ratio value",
    hjust = 0, vjust = 0, size = 3, fill = NA, label.color = NA
    ) +
  annotate(
    "text", x = median(team_stats$median_ratio) + 0.05, y = length(team_order) + 1.1,
    label = "League median", hjust = 0, size = 3, lineheight = 0.9
    ) +
  # Scales
  scale_x_continuous(
    breaks = seq(0, 5, 0.5),
    limits = c(0, 5.2),
    expand = expansion(add = c(0.1, 0))
  ) +
  scale_y_continuous(
    breaks = 1:length(team_order), 
    labels = team_order,
    expand = expansion(add = c(0.8, 0.8))
    ) +
  scale_size_continuous(
    range = c(1, 9), 
    breaks = c(1, 3, 5, 7),
    name = "Number of Players"
    ) +
  # Labs
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x = "Assist-to-Turnover Ratio (rounded to nearest 0.5)",
       y = NULL
    ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(2),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(1),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.2,
      margin = margin(t = 5, b = 10)
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

# ─ Session info ─────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-24
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────
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
# P  lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P  listenv        0.9.1    2024-01-29 [?] CRAN (R 4.4.3)
# P  lubridate    * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P  magick         2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P  magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P  markdown       1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P  methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P  pacman       * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
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
# ────────────────────────────────────────────────────────────────────────
# > 
