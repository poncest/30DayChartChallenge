
## Challenge:     #30DayChartChallenge 2026 — Day 17
## Prompt:        Relationships | Remake
## Topic:         The Holiday-Volatility Paradox: Remaking TidyTuesday 2024 Week 52
    
## Author:        Steven Ponce
## Date:          2026-04-17
    
## NOTE:          This script uses custom helper functions for theming and formatting.
##                Sources: R/utils/fonts.R, R/utils/social_icons.R, R/themes/base_theme.R
    
## Data:          Global Holidays and Travel
##                https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/
##
## Original Chart https://github.com/poncest/tidytuesday/tree/main/2024/Week_52



## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, patchwork,
  janitor, scales, glue, ggrepel
)

### |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))
source(here::here("R/utils/snap.R"))


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2024, week = 52)

global_holidays_raw <- tt$global_holidays |> clean_names()
monthly_passengers_raw  <- tt$monthly_passengers |> clean_names()
rm(tt)


## 3. EXAMINING THE DATA ----
glimpse(global_holidays_raw)
glimpse(monthly_passengers_raw)


## 4. TIDY DATA ----

monthly_passengers_clean <- monthly_passengers_raw |>
  mutate(
    date  = ymd(paste(year, month, "01", sep = "-")),
    total_passengers = coalesce(total, total_os)
  )

monthly_holidays_clean <- global_holidays_raw |>
  mutate(
    year  = year(date),
    month = month(date)
  ) |>
  group_by(iso3, year, month) |>
  summarise(
    holiday_count = n(),
    public_holidays = sum(type == "Public holiday"),
    .groups = "drop"
  )

combined_data <- monthly_passengers_clean |>
  left_join(monthly_holidays_clean, by = c("iso3", "year", "month"))

# Housekeeping
rm(global_holidays_raw, monthly_passengers_raw, monthly_holidays_clean, monthly_passengers_clean)
gc()

# Volatility summary by country ----
volatility_df <- combined_data |>
  group_by(iso3) |>
  summarise(
    mean_traffic = mean(total_passengers, na.rm = TRUE),
    sd_traffic = sd(total_passengers, na.rm = TRUE),
    cv = sd_traffic / mean_traffic,
    avg_holidays = mean(holiday_count, na.rm = TRUE),
    total_observations = n(),
    traffic_size = sum(total_passengers, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(
    complete.cases(cv, avg_holidays),
    total_observations >= 12,
    cv >= 0,
    cv <= quantile(cv, 0.95, na.rm = TRUE)
  ) |>
  mutate(
    size_category = cut(
      traffic_size,
      breaks = quantile(traffic_size, probs = seq(0, 1, 0.25), na.rm = TRUE),
      labels = c("Small", "Medium", "Large", "Very Large"),
      include.lowest = TRUE
    )
  )

# Per-facet correlation values ----
cor_labels <- volatility_df |>
  group_by(size_category) |>
  summarise(
    r = cor(avg_holidays, cv, use = "complete.obs"),
    n_country = n(),
    .groups = "drop"
  ) |>
  mutate(
    # Direction drives color encoding in strip + summary panel
    direction = if_else(r > 0, "positive", "negative"),
    label = glue("r = {sprintf('%+.2f', r)}\nn = {n_country}")
  )

# Identify notable outliers per facet (top/bottom CV, only if genuinely extreme) ----
outlier_threshold <- 0.15 

outliers_df <- volatility_df |>
  left_join(
    volatility_df |>
      group_by(size_category) |>
      summarise(med_cv = median(cv), .groups = "drop"),
    by = "size_category"
  ) |>
  group_by(size_category) |>
  filter(
    abs(cv - med_cv) >= outlier_threshold,
    cv == max(cv) | cv == min(cv)
  ) |>
  ungroup()


# 5. VISUALIZATION ---- 

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    positive  = "#C0392B",   
    negative  = "#2980B9",  
    neutral   = "#7F8C8D",  
    point     = "#34495E",  
    highlight = "#E67E22",   
    bg        = "#F8F9FA"
  )
)

### |- titles and caption ----
title_text <- str_glue(
  "The Holiday-Volatility Paradox"
)

subtitle_text <- str_glue(
  "In small markets, more holidays correlate with **higher** traffic volatility (r = +0.48).<br>
    In larger markets, the relationship **flips** — more holidays predict **lower** volatility.<br>
    <span style='color:#C0392B;'>● Positive correlation</span> &nbsp;&nbsp;
    <span style='color:#2980B9;'>● Negative correlation</span> &nbsp;&nbsp;
    *Remake of TidyTuesday 2024 · Week 52*"
)

caption_text <- create_dcc_caption(
  dcc_year    = 2026,
  dcc_day     = 17,
  source_text = "Global Holidays and Travel · TidyTuesday 2024 Week 52"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # strip labels
    strip.text = element_text(
      family = fonts$text, size = 11, face = "bold",
      margin = margin(b = 6)
    ),
    # axes
    axis.title = element_text(family = fonts$text, size = 9, color = "gray50"),
    axis.text = element_text(family = fonts$text, size = 8, color = "gray40"),
    axis.ticks = element_blank(),
    # grid — horizontal only, very faint
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # panel spacing — more breathing room
    panel.spacing.x = unit(2.0, "lines"),
    panel.spacing.y = unit(1.8, "lines"),
    # plot margins
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20)
  )
)

theme_set(weekly_theme)

### |- plot ---

### |- helper: strip color per facet ----
strip_labels <- cor_labels |>
  mutate(
    strip_color = if_else(direction == "positive",
                          colors$palette$positive,
                          colors$palette$negative
    ),
    strip_text = glue("{size_category} Market"),
    x_pos = -Inf,
    y_pos = Inf
  )

### |- main scatter plot (p_main) ----
p_main <- ggplot(volatility_df, aes(x = avg_holidays, y = cv)) +
  
  # Geoms
  geom_hline(
    yintercept = median(volatility_df$cv),
    linetype = "dashed",
    color = colors$palette$neutral,
    linewidth  = 0.4,
    alpha = 0.6
  ) +
  geom_point(
    color = colors$palette$point,
    size = 2.8,
    alpha = 0.75,
    shape = 16
  ) +
  geom_smooth(
    aes(color = size_category),
    method = "lm",
    formula = y ~ x,
    linewidth = 1,
    se = TRUE,
    alpha = 0.12
  ) +
  geom_text_repel(
    data = outliers_df,
    aes(label = iso3),
    size = 2.8,
    color = colors$palette$highlight,
    fontface = "bold",
    max.overlaps = 3,
    box.padding = 0.5,
    segment.color = colors$palette$neutral,
    segment.alpha = 0.5,
    segment.size = 0.3,
    seed = 123
  ) +
  geom_text(
    data = volatility_df |> filter(size_category == "Small"),
    x = Inf, y = median(volatility_df$cv) + 0.025,
    label = "industry median",
    size = 2.5, color = colors$palette$neutral,
    hjust = 1.05, vjust = -0.3, fontface = "italic"
  ) +
  geom_text(
    data = cor_labels,
    aes(
      x = Inf,
      y = 0.62,
      label = label,
      color = direction
    ),
    size = 3,
    hjust = 1.1,
    vjust = 1,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = strip_labels,
    aes(
      x = x_pos,
      y = y_pos,
      label = strip_text,
      fill = direction
    ),
    hjust = -0.05,
    vjust = 1.3,
    size = 3.2,
    fontface = "bold",
    color = "white",
    label.size = 0,
    label.padding = unit(0.25, "lines"),
    inherit.aes = FALSE
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Small" = colors$palette$positive,
      "Medium" = colors$palette$negative,
      "Large" = colors$palette$negative,
      "Very Large" = colors$palette$negative
    )
  ) +
  scale_color_manual(
    values = c(
      "positive" = colors$palette$positive,
      "negative" = colors$palette$negative,
      "Small" = colors$palette$positive,
      "Medium" = colors$palette$negative,
      "Large" = colors$palette$negative,
      "Very Large" = colors$palette$negative
    )
  ) +
  scale_fill_manual(
    values = c(
      "positive" = colors$palette$positive,
      "negative" = colors$palette$negative
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.75, by = 0.25),
    limits = c(0, 0.75), # CV cannot be negative — floor at 0
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(2, 8, by = 2),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  
  # Facet ---
  facet_wrap(
    ~size_category,
    nrow   = 2,
    scales = "free_x"
  ) +
  
  # Labs
  labs(
    x = "Average Number of Holidays per Month",
    y = "Coefficient of Variation in Traffic"
  ) +
  
  # Theme
  theme(
    plot.title = element_text(
      size = rel(2),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight  = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.95),
      family = fonts$text,
      color = colors$subtitle,
      lineheight = 1.4,
      margin = margin(t = 5, b = 15)
    ),
    strip.text = element_blank()
  )

### |- summary panel: r values across market sizes (p_summary) ----

# Standalone panel 
p_summary <- cor_labels |>
  mutate(
    size_category = factor(size_category, levels = c("Small", "Medium", "Large", "Very Large")),
    x_num  = as.numeric(size_category),
    vjust_val = if_else(r > 0, -1.2, 2.0)
  ) |>
  ggplot(aes(x = x_num, y = r, color = direction)) +
  
  # Annotate
  annotate(
    "rect",
    xmin = 0.5, xmax = 4.5,
    ymin = 0,   ymax = 0.55,
    fill  = colors$palette$positive,
    alpha = 0.04
  ) +
  
  # Geoms
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = colors$palette$neutral,
    linewidth  = 0.5
  ) +
  geom_line(color = "#BBBBBB", linewidth = 0.8) +
  geom_point(size = 6) +
  geom_text(
    aes(label = sprintf("%+.2f", r), vjust = vjust_val),
    size = 3.2,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2.5, y = 0.62,
    label = "← positive          negative →",
    size = 2.6,
    color = colors$palette$neutral,
    family = 'sans',
    fontface = "italic",
    hjust = 0.5
  ) +
  
  # Scales
  scale_color_manual(
    values = c(
      "positive" = colors$palette$positive,
      "negative" = colors$palette$negative
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Small", "Medium", "Large", "Very\nLarge"),
    expand = expansion(add = 0.5) 
  ) +
  scale_y_continuous(
    limits = c(-0.45, 0.70),
    breaks = seq(-0.4, 0.6, by = 0.2),
    labels = function(x) sprintf("%+.1f", x)
  ) +
  # Labs
  labs(
    x = "Market Size",
    y = "Correlation (r)",
    title = "The Sign Flip"
  ) +
  # Theme
  theme(
    plot.margin = margin(t = 10, r = 18, b = 10, l = 18),
    plot.title = element_text(
      size = rel(1.1),
      family  = fonts$text,
      face = "bold",
      color = colors$title,
      margin = margin(b = 10)
    ),
    axis.text.x = element_text(
      size = rel(0.78), color = "#555555",
      margin = margin(t = 6)
    ),
    axis.text.y = element_text(size = rel(0.78), color = "#555555"),
    axis.title.x = element_text(
      size = rel(0.85), color = "#444444",
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size = rel(0.85), color = "#444444",
      margin = margin(r = 8)
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank()
  )

# Combined plots
p_right <- p_summary / plot_spacer() +
  plot_layout(heights = c(1, 0.20)) 

# Left = evidence (2x2 scatter), Right = conclusion 
p_final <- 
  (p_main | p_right) +
  plot_layout(widths = c(2.4, 1.1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(2),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.95),
        family = 'sans',
        color = colors$subtitle,
        lineheight = 1.4,
        margin = margin(t = 5, b = 15)
      ),
      plot.caption = element_markdown(
        family = fonts$caption,
        size = rel(0.65),
        color = colors$caption,
        linewidth = 1.3,
        hjust = 0,
        margin = margin(t = 15)
      ),
      plot.margin = margin(15, 15, 10, 15)
    )
  )


### |- preview ----
snap(p_final)


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #TidyTuesday projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #TidyTuesday attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across 50+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/30DayChartChallenge/tree/main/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/30DayChartChallenge/
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-26
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpUVMnUV/file1f406d177ecd". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ───────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gh             1.4.1    2024-03-28 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# gitcreds       0.1.2    2022-09-08 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# httr2          1.2.2    2025-12-08 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# Matrix         1.5-4.1  2023-05-18 [2] CRAN (R 4.3.1)
# P methods      * 4.3.1    2023-06-16 [2] local
# mgcv           1.8-42   2023-03-02 [2] CRAN (R 4.3.1)
# nlme           3.1-162  2023-01-31 [2] CRAN (R 4.3.1)
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# P parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P splines        4.3.1    2023-06-16 [2] local
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# svglite        2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping    1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidytuesdayR   1.2.1    2025-04-29 [1] CRAN (R 4.3.1)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools          4.3.1    2023-06-16 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# P utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# vroom          1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────