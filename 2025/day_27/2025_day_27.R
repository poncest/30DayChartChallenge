
## Challenge: #30DayChartChallenge 2025 day 27
## Topic:     Uncertainties | noise
## Author:    Steven Ponce
## Date:      2024-04-27

## Data:      quantmod
##            Yahoo Finance via { quantmod }
##            https://www.quantmod.com/           

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
  ggrepel,        # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  quantmod        # Quantitative Financial Modelling Framework
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

# Get S&P 500 data (just last 2 years to keep it faster)
getSymbols("^GSPC", from = "2022-01-01")

# Convert to data frame and prepare data
sp500_df <- GSPC |>  
  as.data.frame() |>
  rownames_to_column(var = "date") |>
  mutate(date = as.Date(date)) |>
  select(date, GSPC.Adjusted) |>
  rename(price = GSPC.Adjusted)  


## 3. EXAMINING THE DATA ----
glimpse(sp500_df)
skim(sp500_df)


## 4. TIDYDATA ----

# Calculate log returns
sp500_df <- sp500_df |>
  arrange(date) |>
  mutate(log_return = c(NA, diff(log(price))))

# Decompose the signal into trend and noise components
# Use loess smoothing to extract the trend
loess_fit <- loess(price ~ as.numeric(date), data = sp500_df, span = 0.15)
sp500_df$trend <- predict(loess_fit)
sp500_df$noise <- sp500_df$price - sp500_df$trend

# Calculate noise metrics
noise_sd <- sd(sp500_df$noise, na.rm = TRUE)
noise_range <- max(sp500_df$noise, na.rm = TRUE) - min(sp500_df$noise, na.rm = TRUE)
signal_to_noise <- sd(sp500_df$trend, na.rm = TRUE) / noise_sd

# Calculate historical volatility (standard deviation of returns)
vol <- sd(sp500_df$log_return, na.rm = TRUE)

# Create forecast data
last_date <- max(sp500_df$date)
last_price <- sp500_df |> filter(date == last_date) |> pull(price)
forecast_days <- 60

# Create date sequence for weekdays only
all_dates <- seq.Date(from = last_date + days(1), 
                      by = "day", 
                      length.out = forecast_days * 1.5) # Add buffer for weekends
forecast_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
forecast_dates <- head(forecast_dates, forecast_days)

# Simple random walk simulation for forecasting
set.seed(123)
n_simulations <- 1000
simulations <- matrix(nrow = length(forecast_dates), ncol = n_simulations)

for (i in 1:n_simulations) {
  # Start with last known price
  price <- last_price
  # Generate random returns based on historical volatility
  random_returns <- rnorm(length(forecast_dates), mean = 0, sd = vol)
  
  for (j in 1:length(forecast_dates)) {
    # Apply random return
    price <- price * exp(random_returns[j])
    simulations[j, i] <- price
  }
}

# Calculate percentiles for confidence intervals
forecast_df <- data.frame(
  date = forecast_dates,
  mean = rowMeans(simulations),
  lower_95 = apply(simulations, 1, quantile, probs = 0.025),
  lower_80 = apply(simulations, 1, quantile, probs = 0.1),
  lower_50 = apply(simulations, 1, quantile, probs = 0.25),
  upper_50 = apply(simulations, 1, quantile, probs = 0.75),
  upper_80 = apply(simulations, 1, quantile, probs = 0.9),
  upper_95 = apply(simulations, 1, quantile, probs = 0.975)
)


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "#8C1C13", "#BF4342", "#E7D7C1", "#1A4D2E"
    )
  )

### |-  titles and caption ----
# text
title_text    <- str_wrap("S&P 500 Price Uncertainty and Noise",
                          width = 70) 

subtitle_text <- str_wrap("Visualizing price trend, random variations, and forecast uncertainty",
                          width = 80)

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 27,
  source_text =  "Yahoo Finance via { quantmod }" 
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
ggplot() +
  # Geom
  geom_line( # tend line
    data = sp500_df, aes(x = date, y = trend), 
    color = colors$palette[4], linewidth = 0.9
    ) +
  geom_line( # noise around trend
    data = sp500_df, aes(x = date, y = price), 
    color = colors$palette[4], linewidth = 0.4, alpha = 0.6
    ) +
  geom_ribbon(
    data = forecast_df,  # 95% prediction interval
    aes(x = date, ymin = lower_95, ymax = upper_95), 
    fill = colors$palette[1], alpha = 0.15
    ) +
  geom_ribbon(
    data = forecast_df,  # 80% prediction interval
    aes(x = date, ymin = lower_80, ymax = upper_80), 
    fill = colors$palette[2], alpha = 0.2
    ) +
  geom_ribbon(
    data = forecast_df,  # 50% prediction interval
    aes(x = date, ymin = lower_50, ymax = upper_50), 
    fill = colors$palette[3], alpha = 0.3,
    ) +
  geom_line(
    data = forecast_df,  # Mean forecast
    aes(x = date, y = mean), 
    color = colors$palette[1], linetype = "dashed", linewidth = 0.5
    ) +
   geom_vline(
    xintercept = as.numeric(last_date), 
    linetype = "dashed", color = "gray50"
    ) +
  # Annotation
  annotate(
    "text", x = as.Date("2022-04-01"), y = max(sp500_df$price), 
           label = "Historical Performance", hjust = 0, 
           fontface = "bold", color = colors$palette[4], size = 3.5
    ) +
  annotate(
    "text", x = as.Date("2022-04-01"), y = max(sp500_df$price) * 0.98, 
    label = paste0("Trend (signal) with noise overlay"), 
    hjust = 0, color = colors$palette[4], size = 3
    ) +
  annotate(
    "text", x = last_date + days(110), y = max(forecast_df$upper_95) * 0.95, 
    label = "95%", color = colors$palette[1], fontface = "bold", size = 3.5
    ) +
  annotate(
    "text", x = last_date + days(110), y = max(forecast_df$upper_80), 
    label = "80%", color = colors$palette[1], fontface = "bold", size = 3.5,
    vjust = 5
    ) +
  annotate(
    "text", x = last_date + days(110), y = max(forecast_df$upper_50), 
    label = "50%", color = colors$palette[1], fontface = "bold", size = 3.5,
    vjust = 5
    ) +
  annotate(
    "text", x = forecast_dates[length(forecast_dates)/4], y = min(forecast_df$lower_95) * 0.97, 
    label = paste0("Signal-to-noise ratio: ", round(signal_to_noise, 1)), 
    hjust = 0.5, fontface = "italic", size = 3
    ) +
  # Scales
  scale_y_continuous(
    labels = scales::dollar_format()
    ) +
  scale_x_date(
    date_breaks = "6 months", date_labels = "%b %Y"
    ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Price (USD)",
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
      size = rel(0.95),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 14)
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

# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-11
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# P cli           3.6.4    2025-02-13 [?] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark    1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
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
# P labeling      0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.3)
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
# P quantmod    * 0.4.26   2024-02-14 [?] CRAN (R 4.4.0)
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
# P TTR         * 0.24.4   2023-11-28 [?] CRAN (R 4.4.0)
# tzdb          0.5.0    2025-03-15 [1] CRAN (R 4.4.3)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P withr         3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] RSPM (R 4.4.0)
# xml2          1.3.8    2025-03-14 [1] CRAN (R 4.4.3)
# P xts         * 0.13.2   2024-01-21 [?] CRAN (R 4.4.0)
# P zoo         * 1.8-12   2023-04-13 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────
# > 