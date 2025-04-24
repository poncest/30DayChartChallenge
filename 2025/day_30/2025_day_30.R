
## Challenge: #30DayChartChallenge 2025 day 30
## Topic:     Uncertainties | national geographics (theme)
## Author:    Steven Ponce
## Date:      2024-04-29

##  Citation: Dussaillant, I., Hugonnet, R., Huss, M., Berthier, E., Bannwart, J., Paul, F., 
##  and Zemp, M. (2025): Annual mass-change estimates for the world’s glaciers. Individual 
##  glacier time series and gridded data products. Digital media. 
##  https://doi.org/10.5904/wgms-amce-2025-02

## 1. LOAD PACKAGES & SETUP ----  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,          # Easily Install and Load the 'Tidyverse'
  ggtext,             # Improved Text Rendering Support for 'ggplot2'
  showtext,           # Using Fonts More Easily in R Graphs
  janitor,            # Simple Tools for Examining and Cleaning Dirty Data
  skimr,              # Compact and Flexible Summaries of Data
  scales,             # Scale Functions for Visualization
  lubridate,          # Make Dealing with Dates a Little Easier
  ggdist,             # Visualizations of Distributions and Uncertainty 
  sf,                 # Simple Features for R
  rnaturalearth,      # World Map Data from Natural Earth
  rnaturalearthdata,  # World Vector Map Data from Natural Earth Used in 'rnaturalearth'
  viridis,            # Colorblind-Friendly Color Maps for R
  ggnewscale,         # Multiple Fill and Colour Scales in 'ggplot2'
  ggrepel             # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

alaska_combined <- readRDS("2025/data/alaska_combined_data.rds") |> 
  clean_names()  


## 3. EXAMINING THE DATA ----
glimpse(alaska_combined)
skim(alaska_combined)


## 4. DATA PREP ----

# Note: 
# For the data preparation step, refer to the `data_preparation.R` file.


## 4.1. TIDYDATA  ----

# Base map data
world <- ne_countries(scale = "medium", returnclass = "sf")
alaska_bounds <- st_bbox(c(
  xmin = -170, ymin = 55, xmax = -130, ymax = 70), crs = st_crs(4326)
  )

# Calculate average error per glacier over all years 
glacier_avg_error <- alaska_combined |>
  group_by(rgi_id, region, cen_lon, cen_lat, area) |>
  summarize(
    mean_error = mean(error_estimate, na.rm = TRUE),
    mean_dist = mean(mean_dist_gla_anom, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to SF for mapping
glacier_error_sf <- glacier_avg_error |>
  st_as_sf(coords = c("cen_lon", "cen_lat"), crs = 4326)

# Define regions 
region_points <- tibble(
  region_id = c("Region 1", "Region 2", "Region 3"),          
  lon = c(-166, -142, -158),
  lat = c(56.5, 62, 69),
  color = c("gray20", "gray20", "gray20")  
)

# Convert to SF for plotting
region_points_sf <- st_as_sf(
  region_points, coords = c("lon", "lat"), crs = 4326
  )

# Create informational box 
# Extract uncertainty statistics
uncertainty_stats <- glacier_avg_error |>
  summarize(
    mean_error = mean(mean_error, na.rm = TRUE),
    max_error = max(mean_error, na.rm = TRUE),
    min_error = min(mean_error, na.rm = TRUE),
    glacier_count = n()
  )

# Textbox
info_box_text <- paste0(
  "UNCERTAINTY FACTORS\n\n",
  "• Distance from observation points\n",
  "• Small glaciers (<1 km²)\n",
  "• Remote mountain locations\n",
  "• Avg. measurement error: ", round(uncertainty_stats$mean_error, 2), " m w.e.\n",
  "• Total glaciers: ", uncertainty_stats$glacier_count
)


# 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "water" = "#71ABD8",       
    "land" = "#E8E6D9",       
    "glacier" = "#D1E6EC",     
    "highlight" = "#FFCD00",   
    "text" = "#000000",        
    "uncertainty" = "#8BB9DD", 
    "dark_blue" = "#1A5088",   
    "text_gray" = "#555555"    
    )
  )          
 
### |-  titles and caption ----
# text
title_text    <- str_glue("THE HIDDEN UNCERTAINTY")

subtitle_text <- str_glue("Mapping measurement challenges in Alaska's vanishing glaciers\n
                          Color indicates average error estimate magnitude (1946-2023)")

caption_text <- create_dcc_caption(
  dcc_year = 2025,
  dcc_day = 30,
  source_text =  "World Glacier Monitoring Service" 
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# National Geographic theme (at least my interpretation)
theme_natgeo <- function() {
  theme_minimal(base_size = 14) +
    theme(
      # Text elements
      plot.title = element_text(face = "bold", size = 20, family = "sans"),
      plot.subtitle = element_text(size = 14, family = "sans", margin = margin(b = 20)),
      axis.title = element_text(face = "bold", size = 10),
      legend.title = element_text(face = "bold", size = 10),
      
      # Grid elements
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      
      # Background elements
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins and spacing
      plot.margin = margin(20, 20, 20, 20),
      legend.margin = margin(10, 10, 10, 10),
      
      # Caption styling
      plot.caption = element_text(size = 9, hjust = 0, color = colors$palette["text_gray"], 
                                  margin = margin(t = 15))
    )
}

# Main map -----
main_map <- ggplot() +
  # Base map
  geom_sf(data = world, fill = colors$palette['land'], color = "gray70") +
  
  # Geoms
  geom_sf(
    data = glacier_error_sf, 
    aes(size = area, color = mean_error),
    alpha = 0.8
    ) +
  geom_label(
    data = region_points_sf,
    aes(geometry = geometry, label = region_id),
    stat = "sf_coordinates",
    fill = "white",
    color = region_points$color, 
    fontface = "bold",
    size = 4,
    label.padding = unit(0.4, "lines"),
    label.r = unit(0.15, "lines"),
    alpha = 0.9,
    label.size = 0.8  
  ) +
  # Scales
  scale_color_viridis_c(
    name = "Mean Error\nEstimate (m w.e.)",
    option = "plasma",
    direction = -1,
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 12,
      barheight = 1
    )
  ) +
  scale_size_continuous(
    name = "Glacier Area (km²)",
    range = c(0.1, 3.5),
    breaks = c(1, 10, 100, 1000),
    trans = "log10",
    labels = label_comma(),
    guide = guide_legend(
      title.position = "top",
      override.aes = list(color = colors$palette["dark_blue"])
    )
  ) +
  coord_sf(  # Alaska region
    xlim = c(alaska_bounds$xmin, alaska_bounds$xmax), 
    ylim = c(alaska_bounds$ymin, alaska_bounds$ymax)
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL
  ) +
  # Theme
  theme_natgeo() +
  theme(

    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.spacing.x = unit(1, "cm"),
    legend.box.spacing = unit(0.5, "cm"),
    legend.key.size = unit(0.8, "cm"),

    plot.title = element_text(
      size = rel(2.6),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.90),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 0.8,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color = colors$caption,
      lineheight = 0.65,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 10, b = 5)
    ),
  )


# Info textbox -----
info_box <- ggplot() +
  annotate(
    "rect",
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    fill = alpha(colors$palette["highlight"], 0.08)
  ) +
  annotate(
    "text",
    x = 0.05, y = 0.5,
    label = info_box_text,
    hjust = 0, vjust = 0.5,
    size = 3.2,  
    fontface = "plain",
    color = colors$palette["text"]
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    fill = NA, color = alpha(colors$palette["highlight"], 0.5),
    linewidth = 1
  ) +
  theme_void() +
  xlim(0, 1) + ylim(0, 1)

# Combine main map and info box -----
combined_map <- main_map +
  annotation_custom(
    grob = ggplotGrob(info_box),
    xmin = -143, xmax = -128,  
    ymin = 62.5, ymax = 68.5     
  )

# Final map -----
final_map <- combined_map +
  theme(
    plot.background = element_rect(
      fill = "white", 
      color = colors$palette["highlight"], 
      linewidth = 5
    )
  )

final_map


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-04-15
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────
# ! package           * version  date (UTC) lib source
# V base              * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc           0.1-3    2015-07-28 [?] RSPM (R 4.4.0)
# P camcorder           0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# P class               7.3-22   2023-05-03 [?] CRAN (R 4.4.0)
# P classInt            0.4-10   2023-09-05 [?] CRAN (R 4.4.0)
# P cli                 3.6.4    2025-02-13 [?] RSPM (R 4.4.0)
# P codetools           0.2-20   2024-03-31 [?] CRAN (R 4.4.0)
# colorspace          2.1-1    2024-07-26 [1] CRAN (R 4.4.3)
# P commonmark          1.9.1    2024-01-30 [?] RSPM (R 4.4.0)
# P compiler            4.4.0    2024-04-24 [?] local
# crayon              1.5.3    2024-06-20 [1] CRAN (R 4.4.3)
# curl                6.2.2    2025-03-24 [1] CRAN (R 4.4.3)
# P datasets          * 4.4.0    2024-04-24 [?] local
# P DBI                 1.2.2    2024-02-16 [?] RSPM (R 4.4.0)
# digest              0.6.37   2024-08-19 [1] CRAN (R 4.4.3)
# P distributional      0.4.0    2024-02-07 [?] RSPM (R 4.4.0)
# P dplyr             * 1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# P e1071               1.7-14   2023-12-06 [?] CRAN (R 4.4.0)
# farver              2.1.2    2024-05-13 [1] CRAN (R 4.4.3)
# P fastmap             1.2.0    2024-05-15 [?] RSPM (R 4.4.0)
# P forcats           * 1.0.0    2023-01-29 [?] RSPM (R 4.4.0)
# P generics            0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggdist            * 3.3.2    2024-03-05 [?] RSPM (R 4.4.0)
# ggnewscale        * 0.5.1    2025-02-24 [1] CRAN (R 4.4.3)
# P ggplot2           * 3.5.1    2024-04-23 [?] CRAN (R 4.4.0)
# P ggrepel           * 0.9.5    2024-01-10 [?] CRAN (R 4.4.0)
# P ggtext            * 0.1.2    2022-09-16 [?] RSPM (R 4.4.0)
# P gifski              1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# glue                1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# P graphics          * 4.4.0    2024-04-24 [?] local
# P grDevices         * 4.4.0    2024-04-24 [?] local
# P grid                4.4.0    2024-04-24 [?] local
# P gridExtra           2.3      2017-09-09 [?] CRAN (R 4.4.0)
# P gridtext            0.1.5    2022-09-16 [?] RSPM (R 4.4.0)
# gtable              0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# P here              * 1.0.1    2020-12-13 [?] RSPM (R 4.4.0)
# P hms                 1.1.3    2023-03-21 [?] RSPM (R 4.4.0)
# P htmltools           0.5.8.1  2024-04-04 [?] RSPM (R 4.4.0)
# P httr                1.4.7    2023-08-15 [?] RSPM (R 4.4.0)
# P janitor           * 2.2.0    2023-02-02 [?] RSPM (R 4.4.0)
# jsonlite            1.9.1    2025-03-03 [1] CRAN (R 4.4.3)
# P KernSmooth          2.23-22  2023-07-10 [?] CRAN (R 4.4.0)
# P knitr               1.46     2024-04-06 [?] RSPM (R 4.4.0)
# P labeling            0.4.3    2023-08-29 [?] CRAN (R 4.4.0)
# P lifecycle           1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate         * 1.9.3    2023-09-27 [?] RSPM (R 4.4.0)
# P magick              2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# P magrittr            2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown            1.12     2023-12-06 [?] RSPM (R 4.4.0)
# P methods           * 4.4.0    2024-04-24 [?] local
# munsell             0.5.1    2024-04-01 [1] CRAN (R 4.3.3)
# P pacman            * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# pillar              1.10.1   2025-01-07 [1] CRAN (R 4.4.3)
# P pkgconfig           2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P proxy               0.4-27   2022-06-09 [?] CRAN (R 4.4.0)
# P purrr             * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# R6                  2.6.1    2025-02-15 [1] CRAN (R 4.4.3)
# P ragg                1.3.1    2024-05-06 [?] CRAN (R 4.4.0)
# Rcpp                1.0.12   2024-01-09 [1] CRAN (R 4.3.3)
# P readr             * 2.1.5    2024-01-10 [?] RSPM (R 4.4.0)
# renv                1.0.5    2024-02-29 [1] CRAN (R 4.3.3)
# P repr                1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang               1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
# P rnaturalearth     * 1.0.1    2023-12-15 [?] CRAN (R 4.4.0)
# rnaturalearthdata * 1.0.0    2024-02-09 [1] CRAN (R 4.4.3)
# P rprojroot           2.0.4    2023-11-05 [?] RSPM (R 4.4.0)
# rstudioapi          0.17.1   2024-10-22 [1] CRAN (R 4.4.3)
# P rsvg                2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# P scales            * 1.3.0    2023-11-28 [?] CRAN (R 4.4.0)
# P sessioninfo         1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P sf                * 1.0-16   2024-03-24 [?] CRAN (R 4.4.0)
# P showtext          * 0.9-7    2024-03-02 [?] RSPM (R 4.4.0)
# P showtextdb        * 3.0      2020-06-04 [?] RSPM (R 4.4.0)
# P skimr             * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase           0.11.1   2023-08-27 [?] RSPM (R 4.4.0)
# P stats             * 4.4.0    2024-04-24 [?] local
# stringi             1.8.7    2025-03-27 [1] CRAN (R 4.4.3)
# P stringr           * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite             2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# P sysfonts          * 0.8.9    2024-03-02 [?] RSPM (R 4.4.0)
# P systemfonts         1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P terra               1.7-71   2024-01-31 [?] CRAN (R 4.4.0)
# P textshaping         0.3.7    2023-10-09 [?] RSPM (R 4.4.0)
# P tibble            * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# P tidyr             * 1.3.1    2024-01-24 [?] RSPM (R 4.4.0)
# tidyselect          1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# P tidyverse         * 2.0.0    2023-02-22 [?] RSPM (R 4.4.0)
# P timechange          0.3.0    2024-01-18 [?] RSPM (R 4.4.0)
# P tools               4.4.0    2024-04-24 [?] local
# tzdb                0.5.0    2025-03-15 [1] CRAN (R 4.4.3)
# P units               0.8-5    2023-11-28 [?] CRAN (R 4.4.0)
# P utf8                1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils             * 4.4.0    2024-04-24 [?] local
# P vctrs               0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P viridis           * 0.6.5    2024-01-29 [?] CRAN (R 4.4.0)
# P viridisLite       * 0.4.2    2023-05-02 [?] CRAN (R 4.4.0)
# P withr               3.0.2    2024-10-28 [?] RSPM (R 4.4.0)
# P xfun                0.43     2024-03-25 [?] RSPM (R 4.4.0)
# xml2                1.3.8    2025-03-14 [1] CRAN (R 4.4.3)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────
# > 
