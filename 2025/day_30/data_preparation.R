
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
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  janitor,        # Simple Tools for Examining and Cleaning Dirty Data
  skimr,          # Compact and Flexible Summaries of Data
  scales,         # Scale Functions for Visualization
  lubridate,      # Make Dealing with Dates a Little Easier
  ggdist,         # Visualizations of Distributions and Uncertainty 
  sf              # Simple Features for R
)


# ----- Load and clean the data -----

# Load the Alaska metadata
alaska_meta <- read_csv('2025/data/ALA_Alaska_metadata.csv') |> 
  clean_names()

# Load the Alaska total error dataset          
alaska_total_error <- read_csv('2025/data/ALA_gla_mean-cal-mass-change_TOTAL-ERROR_obs_unobs.csv') |> 
  clean_names()

# Convert character columns in the metadata to numeric
alaska_meta <- alaska_meta |>
  mutate(
    # Handle potential "N/A" strings or other non-numeric values
    n_gla_anom_used = ifelse(n_gla_anom_used == "N/A" | n_gla_anom_used == "no_obs", 
                             NA_character_, n_gla_anom_used),
    mean_dist_gla_anom = ifelse(mean_dist_gla_anom == "N/A" | mean_dist_gla_anom == "no_obs", 
                                NA_character_, mean_dist_gla_anom),
    std_dist_gla_anom = ifelse(std_dist_gla_anom == "N/A" | std_dist_gla_anom == "no_obs", 
                               NA_character_, std_dist_gla_anom),
    min_dist_gla_anom = ifelse(min_dist_gla_anom == "N/A" | min_dist_gla_anom == "no_obs", 
                               NA_character_, min_dist_gla_anom),
    max_dist_gla_anom = ifelse(max_dist_gla_anom == "N/A" | max_dist_gla_anom == "no_obs", 
                               NA_character_, max_dist_gla_anom)
  ) |>
  # Then convert to numeric
  mutate(
    n_gla_anom_used = as.numeric(n_gla_anom_used),
    mean_dist_gla_anom = as.numeric(mean_dist_gla_anom),
    std_dist_gla_anom = as.numeric(std_dist_gla_anom),
    min_dist_gla_anom = as.numeric(min_dist_gla_anom),
    max_dist_gla_anom = as.numeric(max_dist_gla_anom)
  )

# ----- Transform the total error data from wide to long format -----

# First, get the columns that identify glaciers
id_cols <- c("rgi_id", "region", "cen_lon", "cen_lat", "area", "wgms_id")

# Transform the wide format to long format
alaska_ts <- alaska_total_error |>
  # Pivot the year columns to long format
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "error_estimate"
  ) |>
  # Clean up the year column by removing the 'x' prefix
  mutate(year = as.numeric(str_replace(year, "^x", "")))

# ----- Merge the datasets -----

# Join the metadata with the time series data
alaska_combined <- alaska_ts |>
  left_join(alaska_meta, by = c("rgi_id", "region", "cen_lon", "cen_lat", "area", "wgms_id"))

# ----- Calculate summary statistics by year -----

# Calculate mean error and related statistics by year
yearly_summary <- alaska_combined |>
  group_by(year) |>
  summarize(
    mean_error = mean(error_estimate, na.rm = TRUE),
    median_error = median(error_estimate, na.rm = TRUE),
    sd_error = sd(error_estimate, na.rm = TRUE),
    q25_error = quantile(error_estimate, 0.25, na.rm = TRUE),
    q75_error = quantile(error_estimate, 0.75, na.rm = TRUE),
    min_error = min(error_estimate, na.rm = TRUE),
    max_error = max(error_estimate, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# ----- Calculate summary statistics by glacier size -----

# Create size categories
alaska_combined <- alaska_combined |>
  mutate(
    size_category = case_when(
      area < 1 ~ "< 1 km²",
      area < 10 ~ "1-10 km²",
      area < 100 ~ "10-100 km²",
      TRUE ~ "> 100 km²"
    ),
    # Also create a factor version with ordered levels
    size_category_f = factor(size_category, 
                             levels = c("< 1 km²", "1-10 km²", "10-100 km²", "> 100 km²"))
  )

# Calculate statistics by size category
size_summary <- alaska_combined |>
  group_by(size_category_f) |>
  summarize(
    mean_error = mean(error_estimate, na.rm = TRUE),
    median_error = median(error_estimate, na.rm = TRUE),
    sd_error = sd(error_estimate, na.rm = TRUE),
    mean_distance = mean(mean_dist_gla_anom, na.rm = TRUE),
    median_distance = median(mean_dist_gla_anom, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# ----- Create spatial data -----

# Convert to SF object for mapping
alaska_glaciers_sf <- alaska_meta |>
  st_as_sf(coords = c("cen_lon", "cen_lat"), crs = 4326)

# ----- Calculate recent trends -----

# Look at trends in most recent years (e.g., last 20 years)
recent_years <- alaska_combined |>
  filter(year >= 2000) |>
  group_by(year) |>
  summarize(
    mean_error = mean(error_estimate, na.rm = TRUE),
    median_error = median(error_estimate, na.rm = TRUE),
    sd_error = sd(error_estimate, na.rm = TRUE),
    .groups = "drop"
  )

# ----- Calculate regional variations -----

# Group Alaska into sub-regions based on coordinate clusters
# Using a simple approach with latitude/longitude bins
alaska_combined <- alaska_combined |>
  mutate(
    lon_bin = cut(cen_lon, breaks = seq(-180, -130, by = 10), include.lowest = TRUE),
    lat_bin = cut(cen_lat, breaks = seq(55, 70, by = 5), include.lowest = TRUE),
    sub_region = paste(lon_bin, lat_bin, sep = "_")
  )

# Calculate statistics by sub-region
region_summary <- alaska_combined |>
  group_by(sub_region) |>
  summarize(
    mean_error = mean(error_estimate, na.rm = TRUE),
    mean_distance = mean(mean_dist_gla_anom, na.rm = TRUE),
    count = n(),
    mean_lat = mean(cen_lat, na.rm = TRUE),
    mean_lon = mean(cen_lon, na.rm = TRUE),
    .groups = "drop"
  )

# ----- Examine relationship between distance and error -----

# Calculate correlation between measurement distance and error
distance_error_cor <- alaska_combined |>
  filter(!is.na(mean_dist_gla_anom) & !is.na(error_estimate)) |>
  summarize(correlation = cor(mean_dist_gla_anom, error_estimate, use = "complete.obs"))

# Create a dataset for plotting the relationship
distance_error_data <- alaska_combined |>
  filter(!is.na(mean_dist_gla_anom) & !is.na(error_estimate))

# Save prepared data for visualization
saveRDS(alaska_combined, "2025/data/alaska_combined_data.rds")
saveRDS(yearly_summary, "2025/data/alaska_yearly_summary.rds")
saveRDS(alaska_glaciers_sf, "2025/data/alaska_glaciers_sf.rds")

# ----- Housekeeping-----
rm(
  alaska_meta, alaska_total_error, alaska_ts, distance_error_cor, 
  distance_error_data, recent_years, region_summary, size_summary
   )

gc()

