## 30DayChartChallenge Contribution Heatmap 
## Author: Steven Ponce
## Purpose: GitHub-style calendar heatmap for README

# 1. LOAD PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggtext, showtext, glue, here)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 2.5,
  units  = "in",
  dpi    = 320
)

# 2. DATA EXTRACTION ----
get_contributions_30day <- function(base_path, year_range = 2024:2025) {
  
  # Find year folders
  year_folders <- list.dirs(base_path, recursive = FALSE, full.names = FALSE)
  years <- year_folders[str_detect(year_folders, "^\\d{4}$")] |> as.integer()
  years <- years[years %in% year_range]
  
  map_dfr(years, \(yr) {
    year_path <- file.path(base_path, yr)
    
    # List day_XX folders
    day_folders <- list.dirs(year_path, recursive = FALSE, full.names = FALSE)
    day_folders <- day_folders[str_detect(day_folders, "^day_\\d+$")]
    
    if (length(day_folders) == 0) return(NULL)
    
    tibble(
      year = yr,
      day = as.integer(str_extract(day_folders, "\\d+"))
    )
  }) |> 
    arrange(year, day)
}

# 3. BUILD CALENDAR GRID ----
build_calendar_grid_30day <- function(contributions, year_range = 2024:2025) {
  
  # 30-day challenge grid
  full_grid <- expand_grid(
    year = year_range,
    day = 1:30
  ) |>
    left_join(
      contributions |> mutate(contributed = TRUE),
      by = c("year", "day")
    ) |>
    mutate(
      contributed = replace_na(contributed, FALSE)
    )
  
  return(full_grid)
}

# 4. VISUALIZATION ----
create_heatmap_30day <- function(calendar_data) {
  
  # Fonts
  font_add_google("Oswald", "title")
  font_add_google("Inter", "text")
  showtext_auto()
  
  # Colors - Coral/salmon to differentiate from other challenges
  col_contributed <- "#7209b7" 
  col_empty       <- "#1b2129"
  col_text        <- "#7d8590"
  col_title       <- "#e6edf3"
  bkg_col         <- "#0d1117"
  
  # Stats
  total <- sum(calendar_data$contributed, na.rm = TRUE)
  years_active <- n_distinct(calendar_data$year)
  
  # Year totals
  year_totals <- calendar_data |>
    group_by(year) |>
    summarize(n = sum(contributed, na.rm = TRUE), .groups = "drop")
  
  # Plot
  p <- calendar_data |> 
    mutate(year = factor(year, levels = rev(sort(unique(year))))) |>
    ggplot(aes(x = day, y = year)) +
    
    # Tiles
    geom_tile(
      aes(fill = contributed),
      color = bkg_col,
      linewidth = 0.8,
      width = 0.9,
      height = 0.9
    ) +
    
    # Year count labels on right
    geom_text(
      data = year_totals |> mutate(year = factor(year, levels = rev(sort(unique(year))))),
      aes(x = 32, y = year, label = n),
      color = col_text,
      family = "text",
      size = 6,
      hjust = 0
    ) +
    
    # Scales
    scale_fill_manual(
      values = c("TRUE" = col_contributed, "FALSE" = col_empty),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = c(1, 5, 10, 15, 20, 25, 30),
      expand = expansion(mult = c(0.01, 0.08))
    ) +
    
    # Labels
    labs(
      title = "#30DayChartChallenge Contributions",
      subtitle = glue("{total} visualizations across {years_active} years"),
      x = "Day",
      y = NULL
    ) +
    
    # Theme
    theme_minimal(base_family = "text") +
    theme(
      plot.background = element_rect(fill = bkg_col, color = NA),
      panel.background = element_rect(fill = bkg_col, color = NA),
      panel.grid = element_blank(),
      axis.text.x = element_text(color = col_text, size = 14),
      axis.text.y = element_text(color = col_text, size = 14, face = "bold"),
      plot.title = element_text(
        family = "title",
        color = col_title,
        size = 30,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        color = col_text,
        size = 18,
        margin = margin(b = 10)
      ),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  return(p)
}

# 5. EXECUTION ----

# Extract data
df_contributions <- get_contributions_30day(here::here(), year_range = 2024:2025)

# Quick check
cat("=== 30DayChartChallenge Contributions ===\n")
df_contributions |> 
  count(year, name = "days") |> 
  print()

cat("\nTotal:", nrow(df_contributions), "\n")

# Build grid and plot
df_grid <- build_calendar_grid_30day(df_contributions, year_range = 2024:2025)
viz <- create_heatmap_30day(df_grid)

# Display
print(viz)

# Save
ggsave(
  filename = here::here("summary/30daychartchallenge_heatmap.png"),
  plot = viz,
  width = 10,
  height = 3,
  dpi = 320,
  bg = "#0d1117"
)
