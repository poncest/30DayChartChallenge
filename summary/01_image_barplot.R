## Challenge: #30DayChartChallenge 2025
## Author:    Steven Ponce
## Date:      2025-04-15

## Goal:      Generate a bar plot summary using images of my #30DayChartChallenge contributions

# Section 1: Load Packages ----
pacman::p_load(
  tidyverse,      # Data manipulation and visualization
  ggtext,         # Text rendering in ggplot2
  showtext,       # Font handling
  camcorder,      # Plot recording
  scales,         # Scale functions
  ggtextures      # Image handling in ggplot2
)

# Section 2: Functions ----

# Get PNG files for a specific year
get_png_files <- function(year) {
  list.files(
    path = here::here(as.character(year)), 
    pattern = "\\.png$",
    full.names = TRUE, 
    recursive = TRUE
  )
}

# Create contribution theme
create_contribution_theme <- function(bkg_col, text_col, title_col, caption_col) {
  theme_minimal(base_size = 20, base_family = 'text') +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position       = 'top',
      plot.background       = element_rect(fill = bkg_col, color = bkg_col),
      panel.background      = element_rect(fill = bkg_col, color = bkg_col),
      plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
      axis.line.x           = element_line(color = "black"),
      axis.ticks.x          = element_line(color = "black", unit(0.4,"cm")),
      axis.text             = element_text(
        family = 'text', 
        color = text_col, 
        size = 16
      ),
      axis.title = element_text(
        family = 'text', 
        color = text_col, 
        size = 20
      ),
      panel.grid = element_blank(),
      plot.title = element_text(
        family = 'title',
        color = title_col,
        face = "bold",
        size = 28,  
        margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_markdown(
        family = 'caption',
        color = caption_col,
        lineheight = 0.6,
        size = 12,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(t = 15, b = 5)
      )
    )
}

# Create plot
create_image_plot <- function(data, 
                              img_width = 1, 
                              img_height = 1,
                              colors = list(
                                bkg = "#A1B0AB",
                                title = "#0B132B",
                                text = "#0B132B",
                                caption = "#0B132B"
                              )) {
  
  # Create social media icons
  icons <- list(
    li = str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>"),
    gh = str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>"),
    bs = str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
  )
  
  # Create plot
  data |> 
    ggplot(aes(year, count, image = image_path)) +
    geom_isotype_col(
      img_width = grid::unit(img_width, "native"), 
      img_height = unit(img_height, "null"),
      ncol = NA, 
      nrow = 1, 
      hjust = 0, 
      vjust = 0.5, 
      fill = "#80808040"
    ) +
    scale_image_identity() +
    coord_flip() +
    labs(
      x = "",
      y = "Count",
      title = "#30DayChartChallenge Contributions",
      caption = str_glue(
        "{icons$li} stevenponce &bull; {icons$bs} sponce1 &bull; 
                {icons$gh} poncest &bull; #rstats #ggplot2"
      )
    ) +
    create_contribution_theme(
      bkg_col = colors$bkg,
      text_col = colors$text,
      title_col = colors$title,
      caption_col = colors$caption
    )
}

# Setup visualization
setup_visualization <- function() {
  # Set up fonts
  font_add('fa6-brands', here::here('fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf'))
  font_add_google("Rock Salt", family = "title")
  font_add_google("Lato", family = "text")
  font_add_google("PT Sans Narrow", family = "caption")
  showtext_auto(enable = TRUE)
  
  # Set up recording
  gg_record(
    dir = here::here("temp_plots"),
    device = "png",
    width = 10,
    height = 6,
    units = "in",
    dpi = 320
  )
  
  # Set resolution
  showtext_opts(dpi = 320)
}


# Section 3: Main Execution ----
main <- function() {
  # Setup
  setup_visualization()
  
  # Create data frame with both 2024 and 2025
  images_df <- tibble(
    year = c(rep(2024, each = 1), rep(2025, each = 1))
  ) |>
    mutate(
      image_path = map(year, get_png_files),
      year = as.factor(year)
    ) |>
    unnest(image_path) |>
    mutate(
      # Add action column for filtering
      action = case_when(
        str_detect(image_path, "\\initial") | 
          str_detect(image_path, "topics") ~ "remove",
        TRUE ~ "keep"
      )
    ) |>
    filter(
      file.exists(image_path),  # Keep existing files
      action == "keep"          # Filter out unwanted images
    ) |>
    arrange(year) |>
    mutate(count = 1) |>
    select(year, image_path, count)  # Keep only needed columns
  
  # Group by year to validate counts
  year_counts <- images_df |>
    group_by(year) |>
    summarise(count = n())
  
  # Print counts for validation
  print(year_counts)
  
  # Warning if any year doesn't have 30 images
  for(yr in unique(images_df$year)) {
    yr_count <- nrow(filter(images_df, year == yr))
    if(yr_count != 30) {
      warning(sprintf("Year %s: Expected 30 images, but found %d", yr, yr_count))
    }
  }
  
  # Create and save plot
  images_df |>
    create_image_plot() |>
    ggsave(
      path = here::here("summary/"),
      filename = "image_plot.png",
      width = 10,
      height = 6,
      units = 'in',
      dpi = 320
    )
}

# Run ----
main()


# Section 4: Session Info ---- 

# Save session info to file
session_info <- sessioninfo::session_info(include_base = TRUE) 
session_info

# if needed
writeLines(capture.output(print(session_info)), "session_info.txt")