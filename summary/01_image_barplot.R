## Challenge: #30DayChartChallenge 2024
## Author:    Steven Ponce
## Date:      2024-04-30

## Goal:      Generate a bar plot summary using images of my #30DayChartChallenge contributions


## References:
# https://github.com/clauswilke/ggtextures


# devtools::install_github("clauswilke/ggtextures")


# Section 1: Load Packages ----
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  camcorder,   # Record Your Plot History
  scales,      # Scale Functions for Visualization
  ggtextures   # Drawing Textured Rectangles and Bars with grid and ggplot2
)


# Section 2: Data ----
# list all files in `2023` folders
all_2024_files <- list.files(
  path = here::here("2024"),
  full.names = TRUE,
  recursive = TRUE
)

# keep only images in `2023` folders
all_2024_img <- all_2024_files[
  which(
    str_detect(all_2024_files, "\\.png")
  )
]

# combine images list
images_list <- all_2024_img  

# plot data
images_df <- data.frame(images_list) |>
  pivot_longer(cols = everything()) |>
  rename(image_path = value) |>
  mutate(
    year = str_extract(image_path, "[0-9]{4}"),
    year = factor(year),
    count = 1,
    action = case_when(
      str_detect(image_path, "\\initial") | str_detect(image_path, "topics") ~ "remove",
      TRUE ~ "keep"
    )
  ) |>
  filter(action == "keep") |>
  select(year, image_path, count)



# Section 3: Visualization ----

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 7.5,
  height = 3.93,
  units  = "in",
  dpi    = 320
)


### |- resolution ----
showtext_opts(dpi = 320)


### |- plot aesthetics ----
bkg_col     <- "#A1B0AB"
title_col   <- "#0B132B"
caption_col <- "#0B132B"
text_col    <- "#0B132B"


### |-  titles and caption ----
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("#30DayChartChallenge Contributions")

caption_text  <- str_glue("{li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Rock Salt", family = "title")
font_add_google("Lato", family = "text")
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)


### |-  plot theme ----
theme_set(theme_minimal(base_size = 16, base_family = "text"))

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "plot",
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "black", linewidth = .2),
  axis.ticks.x          = element_line(color = "black", unit(0.4, "cm")),
  panel.grid.major.y    = element_line(linetype = "solid", linewidth = 0.02, color = "#fafafa"),
  panel.grid.minor.y    = element_blank(),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
)


### |-  Plot Function ----
create_image_plot <- function(data) {
  images_df |>
    ggplot(aes(year, count, image = image_path)) +

    # geoms
    ggtextures::geom_isotype_col(
      img_width = grid::unit(1, "native"), img_height = unit(1, "null"),
      ncol = NA, nrow = 1, hjust = 0, vjust = 0.5, fill = "#80808040"
    ) +

    # scale
    scale_image_identity() +
    coord_flip() +

    # labs
    labs(
      x = "",
      y = "Count",
      title = title_text,
      caption = caption_text
    ) 
} 


### |-  final plot ----
# image_plot <- 
  create_image_plot(data = images_df) +
  
  # Theme
  theme(  
    plot.title      = element_text(
      size          = rel(1.3),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.caption    = element_markdown(
      size          = rel(.5),
      family        = "caption",
      color         = caption_col,
      lineheight    = 0.65,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 5, b = 5)
    ),
  ) 



# Section 4: Session Info ----

# Save session info to file
session_info <- sessioninfo::session_info(include_base = TRUE)
session_info

# if needed
# writeLines(capture.output(print(session_info)), "session_info.txt")
