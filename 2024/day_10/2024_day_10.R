
## Challenge: #30DayChartChallenge 2024 day 10
## Topic:     Distributions | physical
## Author:    Steven Ponce
## Date:      2024-04-10

## Data:      M&M's Milk Chocolate, Share Size, wt 3.14oz
##            Recorded Manually
## Link:      


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  readxl,      # Read Excel Files
)


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 6,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
candy_color_data <- read_excel(here::here("2024/data/m&m_milk_chocolate_3.14oz.xlsx")) |>
  clean_names() |>
  glimpse()



## 3. EXAMINING THE DATA ----
glimpse(candy_color_data)
skim(candy_color_data)
colnames(candy_color_data)



## 4. TIDYDATA ----

### |- Tidy ----
data_plot <- candy_color_data |> 
  pivot_longer(cols = -count, names_to = 'temp', values_to = 'color') |> 
  select(color) |> 
  summarise(n = n(), .by = color) |> 
  filter(!is.na(color)) |> 
  mutate(
    color = str_to_title(color),
    color = factor(color, levels = c("Red", "Blue","Orange","Green","Brown", "Yellow"))
  ) 


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- 'white'
title_col    <- "gray10"              
subtitle_col <- "gray20"   
caption_col  <- "gray20"  
text_col     <- "gray20"    


### |-  titles and caption ----
# icons
tt <- str_glue("#30DayChartChallenge: { 2024 } Day { 10 } &bull; Source: Manual Collection<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text   <- str_glue("M&M Milk Chocolate, Share Size, 3.14oz") 

subtitle_text <- str_glue("Distribution of M&M colors in a randomly selected bag.")

caption_text <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Poppins", regular.wt = 400, family = "title")       
font_add_google("Poppins", family = "subtitle")  
font_add_google("Inter", regular.wt = 400, family = "text")        
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_classic(base_size = 14, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  
  axis.text             = element_text(size = rel(.8), color = text_col, family = 'text'),
  
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
)


### |-  final plot ----  

data_plot  |> 
  
  ggplot(aes(x = color, y = n)) + 
  
  # Geoms
  geom_col() +

  # Scales
  scale_x_discrete() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +

  # Labs
  labs(
    x        = "Color",
    y        = "Count",
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
  ) +
  
  # Theme
  theme(
    plot.title      = element_text(
      size          = rel(2),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(1), 
      family        = 'subtitle',
      color         = title_col,
      lineheight    = 0.85, 
      margin        = margin(t = 0, b = 20)
    ),
    plot.caption    = element_markdown(
      size          = rel(.7),
      family        = "caption",
      color         = caption_col,
      lineheight    = 0.65,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 10, b = 5)
    ),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

