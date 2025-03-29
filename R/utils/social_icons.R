## social_icons.R

# Social media icons configuration
get_social_icons <- function() {
    list(
        linkedin = str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>"),
        github   = str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>"),
        bluesky  = str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
    )
}

# Create social media caption (#30DayChartChallenge)
create_dcc_caption <- function(dcc_year, dcc_day, source_text) {
    icons <- get_social_icons()
    
    dcc_text <- str_glue("#30DayChartChallenge: {dcc_year} Day {dcc_day} &bull; Source: {source_text}<br>")
    social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
    
    str_glue("{dcc_text} {social_text}")
}
