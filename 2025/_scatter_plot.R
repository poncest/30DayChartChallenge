

# Load required libraries
library(hoopR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Get NBA player stats for 2023-24 season
players_2024_raw <- nba_leaguedashplayerstats(season = "2023-24")
players_2024 <- players_2024_raw$LeagueDashPlayerStats

# Convert character columns to numeric
players_2024 <- players_2024 %>%
  mutate(across(c(GP, MIN, AST, TOV, PTS, REB, STL, BLK), as.numeric))

# Filter and prepare data
viz_data <- players_2024 %>%
  filter(MIN >= 15, GP >= 40) %>% # Players with sufficient minutes
  # Calculate assist-to-turnover ratio
  mutate(
    AST_TOV_RATIO = ifelse(TOV > 0, AST / TOV, AST),
    # Calculate scoring efficiency (points per minute)
    PTS_PER_MIN = PTS / MIN,
    # Label for notable players
    label = ifelse(
      (AST_TOV_RATIO > quantile(AST_TOV_RATIO, 0.9, na.rm = TRUE) & PTS_PER_MIN > quantile(PTS_PER_MIN, 0.8, na.rm = TRUE)) |
        (AST_TOV_RATIO < quantile(AST_TOV_RATIO, 0.1, na.rm = TRUE) & PTS_PER_MIN > quantile(PTS_PER_MIN, 0.8, na.rm = TRUE)) |
        (AST_TOV_RATIO > 3.5 | PTS_PER_MIN > 0.9),
      PLAYER_NAME, NA
    )
  )

# Create the distribution visualization
ggplot(viz_data, aes(x = AST_TOV_RATIO, y = PTS_PER_MIN, color = AST_TOV_RATIO - median(AST_TOV_RATIO, na.rm = TRUE))) +
  # Background elements
  geom_hline(yintercept = median(viz_data$PTS_PER_MIN, na.rm = TRUE), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = median(viz_data$AST_TOV_RATIO, na.rm = TRUE), linetype = "dashed", alpha = 0.5) +
  # Main scatter plot
  geom_point(aes(size = MIN)) +
  # Add labels for notable players
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3, max.overlaps = 15) +
  # Color scale - diverging
  scale_color_gradient2(
    low = "firebrick", 
    mid = "gray80", 
    high = "forestgreen", 
    midpoint = 0,
    name = "AST/TOV vs. Median"
  ) +
  scale_y_continuous(name = "Points Per Minute") +
  scale_x_continuous(name = "Assist to Turnover Ratio") +
  scale_size_continuous(name = "Minutes Per Game", range = c(2, 8)) +
  # Add quadrant labels
  annotate("text", x = quantile(viz_data$AST_TOV_RATIO, 0.95, na.rm = TRUE), 
           y = quantile(viz_data$PTS_PER_MIN, 0.05, na.rm = TRUE), 
           label = "Good Playmakers\nLow Scorers", size = 3, hjust = 1, vjust = 0) +
  annotate("text", x = quantile(viz_data$AST_TOV_RATIO, 0.95, na.rm = TRUE), 
           y = quantile(viz_data$PTS_PER_MIN, 0.95, na.rm = TRUE), 
           label = "Elite Guards\nScoring Playmakers", size = 3, hjust = 1, vjust = 1) +
  annotate("text", x = quantile(viz_data$AST_TOV_RATIO, 0.05, na.rm = TRUE), 
           y = quantile(viz_data$PTS_PER_MIN, 0.05, na.rm = TRUE), 
           label = "Limited Role Players", size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = quantile(viz_data$AST_TOV_RATIO, 0.05, na.rm = TRUE), 
           y = quantile(viz_data$PTS_PER_MIN, 0.95, na.rm = TRUE), 
           label = "Score-First Players\nHigh Turnover", size = 3, hjust = 0, vjust = 1) +
  # Titles and theme
  labs(
    title = "NBA 2023-24: Playmaking Efficiency vs. Scoring Output",
    subtitle = "Distribution of players by Assist-to-Turnover Ratio and Points Per Minute",
    caption = "Data: NBA via {hoopR} | Visualization: #30DayChartChallenge Day 9: Distribution/Diverging"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )