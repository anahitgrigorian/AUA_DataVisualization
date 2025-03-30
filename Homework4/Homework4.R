library(tidyverse)

df <- read.csv('bundesliga2.csv')
df$TEAM <- trimws(df$TEAM)

unique_teams <- unique(df$TEAM)
team_colors <- setNames(RColorBrewer::brewer.pal(n = length(unique_teams), name = "Set3")[1:length(unique_teams)], unique_teams)

pdf("points_by_season_R.pdf", width = 10, height = 8)

for (season in sort(unique(df$SEASON))) {
  season_df <- df %>% 
    filter(SEASON == season) %>%
    arrange(POINTS)
  
  winner <- season_df %>% arrange(desc(POINTS)) %>% slice(1) %>% pull(TEAM)
  
  season_df$FillColor <- ifelse(season_df$TEAM == winner, team_colors[season_df$TEAM], "lightgray")
  
  p <- ggplot(season_df, aes(x = POINTS, y = reorder(TEAM, POINTS), fill = FillColor)) +
    geom_col() +
    scale_fill_identity() +
    labs(
      title = paste("Total Points - Season", season),
      subtitle = paste("Winner:", winner),
      x = "Points", y = "Team"
    ) +
    theme_minimal()
  
  print(p)
}

dev.off()
