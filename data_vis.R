
install.packages("extrafont")
install.packages("tidyverse")
install.packages("scales")


library("tidyverse")
library("scales")
library("ggrepel")

# ----- Import Raw Data --------

player_valuations <- read_csv("data/player_valuations.csv")
players <- read_csv("data/players.csv")   
appearances <- read_csv("data/appearances.csv") 
clubs <- read_csv("data/clubs.csv")   

# View(appearances)
# colnames(player_valuations)
# View(clubs)
# View(players)
# View(appearances)
# View(competitions)

# ----- Preparing data --------

serie_a_seasons <- data.frame(
  league = "IT1",
  start_date = as.Date(c(
    "2011-09-09", "2012-08-25", "2013-08-24", "2014-08-30", 
    "2015-08-22", "2016-08-20", "2017-08-19", "2018-08-18", 
    "2019-08-24", "2020-09-19", "2021-08-21", "2022-08-13", 
    "2023-08-19", "2024-08-17"  
  )),
  end_date = as.Date(c(
    "2012-08-24", "2013-08-23", "2014-08-29", "2015-08-21", 
    "2016-08-19", "2017-08-18", "2018-08-17", "2019-08-23", 
    "2020-09-18", "2021-08-20", "2022-08-12", "2023-08-18", 
    "2024-08-16", "2025-05-25"  
  )),
  season = c(
    "11/12", "12/13", "13/14", "14/15", 
    "15/16", "16/17", "17/18", "18/19", 
    "19/20", "20/21", "21/22", "22/23", 
    "23/24", "24/25"  # Season labels
  )
)

# Filter appearances using Serie A season dates
filtered_appearances <- appearances %>%
  left_join(serie_a_seasons, by = c("competition_id" = "league"), relationship = "many-to-many") %>%  # Join with season dates
  filter(date >= start_date & date <= end_date)  # Keep only rows within season dates

# Aggregate stats per season
career_stats_per_season <- filtered_appearances %>%
  group_by(player_id, season, player_club_id, competition_id, player_name) %>%  # Group by season, player, club, and competition
  summarise(
    total_red_cards = sum(red_cards, na.rm = TRUE),       # Total red cards
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE), # Total yellow cards
    total_goals = sum(goals, na.rm = TRUE),               # Total goals
    total_assists = sum(assists, na.rm = TRUE),           # Total assists
    total_minutes_played = sum(minutes_played, na.rm = TRUE),  # Total minutes played
    total_matches = n()  # Count total matches played
  ) 

# Merge career stats with club names
career_stats_with_club <- career_stats_per_season %>%
  left_join(clubs %>% select(club_id, name), by = c("player_club_id" = "club_id")) %>%
  rename(club_name = name)  # Rename 'name' column to 'club_name'

# ----- Combining stats from players and appearances dataset --------

# Extract age from DoB
players$age <- year(as.period(interval(start=players$date_of_birth, end=Sys.Date())))

# Select the required columns
players_filtered <- players %>%
  select(
    player_id,              # Include player ID for merging later
    name,                   # Player name
    country_of_citizenship, # Nationality
    age,                    # Age
    position,               # Position
    foot,                   # Preferred foot
    height_in_cm,           # Height
    sub_position,
  )

# Merge player characteristics
final_table <- career_stats_with_club %>%
  left_join(players %>% 
              select(player_id, country_of_citizenship, age, position, foot, height_in_cm, sub_position),
            by = "player_id")

# ----- Combining previous and current market value from player valuations dataset to main table  --------

# Assign seasons to player valuations based on the date
player_valuations1 <- player_valuations %>%
  mutate(season = case_when(
    date >= as.Date("2011-09-09") & date <= as.Date("2012-08-24") ~ "11/12",
    date >= as.Date("2012-08-25") & date <= as.Date("2013-08-23") ~ "12/13",
    date >= as.Date("2013-08-24") & date <= as.Date("2014-08-29") ~ "13/14",
    date >= as.Date("2014-08-30") & date <= as.Date("2015-08-21") ~ "14/15",
    date >= as.Date("2015-08-22") & date <= as.Date("2016-08-19") ~ "15/16",
    date >= as.Date("2016-08-20") & date <= as.Date("2017-08-18") ~ "16/17",
    date >= as.Date("2017-08-19") & date <= as.Date("2018-08-17") ~ "17/18",
    date >= as.Date("2018-08-18") & date <= as.Date("2019-08-23") ~ "18/19",
    date >= as.Date("2019-08-24") & date <= as.Date("2020-09-18") ~ "19/20",
    date >= as.Date("2020-09-19") & date <= as.Date("2021-08-20") ~ "20/21",
    date >= as.Date("2021-08-21") & date <= as.Date("2022-08-12") ~ "21/22",
    date >= as.Date("2022-08-13") & date <= as.Date("2023-08-18") ~ "22/23",
    date >= as.Date("2023-08-19") & date <= as.Date("2024-08-16") ~ "23/24",
    date >= as.Date("2024-08-17") & date <= as.Date("2025-05-25") ~ "24/25",
    TRUE ~ NA_character_  
  )) %>%
  arrange(player_id, season, desc(date)) %>%  # Sort by player_id, season, and descending date
  group_by(player_id, season) %>%            # Group by player and season
  slice(1) %>%                               
  ungroup()

# Extract current market values for each player and season
current_market_values <- player_valuations1 %>%
  select(player_id, season, market_value_in_eur) %>%  
  rename(current_market_value = market_value_in_eur) 

# Join current market values with the final table
final_table <- final_table %>%
  left_join(current_market_values, by = c("player_id", "season"), relationship = "many-to-many")

# ----- Saving final dataset --------

write.csv(final_table, "data/prepared_dataset.csv", row.names = FALSE)
final_table <- read_csv("data/prepared_dataset.csv")

# ----- Data Visualisation: Plot 1 --------

summary(final_table$total_red_cards)
summary(final_table$total_yellow_cards)
summary(final_table$total_goals)
summary(final_table$total_assists)
summary(final_table$age)
summary(final_table$height_in_cm)
summary(final_table$current_market_value)

final_table <- final_table %>%
  filter(position != "Missing")  # Remove rows where position is "Missing"

# Aggregate market values by position and season
market_values_by_position <- final_table %>%
  group_by(season, position) %>%
  summarise(avg_market_value = mean(current_market_value, na.rm = TRUE)) %>%
  ungroup()

head(market_values_by_position)

# Reorder the factor levels for the "position" variable
market_values_by_position$position <- factor(
  market_values_by_position$position,
  levels = c("Attack", "Midfield", "Defender", "Goalkeeper")  
)

ggplot(market_values_by_position, aes(x = season, y = avg_market_value, colour = position, group = position)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(
    title = "Evolution of Player Market Values by Position in Serie A (2012–2025)",
    x = "Season",
    y = "Average Market Value (Millions EUR)",
    colour = "Position"
  ) +
  scale_colour_manual( 
    values = c(
      "Attack" =   "#d95f02",  # Dark Orange
      "Midfield" = "#1b9e77",  # Dark Green
      "Defender" = "#7570b3",  # Dark Purple
      "Goalkeeper" = "#e6ab02" # Yellow/Gold
    )
  ) +
  theme_minimal() +         
  scale_y_continuous(
    breaks = seq(0, 15e6, by = 3e6),  # Add ticks every 3M
    labels = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)  # Format labels in millions
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12, face = "bold"), 
    axis.title.y = element_text(size = 12, face = "bold"), 
    legend.position = "bottom", 
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9),   
    panel.grid.major = element_line(colour = "gray70", size = 0.4), 
    panel.grid.minor = element_line(colour = "gray85", size = 0.2)  
  )

ggsave("Visualisation/market_values_by_position.png", width = 8, height = 5, dpi = 300)

# ----- Data Visualisation: Plot 2 --------

# Filter the data to include relevant columns and remove missing values
data <- final_table %>%
  filter(!is.na(sub_position), !is.na(current_market_value)) %>%  
  select(position, sub_position, current_market_value)    

ggplot(data, aes(y = reorder(sub_position, -current_market_value, FUN = median), 
                 x = current_market_value, 
                 fill = position)) +  # Group sub positions by position
  # Add whisker ends
  stat_boxplot(geom = "errorbar", width = 0.5, lwd = 0.4) +  
  # Add boxplot without outlier dots
  geom_boxplot(outlier.shape = NA,  
               coef = 1.5,  
               lwd = 0.6,  
               fatten = 1.1) + 
  # Log-scale x-axis for market value
  scale_x_log10(
    breaks = c(1e5, 1e6, 1e7, 1e8),  # Breaks (no 0 since log scale can't include 0)
    labels = c("100K", "1M", "10M", "100M")  
  ) +
  # Custom fill colours for positions
  scale_fill_manual(  
    values = c(
      "Attack" =   "#d95f02",  # Dark Orange
      "Midfield" = "#1b9e77",  # Dark Green
      "Defender" = "#7570b3",  # Dark Purple
      "Goalkeeper" = "#e6ab02" # Yellow/Gold
    )
  ) +
  labs(
    title = "Market Value Distribution by Sub Position in Serie A (Grouped by Position)",
    x = "Market Value (Millions EUR)",
    y = "Sub Position",
    fill = "Position"  
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    legend.position = "top", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 9), 
    panel.grid.major = element_line(colour = "gray70", size = 0.4),  
    panel.grid.minor = element_line(colour = "gray85", size = 0.2)   
  )

ggsave("Visualisation/boxplot_market_value.png", width = 8, height = 5, dpi = 300)

# ----- Data Visualisation: Plot 3 --------

# Filter the final_table for the 23/24 season
final_table_23_24 <- final_table %>%
  filter(season == "23/24")

# Filter the top 10 most valuable players
top_10_valuable_players <- final_table_23_24 %>%
  arrange(desc(current_market_value)) %>%
  slice(1:10)  

ggplot(top_10_valuable_players, aes(x = reorder(player_name, current_market_value), y = current_market_value, fill = position)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the chart to make it horizontal
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, max(top_10_valuable_players$current_market_value, na.rm = TRUE), by = 10e6),  # Add intervals every 10M
    labels = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)  # Format axis in millions (M)
  ) +
  scale_fill_manual(  
    values = c(
      "Attack" =   "#d95f02",  # Dark Orange
      "Midfield" = "#1b9e77",  # Dark Green
      "Defender" = "#7570b3",  # Dark Purple
      "Goalkeeper" = "#e6ab02" # Yellow/Gold
    )
  ) +
  labs(
    title = "Top 10 Most Valuable Players in Serie A (2023/2024)",
    x = "Player Name",
    y = "Current Market Value (Millions EUR)",  
    fill = "Position"
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
    axis.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10), 
    axis.title.x = element_text(size = 12, face = "bold"), 
    axis.title.y = element_text(size = 12, face = "bold"),  
    legend.position = "bottom", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 9),   
    panel.grid.major = element_line(colour = "gray70", size = 0.4),  
    panel.grid.minor = element_line(colour = "gray85", size = 0.2)   
  )

ggsave("Visualisation/top_10_player_market_value.png", width = 8, height = 5, dpi = 300)

# ----- Data Visualisation: Plot 4 --------

# Create scatter plot with all players in grey and top 10 highlighted
ggplot() +
  # Plot all players in grey
  geom_point(data = final_table_23_24, 
             aes(x = total_goals, y = total_assists), 
             alpha = 0.5, fill = "gray", colour = "black", size = 2.5, shape = 21, stroke = 0.3) +  
  # Highlight top 10 players with colour points
  geom_point(data = top_10_valuable_players, 
             aes(x = total_goals, y = total_assists, fill = position), 
             size = 4, alpha = 0.9, colour = "black", shape = 21, stroke = 0.8) + 
  # Add non-overlapping player labels for the top 10 players with colour text
  geom_text_repel(data = top_10_valuable_players, 
                  aes(x = total_goals, y = total_assists, label = player_name, colour = position), 
                  size = 4, fontface = "bold", 
                  box.padding = 0.3, point.padding = 0.3, 
                  show.legend = FALSE, segment.colour = NA) +  
  scale_fill_manual(
    values = c(
      "Attack" = "#d95f02",    # Dark Orange
      "Midfield" = "#1b9e77",  # Dark Green
      "Defender" = "#7570b3",  # Dark Purple
      "Goalkeeper" = "#e6ab02" # Yellow/Gold
    )
  ) +
  scale_colour_manual(
    values = c(
      "Attack" = "#d95f02",    # Dark Orange
      "Midfield" = "#1b9e77",  # Dark Green
      "Defender" = "#7570b3",  # Dark Purple
      "Goalkeeper" = "#e6ab02" # Yellow/Gold
    )
  ) +
  theme_minimal() +
  labs(
    title = "Top 10 Most Valuable Players: Goals vs Assists in Serie A (2023/2024)",
    subtitle = "Grey points represent other players in the league",  
    x = "Total Goals",
    y = "Total Assists",
    fill = "Position",  
    colour = "Position" 
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 10, hjust = 0.5),  
    axis.text.y = element_text(size = 9),  
    axis.text.x = element_text(size = 9),  
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),  
    legend.position = "bottom",  
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),  
    panel.grid.major = element_line(colour = "gray70", size = 0.4),  
    panel.grid.minor = element_line(colour = "gray85", size = 0.2) 
  )

ggsave("Visualisation/player_stats_2023_24.png", width = 8, height = 5, dpi = 300)



