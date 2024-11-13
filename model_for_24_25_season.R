library(tidyverse)
library(hoopR)
library(rvest)

#Table to count the number of possessions each team has in each game 
load_nba_pbp()%>%
  mutate(new_possession = case_when(
    type_text %in% c(
      "Out of Bounds - Step Turnover", 
      "Traveling", 
      "Offensive Goaltending Turnover", 
      "Bad Pass\nTurnover", 
      "Lost Ball Turnover", 
      "Back Court Turnover", 
      "Shot Clock Turnover", 
      "Out of Bounds - Bad Pass Turnover", 
      "3-Second Turnover", 
      "Offensive Foul Turnover", 
      "Out of Bounds - Lost Ball Turnover", 
      "Kicked Ball Turnover", 
      "Double Dribble Turnover", 
      "Disc Dribble Turnover", 
      "5-Second Turnover", 
      "Palming Turnover", 
      "Lane Violation Turnover", 
      "Too Many Players Turnover", 
      "Inbound Turnover", 
      "8-Second Turnover"
    ) ~ TRUE,
    type_text == "Defensive Rebound" ~ TRUE,
    scoring_play == TRUE & !type_text %in% c(
      "Free Throw - 1 of 2", 
      "Free Throw - 2 of 3", 
      "Free Throw - 1 of 3"
    ) ~ TRUE,
    type_text %in% c(
      "Free Throw - 2 of 2", 
      "Free Throw - 3 of 3", 
      "Free Throw - 1 of 1"
    ) ~ TRUE,
    TRUE ~ FALSE
  ))%>%
  group_by(game_id, team_id) %>%
  summarize(total_team_possessions = sum(new_possession))%>%
  filter(!is.na(team_id))%>%
  left_join(load_nba_team_box()%>%distinct(team_id,team_name),by=c("team_id"="team_id"))->count_of_possessions


#Creates a list of each athletes name and id number
load_nba_player_box()%>%
  distinct(athlete_display_name,athlete_id)->player_id_list


load_nba_player_box()%>%
  view()

load_nba_player_box() %>%
  select(c(1,7:8, 12:28,47)) %>%
  left_join(count_of_possessions,by=c("game_id"="game_id","team_id"="team_id"))%>%
  left_join(load_nba_team_box()%>%select(c(1,6,26,31,43))%>%
              rename("team_fga"="field_goals_attempted",
                     "team_fta"="free_throws_attempted",
                     "total_team_turnovers"="total_turnovers"),by=c("game_id"="game_id","team_id"="team_id"))%>%
  mutate(plus_minus = as.numeric(plus_minus),
         game_id=as.character(game_id),
         usg_rate=(100*48*(field_goals_attempted+.44*free_throws_attempted+turnovers))/(minutes*(team_fga+.44*team_fta+total_team_turnovers)),
         usg_rate_multiplier=case_when(usg_rate>21~.8,
                                     TRUE~1),
         gmsc = (points + 0.4 * field_goals_made - 0.7 * field_goals_attempted - 0.4 * (free_throws_attempted - free_throws_made) + 
      0.7 * offensive_rebounds + 0.3 * defensive_rebounds + steals + 0.7 * assists + 
      0.7 * blocks - 0.4 * fouls - turnovers)*usg_rate_multiplier,
    ) %>%
  select(-game_id,-team_id)%>%
  group_by(athlete_display_name) %>%
  mutate(avg_gmsc = mean(gmsc, na.rm = TRUE),
         gmsc_above_avg = ifelse(gmsc > avg_gmsc, 1, 0),
         gmsc_below_avg = ifelse(gmsc < avg_gmsc, 1, 0),
         win_percentage_above_avg = sum(gmsc_above_avg & team_winner, na.rm = TRUE) / sum(gmsc_above_avg, na.rm = TRUE),  
         win_percentage_below_avg = sum(gmsc_below_avg & team_winner, na.rm = TRUE) / sum(gmsc_below_avg, na.rm = TRUE),  
         gmsc_variance = win_percentage_above_avg - win_percentage_below_avg)%>% 
  summarise(across(everything(), mean, na.rm = TRUE))%>%
  filter(!is.nan(gmsc_variance))%>%
  select(-c(24,26:30))%>%
  mutate(across(-1, ~ round(.x, 3)))%>%
  inner_join(load_nba_player_box()%>%
              select(athlete_display_name,team_display_name)%>%
              distinct(athlete_display_name,team_display_name),by=c("athlete_display_name"="athlete_display_name"))%>%
  arrange(-gmsc_variance)->adj_gmsc_var_table


#'''RAMP Cleaning and prep'''

#Filtering to keep only substitutions and arrange by game, quarter and in reverse clock time
substitutions <- load_nba_pbp() %>%
  filter(type_text == "Substitution") %>%
  arrange(game_id, qtr, desc(time))

# Generate stint IDs based on substitution events
stints <- substitutions %>%
  group_by(game_id, qtr) %>%
  mutate(
    stint_id = cumsum(lag(type_text == "Substitution", default = TRUE))
  ) %>%
  ungroup()
# Identify staters so that the intial play prior to the first substitution is included
starters <- stints %>%
  pivot_longer(cols = c(athlete_id_1, athlete_id_2), 
               names_to = "sub_type", 
               values_to = "player_id") %>%
  group_by(game_id, player_id) %>%
  summarize(
    first_appearance = first(sub_type),  # Track first sub type
    .groups = "drop"
  ) %>%
  mutate(is_starter = first_appearance == "athlete_id_2")

# Assign players to each stint
stints_players <- stints %>%
  group_by(game_id, stint_id) %>%
  summarize(
    players_on_court = list(unique(c(athlete_id_1, athlete_id_2))),
    .groups = "drop"
  )

# Merge stint_id into the main play-by-play dataset
pbp_with_stints <- load_nba_pbp() %>%
  filter(type_text %in% c("Substitution", "ScoringPlay")) %>% # Include relevant play types
  left_join(stints %>% select(game_id, qtr, time, stint_id), 
            by = c("game_id", "qtr", "time"))

pbp_with_stints <- pbp_with_stints %>%
  group_by(game_id, qtr) %>%
  arrange(desc(time)) %>%
  fill(stint_id, .direction = "downup") %>% # Fill in missing stint IDs
  ungroup()

# Calculate score margins for each stint
stints_data <- pbp_with_stints %>%
  group_by(game_id, stint_id) %>%
  summarize(
    start_score = first(home_score - away_score),
    end_score = last(home_score - away_score),
    margin = end_score - start_score,
    .groups = "drop"
  )
# Expand player presence for regression
rapm_data <- stints_data %>%
  left_join(stints_players, by = c("game_id", "stint_id")) %>%
  unnest(players_on_court) %>%
  mutate(player_presence = 1) %>%
  pivot_wider(names_from = players_on_court, 
              values_from = player_presence, 
              values_fill = 0)

###################################################################



###################################################################


library(dplyr)
library(tidyr)

# Step 1: Load Play-by-Play Data and Team Info
play_by_play <- load_nba_pbp()

team_info <- load_nba_schedule() %>%
  mutate(home_team_id = as.character(home_team_id), 
         away_team_id = as.character(away_team_id),
         game_id = as.character(game_id))

# Step 2: Filter Substitutions and Pivot
substitutions <- play_by_play %>%
  filter(type_text == "Substitution") %>%
  arrange(game_id, qtr, desc(time)) %>%
  pivot_longer(cols = c(athlete_id_1, athlete_id_2), 
               names_to = "sub_type", 
               values_to = "player_id") %>%
  mutate(player_id = as.character(player_id),
         game_id = as.character(game_id))

# Step 3: Join Team Info and Assign Team Type
substitutions <- substitutions %>%
  left_join(team_info, by = "game_id") %>%
  mutate(
    team_type = case_when(
      player_id == home_team_id ~ "home",
      player_id == away_team_id ~ "away",
      TRUE ~ NA_character_
    )
  )

# Check for unmatched players
unmatched_players <- substitutions %>%
  filter(is.na(team_type)) %>%
  distinct(player_id)

if (nrow(unmatched_players) > 0) {
  print("Unmatched player IDs:")
  print(unmatched_players)
} else {
  print("All players matched correctly!")
}

# Step 4: Create stint_id
substitutions <- substitutions %>%
  group_by(game_id, qtr) %>%
  mutate(stint_id = cumsum(lag(type_text == "Substitution", default = TRUE))) %>%
  ungroup()

# Step 5: Merge stint_id into Play-by-Play
pbp_with_stints <- play_by_play %>%
  left_join(
    substitutions %>%
      group_by(game_id, qtr, time) %>%
      summarize(stint_id = max(stint_id), .groups = "drop"),
    by = c("game_id", "qtr", "time")
  ) %>%
  group_by(game_id, qtr) %>%
  arrange(desc(time)) %>%
  fill(stint_id, .direction = "downup") %>%
  ungroup()

# Step 6: Calculate Scoring Margins
stints_data <- pbp_with_stints %>%
  group_by(game_id, stint_id) %>%
  summarize(
    start_score = first(home_score - away_score),
    end_score = last(home_score - away_score),
    margin = end_score - start_score,
    .groups = "drop"
  )

# Step 7: Identify Players on Court for Each Stint
stints_players <- substitutions %>%
  group_by(game_id, stint_id) %>%
  summarize(players_on_court = list(unique(player_id)), .groups = "drop")

# Step 8: Ensure Unique Rows for Substitutions
substitutions_unique <- substitutions %>%
  distinct(game_id, player_id, team_type, .keep_all = TRUE)

# Step 9: Prepare RAPM Matrix
rapm_data <- stints_players %>%
  unnest(players_on_court) %>%
  rename(player_id = players_on_court) %>%
  left_join(substitutions_unique %>% select(game_id, player_id, team_type), 
            by = c("game_id", "player_id")) %>%
  mutate(
    player_indicator = case_when(
      team_type == "home" ~ 1,
      team_type == "away" ~ -1,
      TRUE ~ 0
    )
  ) %>%
  pivot_wider(
    names_from = player_id,
    values_from = player_indicator,
    values_fill = 0  # Players not in stint get 0
  )
