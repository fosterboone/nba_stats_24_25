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


load_nba_team_box()%>%
  select(team_display_name)%>%
  group_by(team_display_name)%>%
  count()%>%
  rename(total_team_games_played="n")->total_games_played_by_team


load_nba_player_box()%>%
  select(athlete_display_name)%>%
  group_by(athlete_display_name)%>%
  count()%>%
  rename(total_player_games_played="n")->total_games_played_by_player

adj_gmsc_var_table%>%
  select(athlete_display_name,team_display_name,gmsc_variance)%>%
  left_join(total_games_played_by_player,by=c("athlete_display_name"="athlete_display_name"))%>%
  left_join(total_games_played_by_team,by=c("team_display_name"="team_display_name"))%>%
  mutate(player_playing_rate=total_player_games_played/total_team_games_played)%>%
  filter(player_playing_rate>=.6)%>%
  select(-c(4:6))%>%
  view()






