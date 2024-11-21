library(tidyverse)
library(hoopR)
library(rvest)
library(ggplot2)

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
load_nba_pbp()%>%
  head(50)%>%
  view()


poss_vec<-count_of_possessions$total_team_possessions
poss_diff<-c()

for(i in c(1:length(poss_vec))){
  if(i%%2==1){
    poss_diff<-c(poss_diff,-1*(poss_vec[i]-(poss_vec[i+1])))
  }
  else{
    poss_diff<-c(poss_diff,poss_vec[i-1]-(poss_vec[i]))
  }
}
count_of_possessions%>%
  cbind(tibble(poss_diff))%>%
  left_join(load_nba_team_box()%>%select(game_id,team_id,team_home_away),by=c("team_id"="team_id","game_id"="game_id"))%>%
  mutate(total_opp_poss=total_team_possessions+poss_diff,
         team_home=case_when(team_home_away=="home"~1,
                             TRUE~0))%>%
  select(-poss_diff,-team_home_away)->count_of_possessions

#Creates a list of each athletes name and id number
load_nba_player_box()%>%
  distinct(athlete_display_name,athlete_id)->player_id_list


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
         usg_rate_multiplier=case_when(usg_rate>21~.6,
                                     TRUE~1),
         gmsc = (points + 0.4 * field_goals_made - 0.7 * field_goals_attempted - 0.4 * (free_throws_attempted - free_throws_made) + 
      0.7 * offensive_rebounds + 0.3 * defensive_rebounds + steals + 0.7 * assists + 
      0.7 * blocks - 0.4 * fouls - turnovers),
    ) %>%
  select(-game_id,-team_id)%>%
  group_by(athlete_display_name) %>%
  mutate(avg_gmsc = mean(gmsc, na.rm = TRUE),
         gmsc_above_avg = ifelse(gmsc > avg_gmsc, 1, 0),
         gmsc_below_avg = ifelse(gmsc < avg_gmsc, 1, 0),
         win_percentage_above_avg = sum(gmsc_above_avg & team_winner, na.rm = TRUE) / sum(gmsc_above_avg, na.rm = TRUE),  
         win_percentage_below_avg = sum(gmsc_below_avg & team_winner, na.rm = TRUE) / sum(gmsc_below_avg, na.rm = TRUE),  
         gmsc_variance = win_percentage_above_avg - win_percentage_below_avg)->pre_adj_gmsc_var 
pre_adj_gmsc_var%>%
  summarise(across(everything(), mean, na.rm = TRUE))%>%
  filter(!is.nan(gmsc_variance))%>%
  select(-c(24,26:30))%>%
  mutate(across(-1, ~ round(.x, 3)))%>%
  inner_join(load_nba_player_box()%>%
              select(athlete_display_name,team_display_name)%>%
              distinct(athlete_display_name,team_display_name),by=c("athlete_display_name"="athlete_display_name"))%>%
  arrange(-gmsc_variance)%>%
  select(athlete_display_name,team_display_name,gmsc_variance,everything())->adj_gmsc_var_table_usg_mult

load_nba_player_box()%>%
  view()
  




  
#Lebron James:
#On 13 games played a linear model points~total_team_possesion

pre_adj_gmsc_var%>%
  group_by(team_name)%>%
  summarise(med_team_poss=median(total_team_possessions),
            med_opp_poss=median(total_opp_poss))%>%
  view()


pre_adj_gmsc_var%>%
  filter(athlete_display_name=="LeBron James",
         points<=35)%>%
  ggplot(aes(total_team_possessions,points))+
  geom_point()+
  geom_smooth(method = "lm")


lm(data=pre_adj_gmsc_var%>%filter(athlete_display_name=="LeBron James",points<=35),formula = points~total_team_possessions)->lbj_mod

summary(lbj_mod)


####

pre_adj_gmsc_var%>%filter(athlete_display_name=="Lauri Markkanen")%>%
  ggplot(aes(total_team_possessions,points))+
  geom_point()+
  geom_smooth(se=FALSE,method = "lm")+
  theme_bw()

lm(data=pre_adj_gmsc_var%>%filter(athlete_display_name=="Lauri Markkanen"),formula = points~total_team_possessions)->lm_mod

summary(lm_mod)

###

pre_adj_gmsc_var%>%filter(athlete_display_name=="Keyonte George",
                          total_team_possessions<120)%>%
  ggplot(aes(total_team_possessions,points))+
  geom_point()+
  geom_smooth(se=FALSE)+
  theme_bw()

pre_adj_gmsc_var%>%filter(athlete_display_name=="Keyonte George",
                          total_team_possessions<120)%>%
  mutate(poss_sq=total_team_possessions^2)%>%
  lm(formula=points~total_team_possessions+poss_sq)%>%
  summary()

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
load_nba_schedule()%>%
  head()%>%
  view()

library(dplyr)
library(tidyr)

# Step 1: Load Play-by-Play Data and Team Info
play_by_play <- load_nba_pbp() %>%
  mutate(game_id = as.character(game_id))

team_info <- load_nba_schedule() %>%
  mutate(
    home_id = as.character(home_id), 
    away_id = as.character(away_id), 
    game_id = as.character(game_id)
  )

# Step 2: Filter Substitutions and Pivot

substitutions<- play_by_play%>%
  filter(type_text=="Substitution")%>%
  arrange(game_id, qtr, desc(time)) %>%
  pivot_longer(cols = c(athlete_id_1, athlete_id_2), 
               names_to = "sub_type", 
               values_to = "player_id") %>%
  mutate(player_id = as.character(player_id))


# Step 3: Join Team Info and Assign Team Type
substitutions <- substitutions %>%
  left_join(team_info, by = "game_id") %>%
  head(50)%>%
  view()
  mutate(
    team_type = case_when(
      player_id %in% play_by_play %>% 
        filter(game_id == !!game_id) %>%
        pull(home_id) ~ "home",
      player_id %in% play_by_play %>% 
        filter(game_id == !!game_id) %>%
        pull(away_id) ~ "away",
      TRUE ~ "bench"
    )
  )


# Step 4: Create stint_id


# Step 5: Merge stint_id into Play-by-Play


# Step 6: Calculate Scoring Margins


# Step 7: Identify Players on Court for Each Stint


# Step 8: Ensure Unique Rows for Substitutions


# Step 9: Correct RAPM matrix creation
rapm_data <- stints_players %>%
  unnest(players_on_court) %>%
  rename(player_id = players_on_court) %>%
  left_join(
    substitutions_unique %>% select(game_id, player_id, team_type), 
    by = c("game_id", "player_id")
  ) %>%
  mutate(
    player_indicator = case_when(
      team_type == "home" ~ 1,
      team_type == "away" ~ -1,
      TRUE ~ 0  # Players not involved in this stint
    )
  ) %>%
  pivot_wider(
    names_from = player_id,
    values_from = player_indicator,
    values_fill = 0  # Ensure non-playing players get 0
  )

