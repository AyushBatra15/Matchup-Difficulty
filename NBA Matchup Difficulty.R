rm(list=ls())
library(tidyverse)
library(tidymodels)
library(hoopR)
library(nbastatR)
Sys.setenv("VROOM_CONNECTION_SIZE" = 2*131072)

theme_bbs <- function() {
  font = 'sans'
  bg = "#E4DBD7"
  light_ln = "#A0A0A0"
  dark_ln = "#404040"
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = bg, color = NA),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.background = element_blank(),
          panel.grid = element_line(color = light_ln),
          panel.grid.minor = element_blank(),
          plot.title = element_text(family = font, size = 18, face = 'bold', hjust = 0.5, vjust = 2),
          plot.subtitle = element_text(family = font, size = 12, hjust = 0.5, vjust = 1),
          plot.caption = element_text(family = font, size = 9, hjust = 1, vjust = -5, color = dark_ln),
          axis.title.x = element_text(family = font, size = 15, vjust = -2, color = dark_ln),
          axis.title.y = element_text(family = font, size = 15,  angle = 90, vjust = 3, color = dark_ln),
          axis.text = element_text(family = font, size = 13, color = dark_ln),
          legend.title = element_text(family = font, size = 13, color = dark_ln, face = 'bold', hjust = 0.5),
          legend.text = element_text(family = font, size = 12, color = dark_ln),
          legend.box.background = element_blank(),
          axis.ticks = element_line(color = light_ln),
          plot.margin = unit(c(1,1,1,1),"cm"))
}

assign_nba_teams()
assign_nba_players()

SEASON = "2022-23"
OFF_MINIMUM = 300
DEF_MINIMUM = 1000


# NBA PLAYER OFFENSIVE STATS -----

offense <- nba_leaguedashplayerstats(season = SEASON,
                                     per_mode = "Per100Possessions",
                                     season_type = "Regular Season")
offense <- offense[["LeagueDashPlayerStats"]]

minutes <- nba_leaguedashplayerstats(season = SEASON,
                                     season_type = "Regular Season")
minutes <- minutes[["LeagueDashPlayerStats"]]

offense <- offense %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM = TEAM_ABBREVIATION, AGE, GP, FGM, FGA,
         FG3M, FG3A, FTM, FTA, OREB, AST, TOV, PFD, PTS) %>%
  mutate(across(c(AGE : PTS), as.numeric))

minutes <- minutes %>%
  select(PLAYER_ID, MIN) %>%
  mutate(MIN = round(as.numeric(MIN)))

offense <- offense %>%
  inner_join(minutes, by = "PLAYER_ID") %>%
  relocate(MIN, .after = GP) %>%
  filter(MIN >= OFF_MINIMUM)

offense <- offense %>%
  mutate(efficiency = PTS / (FGA + TOV + 0.44*FTA))

points <- offense %>%
  select(PLAYER_ID, PTS, efficiency)



# SPECIFIC TEAM: MATCHUP STATS --------

team_info <- df_dict_nba_teams %>%
  filter(idTeam <= 1610612766 & idTeam >= 1610612737) %>%
  select(nameTeam, slugTeam, idTeam, colorsTeam, urlThumbnailTeam)

myTEAM = "Los Angeles Lakers"

myTeamID <- team_info %>%
  filter(nameTeam == myTEAM) %>%
  select(idTeam) %>%
  pull() %>%
  as.character()


matchups <- nba_leagueseasonmatchups(def_team_id = myTeamID,
                                     season = SEASON)
matchups <- matchups[["SeasonMatchups"]]

matchups <- matchups %>%
  inner_join(points, by = c("OFF_PLAYER_ID" = "PLAYER_ID")) %>%
  rename(SEASON_PTS_100 = PTS)

tm_def_matchups <- matchups %>%
  mutate(across(c(PARTIAL_POSS : MATCHUP_TIME_SEC), as.numeric)) %>%
  mutate(expected_pts = efficiency * (MATCHUP_FGA + MATCHUP_TOV + 0.44*MATCHUP_FTA)) %>%
  group_by(DEF_PLAYER_ID) %>%
  summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
            POSS = sum(PARTIAL_POSS),
            avg_PTS = weighted.mean(SEASON_PTS_100, w = PARTIAL_POSS),
            simple_eff = sum(PLAYER_PTS) / (sum(MATCHUP_FGA) + 
                                              sum(MATCHUP_TOV) + 0.44*sum(MATCHUP_FTA)),
            expected = sum(expected_pts) / (sum(MATCHUP_FGA) + 
                                              sum(MATCHUP_TOV) + 0.44*sum(MATCHUP_FTA)),
            over_exp = simple_eff - expected) %>%
  ungroup() %>%
  inner_join(minutes, by = c("DEF_PLAYER_ID" = "PLAYER_ID")) %>%
  filter(MIN >= DEF_MINIMUM) %>%
  arrange(-avg_PTS)



# LOOP OVER ALL TEAMS ----------

list_IDs <- team_info %>%
  pull(idTeam) %>%
  as.character()

all <- tibble()
iter <- 0

for (id in list_IDs) {
  slug <- team_info %>%
    filter(idTeam == as.numeric(id)) %>%
    select(slugTeam) %>%
    pull()
  
  matchups <- nba_leagueseasonmatchups(def_team_id = id,
                                       season = SEASON)
  matchups <- matchups[["SeasonMatchups"]]
  
  matchups <- matchups %>%
    inner_join(points, by = c("OFF_PLAYER_ID" = "PLAYER_ID")) %>%
    rename(SEASON_PTS_100 = PTS)
  
  tm_def_matchups <- matchups %>%
    mutate(across(c(PARTIAL_POSS : MATCHUP_TIME_SEC), as.numeric)) %>%
    mutate(expected_pts = efficiency * (MATCHUP_FGA + MATCHUP_TOV + 0.44*MATCHUP_FTA)) %>%
    group_by(DEF_PLAYER_ID) %>%
    summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
              POSS = sum(PARTIAL_POSS),
              avg_PTS = weighted.mean(SEASON_PTS_100, w = PARTIAL_POSS),
              simple_eff = sum(PLAYER_PTS) / (sum(MATCHUP_FGA) + 
                                                sum(MATCHUP_TOV) + 0.44*sum(MATCHUP_FTA)),
              expected = sum(expected_pts) / (sum(MATCHUP_FGA) + 
                                                sum(MATCHUP_TOV) + 0.44*sum(MATCHUP_FTA)),
              over_exp = simple_eff - expected) %>%
    ungroup() %>%
    mutate(slugTeam = slug)
  
  all <- rbind(all, tm_def_matchups)
  
  iter <- iter + 1
  print(paste("Loaded ",iter,"/30 Teams", sep = ""))
}


matchup_diff <- all %>%
  inner_join(minutes, by = c("DEF_PLAYER_ID" = "PLAYER_ID")) %>%
  filter(MIN >= DEF_MINIMUM) %>%
  arrange(-avg_PTS)

matchup_diff <- matchup_diff %>%
  mutate(PCT_Diff = rank(avg_PTS) / nrow(matchup_diff),
         PCT_Eff = rank(-over_exp) / nrow(matchup_diff))




# FUNCTION -----

get_matchup_diff <- function(str_season, off_min, def_min) {
  
  offense <- nba_leaguedashplayerstats(season = str_season,
                                       per_mode = "Per100Possessions",
                                       season_type = "Regular Season")
  offense <- offense[["LeagueDashPlayerStats"]]
  
  minutes <- nba_leaguedashplayerstats(season = str_season,
                                       season_type = "Regular Season")
  minutes <- minutes[["LeagueDashPlayerStats"]]
  
  offense <- offense %>%
    select(PLAYER_ID, PLAYER_NAME, TEAM = TEAM_ABBREVIATION, AGE, GP, FGM, FGA,
           FG3M, FG3A, FTM, FTA, OREB, AST, TOV, PFD, PTS) %>%
    mutate(across(c(AGE : PTS), as.numeric))
  
  minutes <- minutes %>%
    select(PLAYER_ID, MIN) %>%
    mutate(MIN = round(as.numeric(MIN)))
  
  offense <- offense %>%
    inner_join(minutes, by = "PLAYER_ID") %>%
    relocate(MIN, .after = GP) %>%
    filter(MIN >= off_min)
  
  offense <- offense %>%
    mutate(efficiency = PTS / (FGA + TOV + 0.44*FTA))
  
  points <- offense %>%
    select(PLAYER_ID, PTS, efficiency)
  
  list_IDs <- team_info %>%
    pull(idTeam) %>%
    as.character()
  
  all <- tibble()
  iter <- 0
  
  for (id in list_IDs) {
    slug <- team_info %>%
      filter(idTeam == as.numeric(id)) %>%
      select(slugTeam) %>%
      pull()
    
    matchups <- nba_leagueseasonmatchups(def_team_id = id,
                                         season = str_season)
    matchups <- matchups[["SeasonMatchups"]]
    
    matchups <- matchups %>%
      inner_join(points, by = c("OFF_PLAYER_ID" = "PLAYER_ID")) %>%
      rename(SEASON_PTS_100 = PTS)
    
    tm_def_matchups <- matchups %>%
      mutate(across(c(PARTIAL_POSS : MATCHUP_TIME_SEC), as.numeric)) %>%
      mutate(expected_pts = efficiency * (MATCHUP_FGA + MATCHUP_TOV + 0.44*MATCHUP_FTA)) %>%
      group_by(DEF_PLAYER_ID) %>%
      summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
                POSS = sum(PARTIAL_POSS),
                avg_PTS = weighted.mean(SEASON_PTS_100, w = PARTIAL_POSS),
                simple_eff = sum(PLAYER_PTS) / (sum(MATCHUP_FGA) + 
                                                  sum(MATCHUP_TOV) + 0.44*sum(MATCHUP_FTA)),
                expected = sum(expected_pts) / (sum(MATCHUP_FGA) + 
                                                  sum(MATCHUP_TOV) + 0.44*sum(MATCHUP_FTA)),
                over_exp = simple_eff - expected) %>%
      ungroup() %>%
      mutate(slugTeam = slug)
    
    all <- rbind(all, tm_def_matchups)
    
    iter <- iter + 1
    print(paste("Loaded ",iter,"/30 Teams", sep = ""))
  }
  
  matchup_diff <- all %>%
    inner_join(minutes, by = c("DEF_PLAYER_ID" = "PLAYER_ID")) %>%
    filter(MIN >= def_min) %>%
    arrange(-avg_PTS)
  
  matchup_diff <- matchup_diff %>%
    mutate(PCT_Diff = rank(avg_PTS) / nrow(matchup_diff),
           PCT_Eff = rank(-over_exp) / nrow(matchup_diff))
  
  return(matchup_diff)
  
}

matchups22 <- get_matchup_diff("2021-22", 300, 1000)
matchups23 <- matchup_diff



# YEAR TO YEAR ----------

both <- matchups22 %>%
  inner_join(matchups23, by = "DEF_PLAYER_ID", suffix = c(".22",".23")) %>%
  select(-PLAYER_NAME.23) %>%
  rename(PLAYER_NAME = PLAYER_NAME.22)

both %>%
  ggplot(aes(x = avg_PTS.22, y = avg_PTS.23)) +
  geom_point() +
  geom_smooth(se = F, method = lm)

both %>%
  mutate(Same_Team = ifelse(slugTeam.22 == slugTeam.23, TRUE, FALSE)) %>%
  ggplot(aes(x = avg_PTS.22, y = avg_PTS.23)) +
  geom_point() +
  geom_smooth(se = F, method = lm) +
  facet_wrap(~Same_Team)

cor(both$avg_PTS.22, both$avg_PTS.23)
cor(both$simple_eff.22, both$simple_eff.23)


both %>%
  ggplot(aes(x = simple_eff.22, y = simple_eff.23)) +
  geom_point() +
  geom_smooth(se = F, method = lm)

both %>%
  mutate(Same_Team = ifelse(slugTeam.22 == slugTeam.23, TRUE, FALSE)) %>%
  ggplot(aes(x = simple_eff.22, y = simple_eff.23)) +
  geom_point() +
  geom_smooth(se = F, method = lm) +
  facet_wrap(~Same_Team)



# CORRELATION TO DEFENSIVE WS -------

defense <- nba_leaguedashplayerstats(measure_type = "Defense",
                                     season = SEASON)
defense <- defense[["LeagueDashPlayerStats"]]

defense <- defense %>%
  mutate(across(c(AGE : DEF_WS), as.numeric)) %>%
  mutate(MIN = round(MIN),
         STL_48 = STL / MIN * 48,
         BLK_48 = BLK / MIN * 48,
         DWS_48 = DEF_WS / MIN * 48) %>%
  select(PLAYER_ID, AGE, STL_48, BLK_48, DWS_48)

defense <- defense %>%
  inner_join(matchup_diff, by = c("PLAYER_ID" = "DEF_PLAYER_ID"))

defense %>%
  ggplot(aes(x = STL_48, y = avg_PTS)) +
  geom_point() +
  geom_smooth(se = F, method = lm)

defense %>%
  ggplot(aes(x = BLK_48, y = avg_PTS)) +
  geom_point() +
  geom_smooth(se = F, method = lm)

defense %>%
  ggplot(aes(x = DWS_48, y = avg_PTS)) +
  geom_point() +
  geom_smooth(se = F, method = lm)  




# CORRELATION WITH RIM PROTECTION ------- 

rim_def <- nba_leaguedashptdefend(season = SEASON,
                                  defense_category = "Less+Than+6Ft")
rim_def <- rim_def[["LeagueDashPTDefend"]]


  
