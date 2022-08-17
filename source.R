###

library(stringr)
library(dplyr)
library(ggplot2)
#library(magrittr)

### data collection
season_initial <- 0:22 %>% str_pad(width = 2, side = "left", pad = "0")
season_final <- (as.numeric(season_initial) + 1) %>% str_pad(width = 2, side = "left", pad = "0")
seasons <- list(initial = season_initial, final = season_final)

get_url <- function(season_initial, season_final) {
  data_url <- str_c("https://www.football-data.co.uk/mmz4281/", season_initial, season_final, "/E0.csv")
  data_url
}
get_data <- function(data_url, season_initial, season_final) {
  status <- download.file(data_url,
                          destfile = str_c("premier-league-data/season", season_initial, "-", season_final, ".csv"),
                          method = "curl")
  if (status == 0) {
    message(str_c("The download of dataset for the season ", season_initial, "-", season_final, " was successful!"))
  }
}
read_data <- function(data_path) {
  readr::read_csv(str_c("premier-league-data/", data_path), show_col_types = F)
}

urls <- mapply(get_url, season_initial, season_final)
invisible(mapply(get_data, urls, season_initial, season_final))

### data wrangling
data_files <- list.files("premier-league-data")
data_list <- lapply(data_files, read_data)
names(data_list) <- str_extract(data_files, "[0-9]+-[0-9]+")

common_fields <- names(data_list[[1]])
for (i in data_list[-1]) {
  data_fields <- names(i)
  common_fields <- base::intersect(common_fields, data_fields)
}

for (i in seq_along(data_list)) {
  data_list[[i]] <- data_list[[i]][, common_fields[-1]]
  season <- rep(names(data_list)[i], nrow(data_list[[i]]))
  data_list[[i]] <- cbind(season, data_list[[i]])
  data_list[[i]] <- data_list[[i]][, 1:23]
  data_list[[i]] <- na.omit(data_list[[i]])
}

agg_data <- data_list[[1]]
for (i in data_list[-1]) {
  agg_data <- rbind(agg_data, i)
}

write.csv(agg_data, "aggregated-data.csv", row.names = F)

for (i in 1:nrow(agg_data)) {
  if (agg_data$FTR[i] == "H") {
    agg_data$FTR[i] <- agg_data$HomeTeam[i]
  } else if (agg_data$FTR[i] == "D") {
    agg_data$FTR[i] <- "Draw"
  } else if (agg_data$FTR[i] == "A") {
    agg_data$FTR[i] <- agg_data$AwayTeam[i]
  }
}
for (i in 1:nrow(agg_data)) {
  if (agg_data$HTR[i] == "H") {
    agg_data$HTR[i] <- agg_data$HomeTeam[i]
  } else if (agg_data$HTR[i] == "D") {
    agg_data$HTR[i] <- "Draw"
  } else if (agg_data$HTR[i] == "A") {
    agg_data$HTR[i] <- agg_data$AwayTeam[i]
  }
}

agg_data <- rename(agg_data,
                   date = Date,
                   home_team = HomeTeam,
                   away_team = AwayTeam,
                   full_time_home_goals = FTHG,
                   full_time_away_goals = FTAG,
                   full_time_result = FTR,
                   half_time_home_goals = HTHG,
                   half_time_away_goals = HTAG,
                   half_time_result = HTR,
                   referee = Referee,
                   home_shots = HS,
                   away_shots = AS,
                   home_shots_on_target = HST,
                   away_shots_on_target = AST,
                   home_corner_kicks = HC,
                   away_corner_kicks = AC,
                   home_fouls = HF,
                   away_fouls = AF,
                   home_yellow_cards = HY,
                   away_yellow_cards = AY,
                   home_red_cards = HR,
                   away_red_cards = AR)

### data analysis
chelsea <- filter(agg_data, (home_team == "Chelsea" | away_team == "Chelsea")) %>%
  mutate(opponent = str_c(home_team, away_team)%>%str_replace("Chelsea", ""))
a_taylor <- filter(agg_data, referee == "A Taylor")
chelsea_taylor <- filter(chelsea, referee == "A Taylor")
arsenal <- filter(agg_data, (home_team == "Arsenal" | away_team == "Arsenal"))
m_dean <- filter(agg_data, referee == "M Dean") #err

chelsea_refs <- group_by(chelsea, referee) %>%
  summarise(no_of_matches = length(referee),
            win_pct = round(sum(full_time_result=="Chelsea") / length(full_time_result), 2),
            favorite_opp = names(which.max(table(opponent))),
            no_of_opp_matches = table(opponent)[which.max(table(opponent))],
            no_of_red_cards_pm = round(sum(home_red_cards, away_red_cards) / length(full_time_result), 2)) %>%
  filter(no_of_matches >= 10) %>%
  arrange(desc(win_pct))
ggplot(chelsea_refs, aes(x = reorder(referee, no_of_matches), y = no_of_matches)) +
  geom_col(fill = ifelse(chelsea_refs$referee == "A Taylor", "red", "blue")) +
  coord_flip()
ggplot(chelsea_refs, aes(x = reorder(referee, no_of_red_cards_pm), y = no_of_red_cards_pm)) +
  geom_col(fill = ifelse(chelsea_refs$referee == "A Taylor", "red", "blue")) +
  coord_flip()
ggplot(chelsea_refs, aes(x = reorder(referee, win_pct), y = win_pct)) +
  geom_col(fill = ifelse(chelsea_refs$referee == "A Taylor", "red", "blue")) +
  coord_flip()
ggplot(chelsea_refs, aes(x = no_of_matches, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(label = chelsea_refs$referee)
with(chelsea_refs, cor(x = no_of_matches, y = win_pct))
ggplot(chelsea_taylor, aes(x = opponent)) +
  geom_bar() +
  coord_flip()

















