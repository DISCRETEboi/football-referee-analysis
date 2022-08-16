###

library(stringr)
#library(dplyr)
#library(ggplot2)
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
  readr::read_csv(str_c("premier-league-data/", data_path)) #mod
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
  #data_list[[i]] <- na.omit(data_list[[i]]) #mod
}

agg_data <- data_list[[1]]
for (i in data_list[-1]) {
  agg_data <- rbind(agg_data, i)
}



































