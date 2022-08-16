###

library(stringr)
#library(dplyr)
#library(ggplot2)
#library(magrittr)

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

urls <- mapply(get_url, season_initial, season_final)
invisible(mapply(get_data, urls, season_initial, season_final))













