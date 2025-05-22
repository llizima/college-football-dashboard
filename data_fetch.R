# data_fetch.R

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(skimr)
library(forcats)
if (!require("tidygeocoder")) install.packages("tidygeocoder")
library(tidygeocoder)
if (!requireNamespace("cfbfastR", quietly = TRUE)) install.packages("cfbfastR")
library(cfbfastR)

# Get your API key from environment variabl
api_key <- Sys.getenv("CFB_API_KEY")

# Set the API key for cfbfastR
Sys.setenv(CFBD_API_KEY = "r8PdLLFaXvZshtdnzaDNfyrQlW5De5HRFGyi8+rlHenWbxlJRvnrZQ7YIC91X567")

# Register with cfbfastR
# register_cfbd(Sys.getenv("CFBD_API_KEY"))

# Get team info from cfbfastR (FBS/FCS, includes stadium lat/lon)
cfb_teams <- cfbd_team_info()

# 2. Teams
teams <- fromJSON(content(
  GET("https://api.collegefootballdata.com/teams?division=fbs",
      add_headers(Authorization = paste("Bearer", api_key))
  ), as = "text"))

# Remove 'id' from location to avoid duplication
location_nodup <- teams$location[, !(names(teams$location) %in% "id")]

# Combine teams and location by columns
teams_combined <- cbind(teams, location_nodup)

# Now select desired columns
teams_clean <- teams_combined %>%
  select(school, conference, division, classification, latitude, longitude, everything())

# Create an address column for all teams
teams_clean <- teams_clean %>%
  mutate(address = paste(school, city, state, sep = ", "))

# Path to geocoded file
geocoded_path <- "data/teams_geocoded.csv"

# If you have a previous geocoded file, load it and update only missing
if (file.exists(geocoded_path)) {
  teams_clean <- read.csv(geocoded_path, stringsAsFactors = FALSE)
  # If address column is missing (from old file), re-create it
  if (!"address" %in% names(teams_clean)) {
    teams_clean <- teams_clean %>%
      mutate(address = paste(school, city, state, sep = ", "))
  }
}

# Only geocode teams missing lat/lon, and remove duplicates
to_geocode <- teams_clean %>% filter(is.na(latitude) | is.na(longitude)) %>%
  distinct(school, .keep_all = TRUE)

if (nrow(to_geocode) > 0) {
  message("Geocoding ", nrow(to_geocode), " teams using ArcGIS...")
  geocoded <- to_geocode %>%
    geocode(address, method = "arcgis", lat = latitude_gc, long = longitude_gc)
  
  # Remove duplicates
  geocoded <- geocoded %>% distinct(school, .keep_all = TRUE)
  teams_clean <- teams_clean %>% distinct(school, .keep_all = TRUE)
  
  # Join and update lat/lon
  teams_clean <- teams_clean %>%
    left_join(geocoded %>% select(school, latitude_gc, longitude_gc), by = "school") %>%
    mutate(
      latitude = ifelse(is.na(latitude), latitude_gc, latitude),
      longitude = ifelse(is.na(longitude), longitude_gc, longitude)
    ) %>%
    select(-latitude_gc, -longitude_gc)
  
  # Remove list columns
  is_list_col <- sapply(teams_clean, is.list)
  teams_clean <- teams_clean[ , !is_list_col]
  
  # Save updated results
  write.csv(teams_clean, geocoded_path, row.names = FALSE)
} else {
  message("No teams need geocoding. Using existing geocoded data.")
}

teams_clean_flat <- teams_clean %>%
  select(where(~ !is.list(.)))

# 3. Team Talent (2015–2024)
all_years_talent <- list()
for(yr in 2015:2024){
  res <- GET(
    url = "https://api.collegefootballdata.com/talent",
    query = list(year = yr),
    add_headers(Authorization = paste("Bearer", api_key))
  )
   # Sys.sleep(1)  # Pause for 1 second between requests
  year_data <- fromJSON(content(res, as = "text"), flatten = TRUE)
  year_data$year = yr
  all_years_talent[[as.character(yr)]] <- year_data
}
talent_data <- bind_rows(all_years_talent)

# 4. Rankings (2015–2024)
all_years_rankings <- list()
for(yr in 2015:2024){
  res <- GET(
    url = "https://api.collegefootballdata.com/rankings",
    query = list(year = yr),
    add_headers(Authorization = paste("Bearer", api_key))
  )
 # Sys.sleep(1)  # Pause for 1 second between requests
  year_data <- fromJSON(content(res, as = "text"), flatten = TRUE)
  year_data$year = yr
  all_years_rankings[[as.character(yr)]] <- year_data
}
rankings <- bind_rows(all_years_rankings)

rankings_flat <- fully_unnest(rankings)

# 5. SP+ Ratings (2015–2024)
all_years_sp <- list()
for(yr in 2015:2024){
  res <- GET(
    url = "https://api.collegefootballdata.com/ratings/sp",
    query = list(year = yr),
    add_headers(Authorization = paste("Bearer", api_key))
  )
  Sys.sleep(1)  # Pause for 1 second between requests
  year_data <- fromJSON(content(res, as = "text"), flatten = TRUE)
  year_data$year = yr
  all_years_sp[[as.character(yr)]] <- year_data
}
sp_ratings <- bind_rows(all_years_sp)

# 6. Games (2015–2024)
all_years_games <- list()
for(yr in 2015:2024){
  res <- GET(
    url = "https://api.collegefootballdata.com/games",
    query = list(year = yr, seasonType = "both"),
    add_headers(Authorization = paste("Bearer", api_key))
  )
  Sys.sleep(1)  # Pause for 1 second between requests
  year_data <- fromJSON(content(res, as = "text"), flatten = TRUE)
  year_data$year = yr
  # Only keep non-list, non-data.frame columns
  if (nrow(year_data) > 0) {
    year_data <- year_data %>%
      select(where(~ !is.list(.) && !is.data.frame(.)))
    all_years_games[[as.character(yr)]] <- year_data
  }
}
games_data <- bind_rows(all_years_games)
games_data_flat <- fully_unnest(games_data)  

# 7. Recruiting (2015–2024)
all_years_recruiting <- list()
for(yr in 2015:2024){
  res <- GET(
    url = "https://api.collegefootballdata.com/recruiting/players",
    query = list(year = yr),
    add_headers(Authorization = paste("Bearer", api_key))
  )
 # Sys.sleep(1)  # Pause for 1 second between requests
  year_data <- fromJSON(content(res, as = "text"), flatten = TRUE)
  year_data$year = yr
  all_years_recruiting[[as.character(yr)]] <- year_data
}
recruiting_data <- bind_rows(all_years_recruiting)
recruiting_data_flat <- fully_unnest(recruiting_data)

# Clean and save all main datasets
clean_and_save <- function(df, name) {
  df <- drop_all_na_columns(df)
  df <- drop_sparse_columns(df, threshold = 0.5)
  df <- add_missing_indicators(df)
  df <- handle_categorical(df)
  df <- handle_continuous(df)
  write.csv(df, paste0("data/", name, ".csv"), row.names = FALSE)
}

# Clean and save games, recruiting, sp_ratings, rankings, talent_data, teams, conferences
clean_and_save(games_data_flat, "games")
clean_and_save(recruiting_data_flat, "recruiting")
clean_and_save(sp_ratings, "sp_ratings")
clean_and_save(rankings_flat, "rankings")
clean_and_save(talent_data, "talent_data")
# clean_and_save(teams_clean, "teams")

teams_clean <- read.csv("data/teams_geocoded.csv", stringsAsFactors = FALSE)

# Now games_data is ready for plotting and analysis!

sp <- read.csv("data/sp_ratings.csv")
talent <- read.csv("data/talent_data.csv")
games <- read.csv("data/games.csv")






