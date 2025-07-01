library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(fs)
library(fuzzyjoin)

# Define the folder containing the CSV files
folder_path <- "Tilt"

# List all CSV files in the folder
file_paths <- dir_ls(folder_path, regexp = "\\.csv$")

# Function to process a single CSV file
process_file <- function(file_path) {
  read_csv(file_path) %>%
    # Select relevant columns and calculate total goals
    select(Div, HomeTeam, AwayTeam, FTHG, FTAG) %>%
    mutate(Goals = FTHG + FTAG) %>%
    # Calculate average goals for home teams
    group_by(Div, HomeTeam) %>%
    mutate(AvgHomeGoals = mean(Goals)) %>%
    ungroup() %>%
    # Calculate average goals for away teams
    group_by(Div, AwayTeam) %>%
    mutate(AvgAwayGoals = mean(Goals)) %>%
    ungroup() %>%
    # Pivot data to long format
    pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "Location", values_to = "Club") %>%
    # Assign average goals based on location
    mutate(AvgGoals = ifelse(Location == "HomeTeam", AvgHomeGoals, AvgAwayGoals)) %>%
    # Select relevant columns and remove duplicates
    select(Div, Club, AvgGoals) %>%
    distinct() %>%
    # Calculate the Tilt for each club by Division
    group_by(Div, Club) %>%
    summarise(Tilt = mean(AvgGoals) / 2, .groups = "drop")
}

# Process all files and combine results
tilt_data <- file_paths %>%
  map_dfr(process_file)

write.csv(tilt_data, "tilt_data.csv")
