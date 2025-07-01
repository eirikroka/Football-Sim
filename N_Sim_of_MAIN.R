# 1. Load Libraries ####
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(viridis)
library(elo)
library(purrr)
library(stringr)
library(doParallel)
library(tictoc)
library(knitr)
library(patchwork)

# 2. Define Functions ####

source("functions/simulate_goals_bias_knock.r")

source("functions/simulate_match.r")

source("functions/generate_schedule.r")

# Choose 1, the rest is commented ------------------------------

source("functions/run_simulation_tournament.r")

# 3. Set Global Variables ####

# This is very slow if the number written is big
n_sim <- 1

# 5. Read data and Simulate  ------------------------------

# Only use this one if there is data from the file
source("pre_work_data/pass_europa.r")

source("pre_work_data/tilt_data.r")

club_data <- club_data[sample(nrow(club_data)), ]

library(dplyr)
library(future)
library(purrr)
library(tictoc)
library(furrr)

# Detect the number of cores and set up the parallel plan
n_chunks <- detectCores() - 1
plan(multisession, workers = n_chunks)

# Define the simulation function
run_simulation <- function(i) {
  result <- run_simulation_tournament(club_data)
  names(result) <- paste0(names(result), "_", i)
  result
}

# Split the tasks into chunks
chunks <- split(1:n_sim, cut(1:n_sim, n_chunks))

# Run the simulations in parallel
tic()
sim_results <- future_map(chunks, ~ {
  map(.x, run_simulation)
}) %>% 
  flatten()
toc()

# Stop the parallel backend
plan(sequential)


# Initialize an empty list to store champions

all_champions <- vector("list", n_sim)
# Iterate through each element in sim_results
for (n in seq_len(n_sim)) {
  key_name <- paste0("champion_", n)
  # Extract the champion_n attribute and store it in all_champions
  all_champions[[n]] <- sim_results[[n]][[key_name]]
}

# Convert the list to a tibble with a column named 'winner'
wins <- tibble(Club = unlist(all_champions)) %>%
  count(Club) %>%
  arrange(desc(n)) %>%
  rename(Wins = n) %>% 
  print()

wins %>%
  left_join(club_data) %>%
  group_by(Group, Pass) %>%
  summarise(Group_wins = sum(Wins)) %>% 
  mutate(Average_wins = Group_wins/Pass) %>% 
  arrange(desc(Group_wins))






