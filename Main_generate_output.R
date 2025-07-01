# 1. Load Libraries ####
library(tidyverse)
library(tictoc)
library(knitr)
library(patchwork)

# 2. Define Functions ####

source("functions/simulate_goals_bias_knock.r")

source("functions/simulate_match.r")

source("functions/generate_schedule.r")

source("functions/run_simulation_tournament.r")

source("functions/europa_league_knockout.r")

source('functions/UL_knockout.r')

# 3. Read data and Simulate  ------------------------------
europa_list <- list()
el_results_list <- list()
cl_results_list <- list()
ul_results_list <- list()

for (i in 1:100){
  set.seed(i)
  
  # This script contains some randomness, and it contains the number of pots for each league to have 
  # in the 'Champions League' and the 'Europa League'
  source("pre_work_data/pass_europa.r")
  
  source("pre_work_data/tilt_data.r")
  
  club_data <- club_data[sample(nrow(club_data)), ]
  
  
  # This simulates the league table for each club
  # and have a knockout competition like the champions league
  sim_results <- run_simulation_tournament(club_data)
  
  # Storing all different leagues in one tibble
  result_tables <- sim_results$group_tables %>%
    map(as_tibble) %>%
    bind_rows() %>%
    left_join(club_data) %>%
    select(-Pass, -Elo, -Tilt) %>%
    group_by(Group) %>%
    mutate(
      n_teams = n(),
      Rank = row_number(-Points)
    ) %>%
    ungroup()
  
  # Joing the result table with the 'club_data', which was used to simulate the leagues 
  europa_data <- result_tables %>%
    left_join(club_data)
  
  # Simulates 'Europa League'
  el_sim_result <- europa_league_knockout(europa_data)
  
  el_results <- el_sim_result$results %>%
    as_tibble() %>%
    select(-Points_A, -Points_B, -Stage) %>%
    mutate(Penalty = if_else(
      (Result_A %% 1 != 0) | (Result_B %% 1 != 0),
      "Penalty",
      ""
    ))
  
  # Simulates 'Conference League'
  ul_sim_result <- UL_knockout(europa_data)
  
  ul_results <- ul_sim_result$results %>%
    as_tibble() %>%
    select(-Points_A, -Points_B, -Stage) %>%
    mutate(Penalty = if_else(
      (Result_A %% 1 != 0) | (Result_B %% 1 != 0),
      "Penalty",
      ""
    ))
  
  cl_results <- sim_results$results %>%
    filter(Stage == "Knockout") %>%
    select(-Points_A, -Points_B) %>%
    mutate(Penalty = if_else(
      (Result_A %% 1 != 0) | (Result_B %% 1 != 0),
      "Penalty",
      ""
    ))
  
  europa_list[[i]] <- europa_data %>% mutate(seed = i)
  el_results_list[[i]] <- el_results %>% mutate(seed = i)
  cl_results_list[[i]] <- cl_results %>% mutate(seed = i)
  ul_results_list[[i]] <- ul_results %>% mutate(seed = i)
  

}
  
all_europa_data <- bind_rows(europa_list)
all_el_results <- bind_rows(el_results_list)
all_cl_results <- bind_rows(cl_results_list)
all_ul_results <- bind_rows(ul_results_list)

all_europa_data %>% 
  filter(Group == 'ESP') %>% 
  group_by(Club) %>% 
  summarise(m = mean(Points)) %>% 
  arrange(desc(m))

write.csv(all_europa_data, file = "output_for_dash/set_seed/league_results.csv", row.names = FALSE)
write.csv(all_el_results, file = "output_for_dash/set_seed/el_results.csv", row.names = FALSE)
write.csv(all_cl_results, file = "output_for_dash/set_seed/cl_results.csv", row.names = FALSE)
write.csv(all_ul_results, file = "output_for_dash/set_seed/ul_results.csv", row.names = FALSE)



