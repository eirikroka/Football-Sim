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
library(furrr)

# 2. Define Functions ####

source("functions/simulate_goals_bias_knock.r")

source("functions/simulate_match.r")

# Choose 1, the rest is commented ------------------------------
run_simulation_tournament <- function(club_data) {
  # Initialize a data frame to store results
  results <- data.frame(
    Stage = character(0),
    Round = integer(0),
    Club_A = character(0),
    Club_B = character(0),
    Result_A = numeric(0),
    Result_B = numeric(0),
    Winner = character(0),
    Points_A = numeric(0),
    Points_B = numeric(0),
    Elo_A = numeric(0),
    Elo_B = numeric(0)
  )

  # Initialize a list to store group tables
  group_tables <- list()

  # Get unique group identifiers
  groups <- unique(club_data$Group)

  # Initialize a list to store remaining teams
  remaining_teams <- c()

  for (group in groups) {
    # Get the teams in this group
    group_teams <- club_data$Club[club_data$Group == group]

    # Ensure that the group has more than 2 teams
    if (length(group_teams) <= 2) {
      stop(paste("Group", group, "has less than 3 teams. Please ensure each group has more than 2 teams."))
    }

    group_size <- length(group_teams)



    # Calculate total number of rounds for a group
    total_rounds <- 2 * (group_size - 1)

    # Create a matrix to hold the schedule
    schedule <- matrix(nrow = total_rounds, ncol = group_size)

    # Round-robin scheduling
    for (round in 1:(group_size - 1)) {
      schedule[round, ] <- c(group_teams[1], group_teams[-1][(0:(group_size - 2) + round) %% (group_size - 1) + 1])
      schedule[round + group_size - 1, ] <- c(schedule[round, 1], rev(schedule[round, -1]))
    }

    # Run the matches for each round
    for (round in 1:total_rounds) {
      for (i in 1:(group_size / 2)) {
        club1 <- schedule[round, i]
        club2 <- schedule[round, group_size - i + 1]

        # If round > group_size - 1, swap home and away teams
        if (round > group_size - 1) {
          tmp <- club1
          club1 <- club2
          club2 <- tmp
        }

        match_result <- simulate_match(club1, club2)


        match_result$Winner <- ifelse(match_result$Result_A > match_result$Result_B, club1,
          ifelse(match_result$Result_A < match_result$Result_B, club2, "Draw")
        )
        match_result$Points_A <- ifelse(match_result$Result_A > match_result$Result_B, 3,
          ifelse(match_result$Result_A < match_result$Result_B, 0, 1)
        )
        match_result$Points_B <- ifelse(match_result$Result_A < match_result$Result_B, 3,
          ifelse(match_result$Result_A > match_result$Result_B, 0, 1)
        )

        match_result$Elo_A <- club_data$Elo[match_result$Club_A == club_data$Club]
        match_result$Elo_B <- club_data$Elo[match_result$Club_B == club_data$Club]

        match_result$Round <- i
        match_result$Stage <- paste("Group", group)
        results <- rbind(results, match_result)
      }
    }






    group_results <- results[results$Stage == paste("Group", group), ]

    total_points <- tapply(c(group_results$Points_A, group_results$Points_B),
      c(group_results$Club_A, group_results$Club_B),
      FUN = sum
    )

    goals_for <- tapply(c(group_results$Result_A, group_results$Result_B),
      c(group_results$Club_A, group_results$Club_B),
      FUN = sum
    )

    goals_against <- tapply(c(group_results$Result_B, group_results$Result_A),
      c(group_results$Club_A, group_results$Club_B),
      FUN = sum
    )

    goal_difference <- goals_for - goals_against

    group_table <- data.frame(
      Club = names(total_points),
      Points = total_points,
      GF = goals_for,
      GA = goals_against,
      GD = goal_difference
    )
    group_table <- group_table[order(-group_table$Points, -group_table$GD, -group_table$GF), ]
    group_tables[[paste(group)]] <- group_table
  }

  list(
    group_tables = group_tables
  )
}


# 5. Read data and Simulate  ------------------------------
source("pre_work_data/pass_europa.r")

source("pre_work_data/tilt_data.r")




club_data <- club_data[sample(nrow(club_data)), ]

tic()
sim_results <- run_simulation_tournament(club_data)
toc()

sim_results$group_tables %>%
  map(as_tibble)


# As Parallel ------------------------------
source("pre_work_data/pass_europa.r")

source("pre_work_data/tilt_data.r")


group <- "TUR"
n_sim <- 400

club_data <- club_data %>%
  filter(Group == group)

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

all_results <- map_dfr(
  seq_along(sim_results),
  ~ {
    # Extract the second element, which is a list with a data frame
    df <- sim_results[[.x]][[2]][[1]] # Extract the data frame inside the list

    # Check if df is a data frame
    if (is.data.frame(df)) {
      df %>% mutate(n = .x) # Add the n column
    } else {
      tibble() # Return an empty tibble if it's not a data frame
    }
  }
) %>%
  as_tibble()


# Add Rank based on Points within each n group
all_results <- all_results %>%
  group_by(n) %>% # Group by n
  mutate(Rank = row_number()) %>% # Create the Rank column
  ungroup() # Ungroup the data to remove the grouping

a <- all_results %>%
  filter(Rank == 1) %>%
  group_by(Club) %>%
  summarise(Wins = n()) %>%
  left_join(all_results %>%
              filter(Rank %in% c(1, 2, 3, 4)) %>%
              group_by(Club) %>%
             summarise(Top_4 = n()) 
  )  %>% 
  left_join(club_data %>%
                  select(Club, Elo, Tilt)) %>% 
  arrange(desc(Wins), desc(Top_4))


model_a <- lm(log(Wins) ~ Elo, data = a)
model_b <- lm(Wins ~ Elo, data = a)
  
summary(model_a)
summary(model_b)

# Plot ------------------------------
ggplot(a, aes(x = Elo, y = log(Wins))) +
  geom_point()



ggplot(
  all_results,
  aes(x = reorder(Club, -Rank, FUN = median), y = Rank)
) +
  geom_boxplot() +
  xlab("Club") +
  coord_flip() +
  scale_y_reverse()

ggplot(
  all_results,
  aes(x = reorder(Club, Points, FUN = median), y = Points)
) +
  geom_boxplot() +
  xlab("Club") +
  coord_flip()

all_results %>%
  group_by(Club) %>%
  summarize(min_rank = min(Rank)) %>%
  filter(min_rank < 5) %>%
  inner_join(all_results, by = "Club")


ggplot(
  all_results,
  aes(x = Points, y = Rank)
) +
  geom_jitter(aes(color = Club),
    width = 0.3, height = 0.3, alpha = 0.7
  ) + # Scatter points with jitter
  geom_smooth(
    se = FALSE,
    color = "black", linewidth = 1, alpha = 0.5
  ) + # Smooth line for all data
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_reverse() # Move legend to the bottom



model <- lm(Rank ~ Points, data = all_results)

summary(model)

