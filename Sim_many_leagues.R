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
# yey ------------------------------

library(doParallel)
library(tictoc)
library(knitr)
library(patchwork)
library(furrr)

source("functions/simulate_goals_bias_knock.r")
source("functions/simulate_match.r")
source("pass_europa.r")
source("tilt_data.r")

club_data <- club_data[sample(nrow(club_data)), ]

n_sim <- 100

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
}, seed = T) %>%
  flatten()
toc()

# Stop the parallel backend
plan(sequential)

all_results <- map_dfr(
  seq_along(sim_results),
  ~ {
    # Extract the group_tables list for the current simulation
    group_tables <- sim_results[[.x]]$group_tables

    # Check if group_tables is a list
    if (is.list(group_tables)) {
      # Combine all league tables within group_tables
      bind_rows(group_tables, .id = "league_id") %>%
        mutate(simulation_id = .x) # Add the simulation ID
    } else {
      tibble() # Return an empty tibble if group_tables is not a list
    }
  }
) %>%
  as_tibble()

# Add Rank based on Points within each n group
all_results <- all_results %>%
  group_by(simulation_id, league_id) %>% # Group by n
  mutate(Rank = row_number()) %>% # Create the Rank column
  ungroup() # Ungroup the data to remove the grouping

all_results <- all_results %>%
  left_join(club_data %>%
    select(Club, Group))

a <- all_results %>%
  filter(Rank %in% c(1, 2, 3)) %>%
  group_by(Club, Group) %>%
  summarise(Top_3 = n()) %>%
  left_join(all_results %>%
    filter(Rank == 1) %>%
    group_by(Club, Group) %>%
    summarise(Wins = n())) %>%
  left_join(club_data %>%
    select(Club, Group, Elo, Tilt)) %>%
  arrange(desc(Wins), desc(Top_3))

model_a <- lm(log(Wins) ~ Elo + factor(Group), data = a)
model_a_t <- lm(log(Top_3) ~ Elo + factor(Group), data = a)

model_b <- lm(Wins ~ Elo + factor(Group), data = a)
model_b_t <- lm(Top_3 ~ Elo + factor(Group), data = a)
# Extract the Adjusted R^2
summary(model_a)$adj.r.squared
summary(model_b)$adj.r.squared

summary(model_a_t)$adj.r.squared
summary(model_b_t)$adj.r.squared

# Plot ------------------------------

all_results <- all_results %>%
  left_join(club_data %>%
    select(Club, Group, Elo, Tilt))


model_a <- lm(Rank ~ Elo + Tilt + factor(Group), data = all_results)

summary(model_a)$adj.r.squared

model_b <- lm(GF ~ GA + factor(Group), data = all_results)

summary(model_b)$adj.r.squared

library(ggplot2)

group <- "NOR"

# Calculate means for each club and reorder the Club factor levels in all_results
means <- all_results %>%
  group_by(Club, Group) %>%
  summarise(mean_rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  filter(Group == group) %>%
  arrange(mean_rank)

# Prepare the data for plotting
data_to_plot <- all_results %>%
  filter(Group == group) %>%
  left_join(means, by = "Club") %>%
  mutate(Club = factor(Club, levels = means$Club))

data_to_plot$Club <- factor(data_to_plot$Club, levels = levels(data_to_plot$Club))

# Plot with reordered facets
ggplot(data_to_plot) +
  geom_histogram(
    aes(x = Rank),
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  geom_vline(
    data = means %>%
      filter(Group == group) %>%
      mutate(Club = fct_reorder(Club, mean_rank)), # Ensure consistent ordering
    aes(xintercept = mean_rank),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_wrap(~Club) +
  labs(
    title = "Distribution of Rank for Each Club",
    x = "Rank",
    y = "Count"
  ) +
  theme_minimal()


all_results <- all_results %>%
  group_by(Group) %>%
  mutate(Clubs_in_Group = n_distinct(Club)) %>%
  ungroup() %>%
  mutate(
    Mean_points_per_match = Points / ((Clubs_in_Group - 1) * 2),
    Mean_GA_per_match = GA / ((Clubs_in_Group - 1) * 2),
    Mean_GF_per_match = GF / ((Clubs_in_Group - 1) * 2),
    Mean_GD_per_match = GD / ((Clubs_in_Group - 1) * 2),
  )

library(viridis)

ggplot(
  all_results,
  aes(
    y = Mean_GF_per_match,
    x = Rank,
    color = -Elo
  )
) +
  geom_jitter() +
  facet_wrap(~Group) +
  scale_color_viridis_c(option = "plasma", name = "Elo") +
  theme_minimal()



ggplot(
  all_results %>%
    filter(Group == "SRB"),
  aes(
    x = Mean_points_per_match,
    y = Mean_GF_per_match, 
    color = -Elo
  )
) +
  geom_jitter() +
  scale_color_viridis_c(option = "plasma", name = "Elo") +
  theme_minimal()
