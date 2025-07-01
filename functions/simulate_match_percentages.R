simulate_match_percentages <- function(club1_name, club2_name, n_simulations = 1000) {
  # Initialize counters
  wins_club1 <- 0
  draws <- 0
  losses_club1 <- 0

  # Simulate n_simulations matches
  for (i in 1:n_simulations) {
    result <- simulate_match(club1_name, club2_name)

    # Check the result of the match
    if (result$Points_A > result$Points_B) {
      wins_club1 <- wins_club1 + 1
    } else if (result$Points_A < result$Points_B) {
      losses_club1 <- losses_club1 + 1
    } else {
      draws <- draws + 1
    }
  }

  # Calculate and return percentages
  win_percentage <- (wins_club1 / n_simulations) * 100
  draw_percentage <- (draws / n_simulations) * 100
  loss_percentage <- (losses_club1 / n_simulations) * 100

  return(list(win_percentage = win_percentage, draw_percentage = draw_percentage, loss_percentage = loss_percentage))
}



matches <- match_results_knockout$results_1 %>% select(Club_A, Club_B)
# make sure the combn function from the utils package is available
library(utils)

# get all unique club names
all_clubs <- unique(c(matches$Club_A, matches$Club_B))

# initialize an empty tibble to store the results
results <- tibble(
  Club = character(),
  Opponent = character(),
  Win_Percentage = numeric(),
  Draw_Percentage = numeric(),
  Loss_Percentage = numeric()
)

# generate all possible pairs of clubs
club_pairs <- combn(all_clubs, 2)

# calculate match percentages for each pair of clubs
for (i in 1:ncol(club_pairs)) {
  club1_name <- club_pairs[1, i]
  club2_name <- club_pairs[2, i]
  percentages <- simulate_match_percentages(club1_name, club2_name)

  # add results for club 1 vs club 2
  results <- add_row(
    results,
    Club = club1_name,
    Opponent = club2_name,
    Win_Percentage = percentages$win_percentage,
    Draw_Percentage = percentages$draw_percentage,
    Loss_Percentage = percentages$loss_percentage
  )

  # add results for club 2 vs club 1
  results <- add_row(
    results,
    Club = club2_name,
    Opponent = club1_name,
    Win_Percentage = percentages$loss_percentage, # note the switch of win and loss
    Draw_Percentage = percentages$draw_percentage,
    Loss_Percentage = percentages$win_percentage # note the switch of win and loss
  )
}

results$Win_Loss <- results$Win_Percentage + results$Loss_Percentage
results <- results %>%
  mutate(
    Win = Win_Percentage / Win_Loss,
    Loss = Loss_Percentage / Win_Loss
  ) %>%
  select(Club, Opponent, Win, Loss)

