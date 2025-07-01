europa_league_knockout <- function(data) {
  
  group_results <- data %>%
    group_by(Group) %>%
    mutate(
      qualified_for_other_comp = row_number() <= Pass,
      eligible_for_europa = row_number() > Pass &
        row_number() <= (Pass + Europa)
    ) %>%
    ungroup()
  
  # Filter teams eligible for Europa League
  europa_teams <- group_results %>%
    filter(eligible_for_europa) %>%
    select(-qualified_for_other_comp, -eligible_for_europa)
  
  if (nrow(europa_teams) %% 2 != 0) {
    stop("The number of teams for the Europa League knockout stage must be even.")
  }
  
  # Initialize results for Europa League knockout
  europa_results <- data.frame(
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
  
  # Shuffle the teams to randomize the initial pairings
  europa_teams <- europa_teams[sample(nrow(europa_teams)), ]
  round_number <- 0
  first_draw <- TRUE
  
  while (nrow(europa_teams) > 1) {
    round_number <- round_number + 1
    
    if (first_draw) {
      # Split teams into two pots based on Elo ranking
      europa_teams <-  europa_teams[order(- europa_teams$Elo), ] # Sort by Elo descending
      num_best <- nrow( europa_teams) %/% 2 # Number of teams in the best pot
      
      pot_best <-  europa_teams$Club[1:num_best] # Top half teams
      pot_rest <-  europa_teams$Club[(num_best + 1):nrow(europa_teams)] # Remaining teams
      
      # Ensure pots have equal size
      if (length(pot_best) != length(pot_rest)) {
        stop("Unequal pots. Please check the number of teams.")
      }
      
      schedule <- data.frame(
        Home = pot_best,
        Away = sample(pot_rest)
      )
      first_draw <- FALSE # Disable pot-based draw for future rounds
    } else {
      # Pair teams for the round
      schedule <- data.frame(
        Home = europa_teams$Club[seq(1, nrow(europa_teams), 2)],
        Away = europa_teams$Club[seq(2, nrow(europa_teams), 2)]
      )
    }
    round_results <- data.frame(
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
    
    # Simulate matches for this round
    for (i in 1:nrow(schedule)) {
      club1 <- schedule$Home[i]
      club2 <- schedule$Away[i]
      
      # Simulate home and away matches
      total_goals_club1 <- 0
      total_goals_club2 <- 0
      
      for (leg in 1:2) {
        # if (leg == 2) {
        #   tmp <- club1
        #   club1 <- club2
        #   club2 <- tmp
        # }
        match_result <- simulate_match_knockout(club1, club2) # Adjust for Europa League bias if needed
        total_goals_club1 <- total_goals_club1 + match_result$Result_A
        total_goals_club2 <- total_goals_club2 + match_result$Result_B
        
        # Handle tie-breaking in the second leg
        if (leg == 2 && total_goals_club1 == total_goals_club2) {
          penalties <- sample(c(1, 0), 1, prob = c(0.5, 0.5))
          if (penalties == 1) {
            total_goals_club1 <- total_goals_club1 + 0.1
          } else {
            total_goals_club2 <- total_goals_club2 + 0.1
          }
        }
      }
      
      winner <- ifelse(total_goals_club1 > total_goals_club2, schedule$Home[i], schedule$Away[i])
      
      round_results <- rbind(round_results, data.frame(
        Stage = "Europa League",
        Round = round_number,
        Club_A = schedule$Home[i],
        Club_B = schedule$Away[i],
        Result_A = total_goals_club1,
        Result_B = total_goals_club2,
        Winner = winner,
        Points_A = NA,
        Points_B = NA,
        Elo_A = europa_teams$Elo[schedule$Home[i] == europa_teams$Club],
        Elo_B = europa_teams$Elo[schedule$Away[i] == europa_teams$Club]
      ))
    }
    
    europa_results <- rbind(europa_results, round_results)
    
    # Update remaining teams
    losers <- ifelse(round_results$Result_A < round_results$Result_B, round_results$Club_A, round_results$Club_B)
    europa_teams <- europa_teams[!(europa_teams$Club %in% losers), ]
  }
  
  champion <- europa_teams$Club
  
  list(
    results = europa_results,
    champion = champion
  )
}
