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
    
    if (group_size %% 2 != 0) {
      group_teams <- c(group_teams, "BYE")
      group_size <- group_size + 1
    }
    
    # Get the number of teams to pass from this group
    pass_teams <- club_data$Pass[club_data$Group == group][1]
    
    if (pass_teams > group_size) {
      stop(paste("Group", group, "has less teams than the number of teams to pass. Please ensure the number of teams to pass is less than the total teams in the group."))
    }
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
        
        if (club1 == "BYE" || club2 == "BYE") {
          next
        }
        
        # If round > group_size - 1, swap home and away teams
        if (round > group_size - 1) {
          tmp <- club1
          club1 <- club2
          club2 <- tmp
        }
        
        match_result <- simulate_match_league(club1, club2) # Group stage bias
        
        #club_data <- update_elo(club_data, match_result)$club_data
        
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
    group_tables[[paste("Group", group)]] <- group_table
    
    if (pass_teams > 0) {
      top_teams <- group_table$Club[1:pass_teams]
      remaining_teams <- c(remaining_teams, top_teams) # Update remaining_teams
    } 
  }
  
  
  # Knockoutstage ----------------------------------------------------------------------------------
  club_data <- club_data[club_data$Club %in% remaining_teams, ]
  print(paste("Number of teams through the group stage:", length(remaining_teams)))
  
  club_data <- club_data[sample(nrow(club_data)), ]
  round_number <- 0 # Initialize round counter
  first_draw <- TRUE
  
  while (nrow(club_data) > 1) {
    round_number <- round_number + 1 
    
    if (first_draw) {
      # Split teams into two pots based on Elo ranking
      club_data <- club_data[order(-club_data$Elo), ] # Sort by Elo descending
      num_best <- nrow(club_data) %/% 2 # Number of teams in the best pot
      
      pot_best <- club_data$Club[1:num_best] # Top half teams
      pot_rest <- club_data$Club[(num_best + 1):nrow(club_data)] # Remaining teams
      
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
      # Random pairing for subsequent rounds
      club_data <- club_data[sample(nrow(club_data)), ] # Shuffle teams
      schedule <- data.frame(
        Home = club_data$Club[seq(1, nrow(club_data), 2)],
        Away = club_data$Club[seq(2, nrow(club_data), 2)]
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
      Points_B = numeric(0)
    )
    
    for (round in 1:nrow(schedule)) {
      club1 <- schedule$Home[round]
      club2 <- schedule$Away[round]
      
      total_goals_club1 <- 0
      total_goals_club2 <- 0
      for (leg in 1:2) {
        # if (leg == 2) { # Swap clubs for the second leg
        #   tmp <- club1
        #   club1 <- club2
        #   club2 <- tmp
        # }
        source("functions/simulate_goals_bias_knock.r")
        
        match_result <- simulate_match_knockout(club1, club2) 
        
        #club_data <- update_elo(club_data, match_result)$club_data
        total_goals_club1 <- total_goals_club1 + match_result$Result_A
        total_goals_club2 <- total_goals_club2 + match_result$Result_B
        
      
        if (leg == 2) {
          while (total_goals_club1 == total_goals_club2) {
            # Get group for the home and away teams
            group_club1 <- club_data$Group[club_data$Club == schedule$Home[round]]
            group_club2 <- club_data$Group[club_data$Club == schedule$Away[round]]
            
            # Retrieve points for the home and away teams from their respective group tables
            points_club1 <- group_tables[[paste("Group", group_club1)]]$Points[group_tables[[paste("Group", group_club1)]]$Club == schedule$Home[round]]
            points_club2 <- group_tables[[paste("Group", group_club2)]]$Points[group_tables[[paste("Group", group_club2)]]$Club == schedule$Away[round]]
            
            # Ensure points_club1 and points_club2 are scalars
            points_club1 <- ifelse(length(points_club1) > 0, points_club1[1], 0)
            points_club2 <- ifelse(length(points_club2) > 0, points_club2[1], 0)
            
            total_points <- points_club1 + points_club2
            prob_club1 <- points_club1 / total_points
            prob_club2 <- points_club2 / total_points
            
            
            # Simulate penalties
            penalties <- sample(c(1, 0), 1, prob = c(prob_club1, prob_club2))
            
            # Assign penalty to the respective team
            if (penalties == 1) {
              total_goals_club1 <- total_goals_club1 + 0.2
            } else {
              total_goals_club2 <- total_goals_club2 + 0.2
            }
          }
        }
        
        
      }
      
      winner <- ifelse(total_goals_club1 > total_goals_club2, schedule$Home[round], schedule$Away[round])
      
      round_results <- rbind(round_results, data.frame(
        Stage = "Knockout",
        Round = round_number,
        Club_A = schedule$Home[round],
        Club_B = schedule$Away[round],
        Result_A = total_goals_club1,
        Result_B = total_goals_club2,
        Winner = winner,
        Points_A = NA,
        Points_B = NA,
        Elo_A = club_data$Elo[schedule$Home[round] == club_data$Club],
        Elo_B = club_data$Elo[schedule$Away[round] == club_data$Club]
      ))
    }
    
    results <- rbind(results, round_results)
    losers <- ifelse(round_results$Result_A < round_results$Result_B, round_results$Club_A, round_results$Club_B)
    club_data <- club_data[!(club_data$Club %in% losers), ]
  }
  
  # The last remaining team is the champion
  champion <- club_data$Club
  
  list(
    results = results,
    champion = champion,
    group_tables = group_tables
  )
}
