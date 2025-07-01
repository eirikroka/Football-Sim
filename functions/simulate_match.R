simulate_match_league <- function(club1_name, club2_name) {
  # Extract club data based on club names
  club1_data <- club_data[club_data$Club == club1_name, ]
  club2_data <- club_data[club_data$Club == club2_name, ]
  
  goals <- simulate_goals_league(club1_data$Elo, club2_data$Elo, club1_data$Tilt, club2_data$Tilt)
  goals_club1 <- goals[1]
  goals_club2 <- goals[2]
  
  # Determine points based on match result
  if (goals_club1 > goals_club2) {
    points_club1 <- 3
    points_club2 <- 0
  } else if (goals_club1 < goals_club2) {
    points_club1 <- 0
    points_club2 <- 3
  } else {
    points_club1 <- 1
    points_club2 <- 1
  }
  
  # Return the results of the match
  return(data.frame(
    Club_A = club1_name,
    Club_B = club2_name,
    Result_A = goals_club1,
    Result_B = goals_club2,
    Points_A = points_club1,
    Points_B = points_club2
  ))
}

# Knockout bias
simulate_match_knockout <- function(club1_name, club2_name) {
  # Extract club data based on club names
  club1_data <- club_data[club_data$Club == club1_name, ]
  club2_data <- club_data[club_data$Club == club2_name, ]
  
  goals <- simulate_goals_knockout(club1_data$Elo, club2_data$Elo, club1_data$Tilt, club2_data$Tilt)
  goals_club1 <- goals[1]
  goals_club2 <- goals[2]
  
  # Determine points based on match result
  if (goals_club1 > goals_club2) {
    points_club1 <- 3
    points_club2 <- 0
  } else if (goals_club1 < goals_club2) {
    points_club1 <- 0
    points_club2 <- 3
  } else {
    points_club1 <- 1
    points_club2 <- 1
  }
  
  # Return the results of the match
  return(data.frame(
    Club_A = club1_name,
    Club_B = club2_name,
    Result_A = goals_club1,
    Result_B = goals_club2,
    Points_A = points_club1,
    Points_B = points_club2
  ))
}

