# In this functions I try to make a greater bias toward the better team
# I try somewhat to replicate reality, but my method is not perfect.

simulate_goals_league <- function(elo_team1, elo_team2, tilt_team1, tilt_team2, home_adv = 0) {
  avg_tilt <- (tilt_team1 + tilt_team2) / 2

  # Calculate bias factor based on difference in ELO ratings
  elo_diff <- abs(elo_team1 - elo_team2)
  
  # Use Elo ratings to calculate expected average goals
  avg_goals_team1 <- ((elo_team1 + home_adv) / elo_team2) * avg_tilt
  avg_goals_team2 <- (elo_team2 / (elo_team1 + home_adv)) * avg_tilt

  if (elo_team1 > elo_team2){
    avg_goals_team1 <- max(0.1, min(5, avg_goals_team1 * (1 + elo_diff / 1500)))
    avg_goals_team2 <- max(0.1, min(5, avg_goals_team2 * (1 - elo_diff / 1500)))
  }

  if (elo_team2 > elo_team1){
    avg_goals_team1 <- max(0.1, min(5, avg_goals_team1 * (1 - elo_diff / 1500)))
    avg_goals_team2 <- max(0.1, min(5, avg_goals_team2 * (1 + elo_diff / 1500)))
  }

  goals_team1 <- rpois(1, avg_goals_team1)
  goals_team2 <- rpois(1, avg_goals_team2)
  
  sd_100 = 0.15
  sd_200 = 0.5
  sd_300 = 1
  
  if (elo_diff > 100) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_100, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_100, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_100, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_100, n = 1)))
    }
  }

  if (elo_diff > 200) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_200, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_200, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_200, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_200, n = 1)))
    }
  }
  
  
  if (elo_diff > 300) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_300, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_300, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_300, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_300, n = 1)))
    }
  }
  
  # Trying to handle to big scores 
  if (goals_team1 + goals_team1 > 7){
    goals_team1 <- ceiling(goals_team1/1.5)
    goals_team2 <- ceiling(goals_team2/1.5)
  }
  
  if (abs(goals_team1 - goals_team2) > 4) {
    if (goals_team1 > 4) {
      goals_team1 = 4 + floor((goals_team1 - 4)/1.5)
    }
    
    if (goals_team2 > 4) {
      goals_team2 = 4 + floor((goals_team2 - 4)/1.5)
    }
  }


  return(c(goals_team1, goals_team2))
}

# This is for the kncokout stages
simulate_goals_knockout <- function(elo_team1, elo_team2, tilt_team1, tilt_team2, home_adv = 0) {
  avg_tilt <- (tilt_team1 + tilt_team2) / 2
  
  # Calculate bias factor based on difference in ELO ratings
  elo_diff <- abs(elo_team1 - elo_team2)
  
  # Use Elo ratings to calculate expected average goals
  avg_goals_team1 <- ((elo_team1 + home_adv) / elo_team2) * avg_tilt
  avg_goals_team2 <- (elo_team2 / (elo_team1 + home_adv)) * avg_tilt

if (elo_team1 > elo_team2){
  avg_goals_team1 <- max(0.1, min(5, avg_goals_team1 * (1 + elo_diff / 2000)))
  avg_goals_team2 <- max(0.1, min(5, avg_goals_team2 * (1 - elo_diff / 2000)))
}

if (elo_team2 > elo_team1){
  avg_goals_team1 <- max(0.1, min(5, avg_goals_team1 * (1 - elo_diff / 2000)))
  avg_goals_team2 <- max(0.1, min(5, avg_goals_team2 * (1 + elo_diff / 2000)))
}
  
  goals_team1 <- rpois(1, avg_goals_team1)
  goals_team2 <- rpois(1, avg_goals_team2)
  
  sd_100 = 0.1
  sd_200 = 0.3
  sd_300 = 0.7
  sd_400 = 1
  
  if (elo_diff > 100) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_100, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_100, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_100, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_100, n = 1)))
    }
  }
  
  if (elo_diff > 200) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_200, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_200, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_200, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_200, n = 1)))
    }
  }
  
  
  if (elo_diff > 300) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_300, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_300, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_300, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_300, n = 1)))
    }
  }
  
  
  if (elo_diff > 400) {
    if (elo_team1 > elo_team2) {
      goals_team1 <- goals_team1 + abs(round(rnorm(0, sd = sd_400, n = 1)))
      goals_team2 <- max(0, goals_team2 - abs(round(rnorm(0, sd = sd_400, n = 1))))
    } else {
      goals_team1 <- max(0, goals_team1 - abs(round(rnorm(0, sd = sd_400, n = 1))))
      goals_team2 <- goals_team2 + abs(round(rnorm(0, sd = sd_400, n = 1)))
    }
  }
  
  # Trying to handle to big scores 
  if (goals_team1 + goals_team1 > 7){
    goals_team1 <- ceiling(goals_team1/1.5)
    goals_team2 <- ceiling(goals_team2/1.5)
  }
  
  if (abs(goals_team1 - goals_team2) > 4) {
    if (goals_team1 > 4) {
      goals_team1 = 4 + floor((goals_team1 - 4)/1.5)
    }
    
    if (goals_team2 > 4) {
      goals_team2 = 4 + floor((goals_team2 - 4)/1.5)
    }
  }
  
  
  return(c(goals_team1, goals_team2))
}

