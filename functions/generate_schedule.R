generate_schedule <- function(club_data) {
  n <- nrow(club_data)
  
  # Create an empty data frame to store the schedule
  schedule_df <- data.frame(
    Round = integer(),
    Team1 = character(),
    Team2 = character()
  )
  
  # Generate the schedule
  for (round in 1:(n - 1)) {
    for (i in 1:(n / 2)) {
      team1 <- club_data$Club[i]
      team2 <- club_data$Club[n - i + 1]
      
      # In the second half of the rounds, swap the home and away teams to balance home-field advantage
      if (round > n / 2) {
        temp <- team1
        team1 <- team2
        team2 <- temp
      }
      
      # Add match to the schedule
      schedule_df <- rbind(schedule_df, data.frame(Round = round, Team1 = team1, Team2 = team2))
    }
    
    # Rotate the teams for the next round, keeping the first team fixed
    club_data$Club <- c(club_data$Club[1], club_data$Club[3:n], club_data$Club[2])
  }
  
  return(schedule_df)
}