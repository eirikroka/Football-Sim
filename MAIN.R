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
set.seed(5)

# This script contains some randomness, and it contains the number of pots for each league to have 
# in the 'Champions League' and the 'Europa League'
source("pre_work_data/pass_europa.r")

source("pre_work_data/tilt_data.r")

club_data <- club_data[sample(nrow(club_data)), ]

tic()
# This simulates the league table for each club
# and have a knockout competition like the champions league
sim_results <- run_simulation_tournament(club_data)
toc()

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

champion <- sim_results$champion
europa_champ <- el_sim_result$champion
ul_champ <- ul_sim_result$champion

winner_nation <- club_data %>%
  filter(Club == champion) %>%
  select(Group) %>%
  pull()

club_data %>%
  filter(Club == champion)

club_data %>%
  filter(Club == europa_champ)

club_data %>%
  filter(Club == ul_champ)

# Results ------------------------------

sim_results$group_tables %>%
  map(as_tibble) %>%
  .[grep(winner_nation, names(.))]

sim_results$group_tables %>%
  map(as_tibble) %>%
  .[grep("ENG", names(.))]

team <- champion

results %>%
  filter(Stage == "Knockout") %>%
  select(Club_A, Club_B, Result_A, Result_B, Round, Penalty) %>%
  filter(Club_A == team | Club_B == team) %>%
  arrange(desc(Round)) %>%
  kable()

results %>%
  filter(Stage == "Knockout") %>%
  filter(Round == 1) %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty) %>%
  mutate(Winner = if_else(Result_A > Result_B, "A", "B")) %>%
  count(Winner)

results %>%
  filter(Stage == "Knockout") %>%
  filter(Round == 2) %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty) %>%
  kable()

results %>%
  filter(Stage == "Knockout") %>%
  filter(Round == 3) %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty) %>%
  kable()

results %>%
  filter(Stage == "Knockout") %>%
  filter(Round == 4) %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty) %>%
  kable()

results %>%
  filter(Stage == "Knockout") %>%
  filter(Round == 5) %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty) %>%
  kable()

# Working with the data ------------------------------

matches <- sim_results$results %>%
  filter(Stage != "Knockout") %>%
  mutate(MatchOrder = row_number()) %>%
  select(Group = Stage, MatchOrder, Club_A, Points_A, Club_B, Points_B) %>%
  pivot_longer(cols = c(Club_A, Club_B), names_to = "HomeAway", values_to = "Club") %>%
  mutate(Points = ifelse(HomeAway == "Club_A", Points_A, Points_B)) %>%
  select(Group, MatchOrder, Club, Points) %>%
  arrange(Group, MatchOrder)

cumulative_points <- matches %>%
  group_by(Group, Club) %>%
  arrange(Group, MatchOrder) %>%
  mutate(MatchOrderInGroup = row_number(), CumulativePoints = cumsum(Points)) %>%
  ungroup() %>%
  separate(Group, into = c("a", "Group"), sep = " ") %>%
  select(-a) %>%
  left_join(result_tables %>% select(Club, Rank)) %>%
  mutate(
    Club = factor(Club, levels = result_tables$Club),
    Mean_points = CumulativePoints / MatchOrderInGroup
  ) %>%
  group_by(Group, MatchOrderInGroup) %>%
  arrange(desc(CumulativePoints), .by_group = TRUE) %>%
  mutate(RankInGroup = row_number()) %>%
  ungroup()

big_groups <- c("ENG", "ESP", "GER", "ITA")

# Plotting ------------------------------

# winner_nation <- "ENG"
n <- 4

europa_winner_nation <- club_data %>%
  filter(Club == europa_champ) %>%
  select(Group) %>%
  pull()

p1 <- ggplot(
  cumulative_points %>%
    filter(Group == winner_nation & Rank <= n & MatchOrderInGroup >= 8),
  aes(y = Mean_points, x = MatchOrderInGroup, color = Club)
) +
  geom_step(linewidth = 0.8, direction = "mid") +
  geom_point(
    data = cumulative_points %>%
      filter(Group == winner_nation & Rank <= n & RankInGroup == 1 & MatchOrderInGroup >= 8),
    aes(y = Mean_points, x = MatchOrderInGroup, color = Club),
    size = 2,
    shape = 22,
    fill = "black"
  ) +
  ggtitle(paste("The cl champion:", champion)) +
  xlab("Round") +
  ylab("Average points through the season") +
  labs(x = NULL) +
  theme_minimal() +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

p2 <- ggplot(
  cumulative_points %>%
    filter(Group == europa_winner_nation & Rank <= n & MatchOrderInGroup >= 8),
  aes(y = Mean_points, x = MatchOrderInGroup, color = Club)
) +
  geom_step(linewidth = 0.8, direction = "mid") +
  geom_point(
    data = cumulative_points %>%
      filter(Group == europa_winner_nation & Rank <= n & RankInGroup == 1 & MatchOrderInGroup >= 8),
    aes(y = Mean_points, x = MatchOrderInGroup, color = Club),
    size = 2,
    shape = 22,
    fill = "black"
  ) +
  ggtitle(paste("The EL Champion:", europa_champ)) +
  ylab("Average points through the season") +
  xlab("Round") +
  theme_minimal() +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

p1 / p2

result_tables %>%
  filter(Group == winner_nation) %>%
  left_join(club_data %>%
    select(Club, Elo)) %>%
  select(Elo, everything()) %>%
  select(-n_teams, -Rank, -Group, -Europa)

club_data %>%
  filter(Club == champion)

results %>%
  filter(Stage == "Knockout") %>%
  filter(Club_A == champion | Club_B == champion) %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty, Round) %>%
  arrange(desc(Round)) %>%
  kable()

result_tables %>%
  filter(Group == europa_winner_nation) %>%
  left_join(club_data %>%
    select(Club, Elo)) %>%
  select(Elo, everything()) %>%
  select(-n_teams, -Rank, -Group, -Europa)

club_data %>%
    filter(Club == europa_champ)

europa_results %>%
  select(Club_A, Club_B, Result_A, Result_B, Penalty, Round) %>%
  filter(Club_A == europa_champ | Club_B == europa_champ) %>%
  arrange(desc(Round)) %>%
  kable()

p3 <- ggplot(
  cumulative_points %>%
    filter(Group == winner_nation & Rank <= n & MatchOrderInGroup >= 4),
  aes(y = CumulativePoints, x = MatchOrderInGroup, color = Club)
) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(
    data = cumulative_points %>%
      filter(Group == winner_nation & Rank <= 3 & RankInGroup == 1 & MatchOrderInGroup >= 4),
    aes(y = CumulativePoints, x = MatchOrderInGroup, color = Club),
    size = 2,
    shape = 22,
    fill = "black"
  ) +
  theme_minimal()

p4 <- ggplot(cumulative_points %>%
  filter(Club == champion), aes(y = RankInGroup, x = MatchOrderInGroup, color = Club)) +
  geom_step(linewidth = 1) +
  scale_y_reverse() +
  geom_point(
    data = cumulative_points %>%
      filter(Club == champion & RankInGroup == 1),
    aes(y = RankInGroup, x = MatchOrderInGroup), size = 2,
    shape = 23,
    fill = "black"
  )


# Plotting all Leagues ------------------------------

group_vector <- club_data %>% 
  filter(Pass > 0 | Europa > 0) %>% 
  arrange(desc(Pass), desc(Europa), desc(Elo)) %>% 
  distinct(Group) %>% 
  pull(Group)

for (group in group_vector) {
  # Filter the data for the current group and create the plot
  p <- ggplot(
    cumulative_points %>%
      filter(Group == group & Rank <= n & MatchOrderInGroup >= 8),
    aes(y = Mean_points, x = MatchOrderInGroup, color = Club)
  ) +
    geom_step(linewidth = 0.8, direction = "mid") +
    geom_point(
      data = cumulative_points %>%
        filter(Group == group & Rank <= n & RankInGroup == 1 & MatchOrderInGroup >= 8),
      aes(y = Mean_points, x = MatchOrderInGroup, color = Club),
      size = 2,
      shape = 22,
      fill = "black"
    ) +
    ggtitle(paste0("Group: ", group, ", Pass:", club_data %>% 
                    distinct(Group, Pass) %>% 
                    filter(Group == group) %>% 
                    pull(Pass), ", Europa:",  club_data %>% 
                     distinct(Group, Europa) %>% 
                     filter(Group == group) %>% 
                     pull(Europa)
                  )) +
    xlab("Round") +
    ylab("Average points through the season") +
    theme_minimal() +
    guides(color = guide_legend(title = NULL)) +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
  
  # Print the plot in the plot window
  print(p)
  
  # Pause to allow viewing the plot before moving to the next one
  readline(prompt = "Press [Enter] to continue to the next plot...")
}
















