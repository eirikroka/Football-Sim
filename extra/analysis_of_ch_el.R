library(tidyverse)
library(readxl)

clubs <- read_csv("data/data_plot.csv") %>%
  select(Club, Country, Elo)

fixtures <- read_excel("data/Fixtures.xlsx")

points <- read_excel("data/points.xlsx")

fixtures <- fixtures %>%
  mutate(
    Home = recode(Home,
      "Bologona" = "Bologna", "Donetsk" = "Shakhtar",
      "Leipzig" = "RB Leipzig", "PSG" = "Paris SG",
      "Slovan" = "Slovan Bratislava"
    ),
    Away = recode(Away,
      "Bologona" = "Bologna", "Donetsk" = "Shakhtar",
      "Leipzig" = "RB Leipzig", "PSG" = "Paris SG",
      "Slovan" = "Slovan Bratislava"
    )
  )


unique_clubs <- fixtures %>%
  count(Home) %>%
  select(Home)

unique_clubs$Club <- unique_clubs$Home
unique_clubs$Home <- NULL

elo <- clubs %>%
  right_join(unique_clubs)

fixtures_elo <- fixtures %>%
  left_join(elo, by = c("Away" = "Club")) %>%
  rename(Opponent_Elo_Away = Elo) %>%
  left_join(elo, by = c("Home" = "Club")) %>%
  rename(Opponent_Elo_Home = Elo)

# Compute average opponent Elo for each team
avg_elo_per_team <- fixtures_elo %>%
  select(Home, Away, Opponent_Elo_Away, Opponent_Elo_Home) %>%
  pivot_longer(cols = c(Home, Away), names_to = "Match_Type", values_to = "Club") %>%
  mutate(Opponent_Elo = ifelse(Match_Type == "Home", Opponent_Elo_Away, Opponent_Elo_Home)) %>%
  group_by(Club) %>%
  summarise(Average_Opponent_Elo = mean(Opponent_Elo, na.rm = TRUE)) %>%
  arrange(desc(Average_Opponent_Elo))

plot_data <- avg_elo_per_team %>% left_join(elo) %>% 
  mutate(diff_elo = Elo - Average_Opponent_Elo) %>% 
  left_join(points)

ggplot(plot_data, aes(x = diff_elo, y = Rank)) +
  geom_point(aes(color = Elo), size = 3, alpha = 0.7) +  # Customize point size and transparency
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1.2) +  # Line thickness
  scale_y_reverse() +  # Flip the y-axis
  labs(
    title = "Diff_Elo at the Start of the Group Stage\nand Final Rank in the Group Stage",
    x = "Diff_Elo",
    y = "Rank"
  ) +
  theme_minimal(base_size = 15) +  # Base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Add light grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.25)
  ) +
  scale_color_gradient(low = "blue", high = "red")  # Gradient color scale for points

# Center & adjust font size



# For Europa League ------------------------------
clean_whitespace <- function(x) {
  gsub("\u00a0", " ", x)
}

# Read the fixtures data and clean whitespace
fixtures_EL <- read_excel("data/fixtures_EL.xlsx") %>%
  mutate(across(everything(), ~trimws(clean_whitespace(.))))

fixtures_EL <- fixtures_EL %>%
  mutate(
    Home = recode(Home,"Ludogorets" = "Razgrad", "Malmo" = "Malmoe"
    ),
    Away = recode(Away,"Ludogorets" = "Razgrad", "Malmo" = "Malmoe"
  )
)

fixtures_EL %>% 
  count(Home) %>% 
  arrange(desc(n()))

fixtures_EL %>% 
  count(Away) %>% 
  arrange((n()))

points_EL <- read_excel("data/points_EL.xlsx")

unique_clubs_EL <- fixtures_EL %>%
  count(Home) %>%
  select(Home)

unique_clubs_EL$Club <- unique_clubs_EL$Home
unique_clubs_EL$Home <- NULL

elo_EL <- clubs %>%
  right_join(unique_clubs_EL)

fixtures_elo_EL <- fixtures_EL %>%
  left_join(elo_EL, by = c("Away" = "Club")) %>%
  rename(Opponent_Elo_Away = Elo) %>%
  left_join(elo_EL, by = c("Home" = "Club")) %>%
  rename(Opponent_Elo_Home = Elo)

# Compute average opponent Elo for each team
avg_elo_per_team_EL <- fixtures_elo_EL %>%
  select(Home, Away, Opponent_Elo_Away, Opponent_Elo_Home) %>%
  pivot_longer(cols = c(Home, Away), names_to = "Match_Type", values_to = "Club") %>%
  mutate(Opponent_Elo = ifelse(Match_Type == "Home", Opponent_Elo_Away, Opponent_Elo_Home)) %>%
  group_by(Club) %>%
  summarise(Average_Opponent_Elo = mean(Opponent_Elo, na.rm = TRUE)) %>%
  arrange(desc(Average_Opponent_Elo))

plot_data_EL <- avg_elo_per_team_EL %>% 
  left_join(elo_EL) %>% 
  mutate(diff_elo = Elo - Average_Opponent_Elo) %>% 
  left_join(points_EL)

ggplot(plot_data_EL, aes(x = diff_elo, y = Rank)) +
  geom_point(aes(color = Elo), size = 3, alpha = 0.7) +  # Customize point size and transparency
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1.2) +  # Line thickness
  scale_y_reverse() +  # Flip the y-axis
  labs(
    title = "Diff_Elo at the Start of the Group Stage\nand Final Rank in the Group Stage",
    x = "Diff_Elo",
    y = "Rank"
  ) +
  theme_minimal(base_size = 15) +  # Base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Add light grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.25)
  ) +
  scale_color_gradient(low = "blue", high = "red")  # Gradient color scale for points

# Center & adjust font size

model_EL <- lm(Elo ~ diff_elo, plot_data_EL)
summary(model_EL)

model <- lm(Elo ~ diff_elo, plot_data)
summary(model)

ggplot(plot_data, aes(Elo, diff_elo, color = Points)) +
  geom_point()
