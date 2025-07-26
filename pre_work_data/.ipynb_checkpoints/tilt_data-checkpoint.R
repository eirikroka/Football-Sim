library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(fs)
library(fuzzyjoin)



tilt_data <- read.csv("data/Tilt/tilt_data.csv") %>%
  mutate(Div = recode(Div,
    "B1" = "BEL",
    "D1" = "GER",
    "E0" = "ENG",
    "G1" = "GRE",
    "I1" = "ITA",
    "N1" = "NED",
    "P1" = "POR",
    "SC0" = "SCO",
    "SP1" = "ESP",
    "T1" = "TUR"
  )) %>%
  rename(Group = Div) %>%
  mutate(Club = recode(Club,
    "Club Brugge" = "Brugge",
    "Nott'm Forest" = "Forest",
    "OFI Crete" = "OFI",
    "Volos NFC" = "NFC Volos",
    "For Sittard" = "Sittard",
    "PSV Eindhoven" = "PSV",
    "Sp Lisbon" = "Sporting",
    "Ath Madrid" = "Atletico",
    "Vallecano" = "Rayo Vallecano",
    "Buyuksehyr" = "Bueyueksehir",
    "Bayern Munich" = "Bayern",
    "Ein Frankfurt" = "Frankfurt",
    "M'gladbach" = "Gladbach",
    "Werder Bremen" = "Werder",
    "Almere City" = "Almere",
    "Sp Braga" = "Braga",
    "Estrela" = "Estrela Amadora",
    "Ath Bilbao" = "Bilbao",
    "St. Gilloise" = "St Gillis",
    "AZ Alkmaar" = "Alkmaar",
    "Ad. Demirspor" = "Adana Demirspor",
    "Kayserispor" = "Kayseri",
    "Gaziantep" = "Gaziantep FK"
  ))



fuzzy_joined <- stringdist_join(club_data, tilt_data,
  by = c("Club" = "Club", "Group" = "Group"),
  mode = "left",
  method = "jw",
  max_dist = 0.05,
  distance_col = "dist"
) %>%
  arrange(Club.y, Club.dist) %>%
  group_by(Club.y) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(Club.y = ifelse(rank > 1, NA, Club.y)) %>%
  select(-dist, -rank)

unmatched_clubs <- tilt_data %>%
  anti_join(fuzzy_joined,
    by = c("Club" = "Club.y", "Group" = "Group.y")
  )

club_data <- fuzzy_joined %>%
  select(Club.x, Elo, Tilt, Group.x, Pass, Europa, UL) %>%
  rename(
    Club = Club.x,
    Group = Group.x
  ) %>%
  mutate(Tilt = ifelse(is.na(Tilt),
    rnorm(sum(is.na(Tilt)),
      mean = 1.3, sd = 0.1
    ), Tilt
  ))
