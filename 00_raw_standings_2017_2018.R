
library(datapasta)
library(tidyverse)


east_standing = tibble::tribble(
         ~Eastern.Conference, ~W, ~L, ~WL,    ~GB, ~PSG, ~PAG,  ~SRS,
      "Toronto Raptors* (1)", 59, 23,  0.72,    "—", 111.7, 103.9,  7.29,
       "Boston Celtics* (2)", 55, 27, 0.671,  "4.0",   104, 100.4,  3.23,
   "Philadelphia 76ers* (3)", 52, 30, 0.634,  "7.0", 109.8, 105.3,   4.3,
  "Cleveland Cavaliers* (4)", 50, 32,  0.61,  "9.0", 110.9, 109.9,  0.59,
       "Indiana Pacers* (5)", 48, 34, 0.585, "11.0", 105.6, 104.2,  1.18,
           "Miami Heat* (6)", 44, 38, 0.537, "15.0", 103.4, 102.9,  0.15,
      "Milwaukee Bucks* (7)", 44, 38, 0.537, "15.0", 106.5, 106.8, -0.45,
   "Washington Wizards* (8)", 43, 39, 0.524, "16.0", 106.6,   106,  0.53,
       "Detroit Pistons (9)", 39, 43, 0.476, "20.0", 103.8, 103.9, -0.26,
    "Charlotte Hornets (10)", 36, 46, 0.439, "23.0", 108.2,   108,  0.07,
      "New York Knicks (11)", 29, 53, 0.354, "30.0", 104.5,   108, -3.53,
        "Brooklyn Nets (12)", 28, 54, 0.341, "31.0", 106.6, 110.3, -3.67,
        "Chicago Bulls (13)", 27, 55, 0.329, "32.0", 102.9,   110, -6.84,
        "Orlando Magic (14)", 25, 57, 0.305, "34.0", 103.4, 108.2, -4.92,
        "Atlanta Hawks (15)", 24, 58, 0.293, "35.0", 103.4, 108.8,  -5.3
  )

west_standing = tibble::tribble(
                            ~Western.Conference, ~W, ~L, ~WL,    ~GB, ~PSG, ~PAG,  ~SRS,
                         "Houston Rockets* (1)", 65, 17, 0.793,    "—", 112.4, 103.9,  8.21,
                   "Golden State Warriors* (2)", 58, 24, 0.707,  "7.0", 113.5, 107.5,  5.79,
                  "Portland Trail Blazers* (3)", 49, 33, 0.598, "16.0", 105.6,   103,   2.6,
                   "Oklahoma City Thunder* (4)", 48, 34, 0.585, "17.0", 107.9, 104.4,  3.42,
                               "Utah Jazz* (5)", 48, 34, 0.585, "17.0", 104.1,  99.8,  4.47,
                    "New Orleans Pelicans* (6)", 48, 34, 0.585, "17.0", 111.7, 110.4,  1.48,
                       "San Antonio Spurs* (7)", 47, 35, 0.573, "18.0", 102.7,  99.8,  2.89,
                  "Minnesota Timberwolves* (8)", 47, 35, 0.573, "18.0", 109.5, 107.3,  2.35,
                           "Denver Nuggets (9)", 46, 36, 0.561, "19.0",   110, 108.5,  1.57,
                    "Los Angeles Clippers (10)", 42, 40, 0.512, "23.0",   109,   109,  0.15,
                      "Los Angeles Lakers (11)", 35, 47, 0.427, "30.0", 108.1, 109.6, -1.44,
                        "Sacramento Kings (12)", 27, 55, 0.329, "38.0",  98.8, 105.8,  -6.6,
                        "Dallas Mavericks (13)", 24, 58, 0.293, "41.0", 102.3, 105.4,  -2.7,
                       "Memphis Grizzlies (14)", 22, 60, 0.268, "43.0",  99.3, 105.5, -5.81,
                            "Phoenix Suns (15)", 21, 61, 0.256, "44.0", 103.9, 113.3,  -8.8
                  )

standing = rbind.data.frame(east_standing %>% rename(Team = Eastern.Conference) , west_standing %>%
                              rename(Team = Western.Conference )) %>%
                    arrange(desc(W)) %>%
  cbind.data.frame(abbreviation = c("HOU",
                                    "TOR",
                                    "GSW",
                                    "BOS",
                                    "PHI",
                                    "CLE",
                                    "POR",
                                    "IND",
                                    "OKC",
                                    "UTA",
                                    "NOP",
                                    "SAS",
                                    "MIN",
                                    "DEN",
                                    "MIA",
                                    "MIL",
                                    "WAS",
                                    "LAC",
                                    "DET",
                                    "CHA",
                                    "LAL",
                                    "NYK",
                                    "BRO",
                                    "CHI",
                                    "SAC",
                                    "ORL",
                                    "ATL",
                                    "DAL",
                                    "MEM",
                                    "PHX"))


saveRDS(standing, file = here::here("data","raw", "standing.rds"))
