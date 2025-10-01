library(readr)
library(dplyr)
# load data
data <- read_csv("~/COSC_6050_project/marquette_plays.csv")
# choose team to select players from
players <- data |> filter(team == "Marquette University") |> distinct(Name)
players <- na.omit(players)
p_list <- list(players['Name'])
# choose players that hit
MU_plays <- filter(data, team == "Marquette University")
hits <- subset(MU_plays, select = c(Name, attack_code,AttackPlay, evaluation_code, start_zone, end_zone, skill_subtype)) 
hits<- hits[complete.cases(hits), ]
# subset to require a certain number of hits-- come back to this later
hit_count <- hits |> count(Name)
# get list of scout player's hits
Nat_hits <- filter(hits, Name == "Emma Parks")
#get kill pct
kill_num <- Nat_hits |> filter(evaluation_code=="#") |> tally() |> pull(n)
total_att <- Nat_hits |> tally() |> pull(n)
err_num <- Nat_hits |> filter(evaluation_code=='=') |> tally() |> pull(n)
Nat_kill_pct <- (kill_num-err_num)/total_att 
# further specification-- goal is top 4 shots
Nat_hits |> count(AttackPlay, skill_subtype, end_zone, sort = TRUE)
