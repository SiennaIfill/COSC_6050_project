data <- read_csv("~/COSC_6050_project/big_east_plays.csv")
teams <- data |> distinct(team)
# this change has been added to all_big_east_games_to_csv.R
data$team <- ifelse(data$team == "Providence College (RI)", "Providence College", data$team)
# this does not work properly right now, messes up other dates
#data$date <- ifelse(data$date == "2025-04-10", "2025-10-04", data$date)
#data$date <- ifelse(data$date == "2025-03-10", "2025-10-03", data$date)
#data$date <- ifelse(data$date == "2025-02-10", "2025-10-02", data$date)
#data$date <- ifelse(data$date == "2025-05-10", "2025-10-05", data$date)

teams <- data |> distinct(team)

# test 1: show only games played by scout (1) and home teams (2) against each other
team1 <- "Marquette University"
team2 <- "DePaul University"

matchup_ids <- data |> distinct(match_id,team,date)
team1_matches <- matchup_ids |> filter(team == team1) 
team1_match_ids <- team1_matches[[1]]
team2_matches <- matchup_ids |> filter(team == team2) 
team2_match_ids <- team2_matches[[1]]

combined_match_ids <- rbind(team1_match_ids,team2_match_ids)
shared_match_ids <- combined_match_ids |> count(match_id) |> filter(n>1) #n = 2 if both teams present
shared_match_ids <- shared_match_ids[[1]]


combined_plays <- data |> filter(match_id %in% shared_match_ids)

# test 2: show scout teams 3 most recent games

ordered_match_ids <- team1_matches[order(team1_matches$date,team1_matches$match_id),] 
last_three_games <- ordered_match_ids |> slice_tail(n=3)
last_three_match_ids <- last_three_games[[1]]

scout_recent_plays <- data |> filter(match_id %in% last_three_match_ids)
