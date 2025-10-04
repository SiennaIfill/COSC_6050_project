library(readr)
library(dplyr)
# load data
data <- read_csv("~/COSC_6050_project/marquette_plays.csv")
# choose team to select players from
players <- data |> filter(team == "Marquette University") |> distinct(Name)
players <- na.omit(players)
p_list <- list(players['Name'])
# choose players that hit
scout_team_plays <- filter(data, team == "Marquette University")
hits <- subset(scout_team_plays, select = c(Name, attack_code,AttackPlay, evaluation_code, start_zone, end_zone, skill_subtype)) 
hits<- hits[complete.cases(hits), ]
# subset to require a certain number of hits-- come back to this later
hit_count <- hits |> count(Name)
# get list of scout player's hits
scout_player_hits <- filter(hits, Name == "Natalie Ring")
#get kill pct
kill_num <- scout_player_hits |> filter(evaluation_code=="#") |> tally() |> pull(n)
total_att <- scout_player_hits |> tally() |> pull(n)
err_num <- scout_player_hits |> filter(evaluation_code=='=') |> tally() |> pull(n)
scout_kill_pct <- (kill_num-err_num)/total_att 
# further specification-- goal is top 4 shots
top_4 <- scout_player_hits |> count(AttackPlay, skill_subtype, end_zone, sort = TRUE) |> slice(1:4)
View(top_4)
# create a dataframe with info on each of top 4 shots
# rows = shot1, shot2, shot3, shot4
# columns = attempts, kills, errors, kill pct
top1_shot <- paste("A",top_4[[1,2]],top_4[[1,1]],"to zone",top_4[[1,3]])
top1_all <- scout_player_hits |> filter(AttackPlay==top_4[[1,1]], skill_subtype==top_4[[1,2]],end_zone==top_4[[1,3]])
top1_att <- top1_all |> tally() |> pull(n)
top1_kill <- top1_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
top1_err <- top1_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
top1_pct <- (top1_kill-top1_err)/top1_att

top2_shot <- paste("A",top_4[[2,2]],top_4[[2,1]],"to zone",top_4[[2,3]])
top2_all <- scout_player_hits |> filter(AttackPlay==top_4[[2,1]], skill_subtype==top_4[[2,2]],end_zone==top_4[[2,3]])
top2_att <- top2_all |> tally() |> pull(n)
top2_kill <- top2_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
top2_err <- top2_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
top2_pct <- (top2_kill-top2_err)/top2_att

top3_shot <- paste("A",top_4[[3,2]],top_4[[3,1]],"to zone",top_4[[3,3]])
top3_all <- scout_player_hits |> filter(AttackPlay==top_4[[3,1]], skill_subtype==top_4[[3,2]],end_zone==top_4[[3,3]])
top3_att <- top3_all |> tally() |> pull(n)
top3_kill <- top3_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
top3_err <- top3_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
top3_pct <- (top3_kill-top3_err)/top3_att

top4_shot <- paste("A",top_4[[4,2]],top_4[[4,1]],"to zone",top_4[[4,3]])
top4_all <- scout_player_hits |> filter(AttackPlay==top_4[[4,1]], skill_subtype==top_4[[4,2]],end_zone==top_4[[4,3]])
top4_att <- top4_all |> tally() |> pull(n)
top4_kill <- top4_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
top4_err <- top4_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
top4_pct <- (top4_kill-top4_err)/top4_att

Shot <- c(top1_shot,top2_shot,top3_shot,top4_shot)
Attempts <- c(top1_att,top2_att,top3_att,top4_att)
Kills <- c(top1_kill,top2_kill,top3_kill,top4_kill)
Errors <- c(top1_err,top2_err,top3_err,top4_err)
Kill_Percentage <- c(top1_pct,top2_pct,top3_pct,top4_pct)
Shot_Chart <- data.frame(Shot,Attempts,Kills,Errors,Kill_Percentage)
View(Shot_Chart)
