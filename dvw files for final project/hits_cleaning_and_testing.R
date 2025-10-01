library(readr)
data <- read_csv("~/COSC_6050_project/marquette_plays.csv")
players <- data |> filter(team == "Marquette University") |> distinct(Name)
p_list <- list(players['Name'])
hits <- subset(data, select = c(Name, attack_code, evaluation_code)) 
hits<- hits[complete.cases(hits), ]
hits <- hits[hits$Name %in% p_list]
