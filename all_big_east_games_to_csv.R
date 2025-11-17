## Load all games and save into a single .csv file of all plays 

# Load necessary libraries
library(lubridate)
library(datavolley)
library(tidyverse)
library(dplyr)
# Set working directory to retrieve files
setwd("~/COSC_6050_project/big east games")

# List all .dvw files
pattern <- paste(".dvw")
files <- list.files(pattern = pattern)
files

# Read the first files and initialize the dataset
x <- read_dv(files[1], insert_technical_timeouts = FALSE)
game <- x$plays # Extract plays
game$date <- as_date(x$meta$match$date) # Add game date
# Loop through remaining files and combine their plays into `game`
for (i in 2:length(files)) {
  temp <- read_dv(files[i], insert_technical_timeouts = FALSE)
  hold <- temp$plays
  hold$date <- as_date(temp$meta$match$date)
  game <- rbind(game, hold)
}
# Begin to construct valid datavolley object
game_dvw <- read_dv(files[1], insert_technical_timeouts = FALSE) # Reload the first file as a template
# Construct a valid datavolley object
game_dvw$plays <- game # Replace plays with the combined data
game_dvw$meta$match$date <- Sys.Date() # Update metadata date
game_dvw$meta$match$id <- paste("All Big East Games") # Assign an ID
game_dvw$meta$match$description <- "Combined all Big East conference games" # Optional description
# Combine into one object and clean data
all_plays <- game_dvw$plays
# Add attack description, set success, and correct rotations
all_plays<- all_plays|>  
  dplyr::rename(Name=player_name,
         AttackPlay=attack_description)  |> 
  mutate(Set_Success = case_when(
    skill == "Set" & lead(skill) == "Attack" & lead(evaluation) == "Winning attack" ~ "Win",
    skill == "Set" & lead(skill) == "Attack" & (lead(evaluation) == "Blocked" | lead(evaluation) == "Error") ~ "Loss",
    TRUE ~ "InPlay"  
  )) |>
  mutate(
    HomeRO = case_when(
      home_setter_position == 2 ~ 6,
      home_setter_position == 3 ~ 5,
      home_setter_position == 5 ~ 3,
      home_setter_position == 6 ~ 2,
      TRUE ~ home_setter_position),
    VisitRO = case_when(
      visiting_setter_position == 2 ~ 6,
      visiting_setter_position == 3 ~ 5,
      visiting_setter_position == 5 ~ 3,
      visiting_setter_position == 6 ~ 2,
      TRUE ~ visiting_setter_position)) |>
  mutate(
    Phase = case_when(
      phase == 'Reception' ~ 'FBSO',
      phase == 'Transition' ~ 'Trans',
      TRUE ~ as.character(phase)
    )
  )

# Additional cleaning
all_plays$team <- ifelse(all_plays$team == "Providence College (RI)", "Providence College", all_plays$team)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Hut", "5", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Red", "9", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "X", "Red", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "O hits a 2nd step 4", "Red", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Quick in front (4)", "C", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Quick in Center", "C", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Quick ball back(5)", "C", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "O hits in middle", "2", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "2", "B", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Slide moved from S", "Slide", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Slide near S", "Slide", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "Slide by the opposite", "Slide", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "D", "Eye", all_plays$AttackPlay)
all_plays$AttackPlay <- ifelse(all_plays$AttackPlay == "High D", "Eye", all_plays$AttackPlay)

# Write/export as .csv file for use in app.R
write.csv(all_plays,"~/COSC_6050_project/big_east_plays.csv", row.names = FALSE)
library(readr)
big_east_plays <- read_csv("~/COSC_6050_project/big_east_plays.csv")
View(big_east_plays)
