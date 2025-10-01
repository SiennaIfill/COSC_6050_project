# Load games and create combined .dvw files, separated into Home and Away
library(lubridate)
library(datavolley)
library(tidyverse)
library(plyr)

# Set working directory to retrieve files
setwd("~/COSC_6050_project/dvw files for final project")

# Establish team to scout
scout_team <- "marquette"
# List all .dvw files
# Save all their home games
home_pattern <- paste("at_",scout_team,sep="")
home_files <- list.files(pattern = home_pattern)
home_files
away_pattern <- paste(scout_team,"_at",sep = "")
away_files <- list.files(pattern= away_pattern)
away_files


# Read the first files and initialize each dataset
x <- read_dv(home_files[1], insert_technical_timeouts = FALSE)
home_game <- x$plays # Extract plays
home_game$date <- as_date(x$meta$match$date) # Add game date
y <- read_dv(away_files[1], insert_technical_timeouts = FALSE)
away_game <- y$plays # Extract plays
away_game$date <- as_date(y$meta$match$date) # Add game date

# Loop through remaining files and combine their plays into `home_game`
for (i in 2:length(home_files)) {
  temp <- read_dv(home_files[i], insert_technical_timeouts = FALSE)
  hold <- temp$plays
  hold$date <- as_date(temp$meta$match$date)
  home_game <- rbind(home_game, hold)
}
# Loop through remaining files and combine their plays into `away_game`
for (i in 2:length(away_files)) {
  temp <- read_dv(away_files[i], insert_technical_timeouts = FALSE)
  hold <- temp$plays
  hold$date <- as_date(temp$meta$match$date)
  away_game <- rbind(away_game, hold)
}

# Begin to construct valid datavolley objects
home_game_dvw <- read_dv(home_files[1], insert_technical_timeouts = FALSE) # Reload the first file as a template
away_game_dvw <- read_dv(away_files[1], insert_technical_timeouts = FALSE) # Reload the first file as a template

# Construct a valid datavolley object
home_game_dvw$plays <- home_game # Replace plays with the combined data
home_game_dvw$meta$match$date <- Sys.Date() # Update metadata date
home_game_dvw$meta$match$id <- paste(scout_team,"_Home_Games_Combined", sep="") # Assign an ID
home_game_dvw$meta$match$description <- "Combined home game data" # Optional description
# Construct another valid datavolley objectclas
away_game_dvw$plays <- away_game # Replace plays with the combined data
away_game_dvw$meta$match$date <- Sys.Date() # Update metadata date
away_game_dvw$meta$match$id <- paste(scout_team,"_Away_Games_Combined", sep="") # Assign an ID
away_game_dvw$meta$match$description <- "Combined away game data" # Optional description


# clean data
home_plays <- home_game_dvw$plays
away_plays <- away_game_dvw$plays
all_plays <- rbind.fill(home_plays,away_plays)

all_plays<- all_plays|>  
  rename(Name=player_name,
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

pr_attack <- all_plays |> 
  subset(skill=="Attack")  |> 
  mutate(Area = case_when(
    start_zone %in% c(2, 3, 4) ~ 'frontrow',
    TRUE ~ 'backrow'
  ))  |> 
  mutate(
    Evaluation = case_when(
      evaluation == 'Winning attack' ~ 'Kill',
      evaluation == 'Positive, good attack' ~ 'Positive',
      evaluation == 'Blocked for reattack' ~ 'Medium',
      evaluation == 'Poor, easily dug' ~ 'Poor',
      TRUE ~ as.character(evaluation)  # Keeps other values unchanged
    )
  )  |> 
  mutate(
    ShotPace = case_when(
      skill_subtype == 'Hard spike' ~ 'Hard',
      skill_subtype == 'Soft spike/topspin' ~ 'Roll',
      skill_subtype == 'Tip' ~ 'Tip',
      TRUE ~ as.character(skill_subtype)
    )
  ) 

pr_set <- all_plays |> 
  subset(skill=="Set")  |> 
  mutate(
    Set_Location = case_when(
      set_type == 'F' ~ 'Left',
      set_type == 'B' ~ 'Right',
      set_type == 'C' ~ 'Middle',
      set_type == 'P' ~ 'Pipe',
      set_type == 'S' ~ 'Dump',
      TRUE ~ as.character(set_type) 
    )
  ) 

pr_serve <- all_plays |> 
  subset(skill=="Serve")  |> 
  mutate(
    Evaluation = case_when(
      evaluation == 'OK, no first tempo possible' ~ 'Medium',
      evaluation == 'Positive, no attack' ~ 'Overpass',
      evaluation == 'Positive, opponent some attack' ~ 'Good',
      evaluation == 'Negative, opponent free attack' ~ 'Poor',
      TRUE ~ as.character(evaluation)  
    )
  )  |> 
  mutate(
    ServeType = case_when(
      skill_type == 'Jump serve' ~ 'Topspin',
      skill_type == 'Jump-float serve' ~ 'Float',
      TRUE ~ as.character(skill_type)
    )
  )

pr_reception <- all_plays|>
  subset(skill=="Reception") |>
  mutate(PassingGrade = case_when(
    evaluation == 'OK, no first tempo possible' ~ 'Medium',
    evaluation == 'Poor, no attack' ~ 'Overpass',
    evaluation == 'Negative, limited attack' ~ 'Poor',
    evaluation == 'Positive, attack' ~ 'Positive',
    TRUE ~ as.character(evaluation)
  )) |>
  mutate(PassType = case_when(
    skill_type == 'Jump serve reception' ~ 'Topspin',
    skill_type == 'Jump-float serve reception' ~ 'Float',
    TRUE ~ as.character(skill_type)
  ))

#write as csv in case that's helpful?
write.csv(all_plays,"~/COSC_6050_project/marquette_plays.csv", row.names = FALSE)
