## R Shiny Web Application for COSC 6050 project
# Load necessary libraries
library(tidyverse)
library(shiny)
library(readr)
library(dplyr)
# Load necessary data from .csv file, extract teams for first dropdown
data <- read_csv("~/COSC_6050_project/big_east_plays.csv")
teams <- data |> distinct(team)
teams <- na.omit(teams)
available_teams <- teams |>  pull(team)
available_teams <- sort(available_teams)



# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Player Patterns"),
    
    sidebarPanel(
      # Team Select dropdown menu
      selectInput("home_team","Choose Your Team", choices = c("",available_teams), multiple = F),
      
      # Team Select dropdown menu
      selectInput("scout_team","Choose Team to Scout", choices = c("",available_teams), multiple = F),
      
      # Player Select populated in server based on team choice
      selectizeInput("scout_player","Choose Player to Scout", choices=NULL),
      
      # Choose range of data available: all games, team matchups, or last three games
      radioButtons("data_range","Choose Range of Data",
                   choiceNames = c("All of Scout Team's Games","Scout Team Vs. Home Team", "Scout Team's Last Three Games"),
                   choiceValues = c(1,2,3), 
                   selected = 1)
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("court", width = "400px", height = "500px"),
      textOutput("text"),
      tableOutput("table")
    )
    
)

# Define server logic required for application
server <- function(input, output, session) {
  
  # Populate second dropdown based on choice of team to scout
  observeEvent(input$scout_team, {
    if (input$scout_team == ""){
      available_players <- c("")
    }else{
      players <- data |> filter(team == input$scout_team) |> distinct(Name)
      players <- na.omit(players)
      available_players <- players |> pull(Name)
      available_players <- sort(available_players) 
    }
    updateSelectizeInput(session, "scout_player",choices=available_players)
  })
  
    
    
    # Updates plot for court visualization
    plot_data <- eventReactive(input$scout_player, {
      req(input$scout_player != "")
      # Use data decided by radio buttons
      if(input$data_range > 1){
        team1 <- input$scout_team
        team2 <- input$home_team
        
        matchup_ids <- data |> distinct(match_id,team,date)
        team1_matches <- matchup_ids |> filter(team == team1) 
        team1_match_ids <- team1_matches[[1]]
        team2_matches <- matchup_ids |> filter(team == team2) 
        team2_match_ids <- team2_matches[[1]]
        
        # Case for scout team matchups vs home team
        if(input$data_range == 2){
          
          combined_match_ids <- as_tibble(c(team1_match_ids,team2_match_ids))
          shared_match_ids <- combined_match_ids |> count(value) |> filter(n>1) #n = 2 if both teams present
          shared_match_ids <- shared_match_ids[[1]]
          
          data_specified <- data |> filter(match_id %in% shared_match_ids)
          # Case for scout team's most recent three games
        }else if(input$data_range == 3){
          ordered_match_ids <- team1_matches[order(team1_matches$date,team1_matches$match_id),] 
          last_three_games <- ordered_match_ids |> slice_tail(n=3)
          last_three_match_ids <- last_three_games[[1]]
          
          data_specified <- data |> filter(match_id %in% last_three_match_ids)
        }
      }else{data_specified <- data}
      
      
      scout_team_plays <- filter(data_specified, team == input$scout_team)
      hits <- subset(data_specified, select = c(Name, attack_code,AttackPlay, evaluation_code, start_zone, end_zone, skill_subtype)) 
      hits<- hits[complete.cases(hits), ]
      # Get complete list of scout player's hits
      scout_player_hits <- filter(hits, Name == input$scout_player)
      # Get overall kill pct
      kill_num <- scout_player_hits |> filter(evaluation_code=="#") |> tally() |> pull(n)
      total_att <- scout_player_hits |> tally() |> pull(n)
      err_num <- scout_player_hits |> filter(evaluation_code=='=') |> tally() |> pull(n)
      scout_kill_pct <- (kill_num-err_num)/total_att 
      # Get player's top 4 shots
      top_four <- scout_player_hits |> dplyr::count(AttackPlay, skill_subtype, end_zone, sort = TRUE) |> slice(1:4)
      # Create a dataframe with info on each of top 4 shots
      # Rows = shot1, shot2, shot3, shot4
      # Columns = shot type, attempts, kills, errors, kill pct
      if(nrow(top_four) >=4){
        top1_shot <- paste("A",top_four[[1,2]],top_four[[1,1]],"to zone",top_four[[1,3]])
        top1_all <- scout_player_hits |> filter(AttackPlay==top_four[[1,1]], skill_subtype==top_four[[1,2]],end_zone==top_four[[1,3]])
        top1_att <- top1_all |> tally() |> pull(n)
        top1_kill <- top1_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
        top1_err <- top1_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
        top1_pct <- (top1_kill-top1_err)/top1_att
        
        top2_shot <- paste("A",top_four[[2,2]],top_four[[2,1]],"to zone",top_four[[2,3]])
        top2_all <- scout_player_hits |> filter(AttackPlay==top_four[[2,1]], skill_subtype==top_four[[2,2]],end_zone==top_four[[2,3]])
        top2_att <- top2_all |> tally() |> pull(n)
        top2_kill <- top2_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
        top2_err <- top2_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
        top2_pct <- (top2_kill-top2_err)/top2_att
        
        top3_shot <- paste("A",top_four[[3,2]],top_four[[3,1]],"to zone",top_four[[3,3]])
        top3_all <- scout_player_hits |> filter(AttackPlay==top_four[[3,1]], skill_subtype==top_four[[3,2]],end_zone==top_four[[3,3]])
        top3_att <- top3_all |> tally() |> pull(n)
        top3_kill <- top3_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
        top3_err <- top3_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
        top3_pct <- (top3_kill-top3_err)/top3_att
        
        top4_shot <- paste("A",top_four[[4,2]],top_four[[4,1]],"to zone",top_four[[4,3]])
        top4_all <- scout_player_hits |> filter(AttackPlay==top_four[[4,1]], skill_subtype==top_four[[4,2]],end_zone==top_four[[4,3]])
        top4_att <- top4_all |> tally() |> pull(n)
        top4_kill <- top4_all |> filter(evaluation_code=="#") |> tally() |> pull(n)
        top4_err <- top4_all |> filter(evaluation_code=="=") |> tally() |> pull(n)
        top4_pct <- (top4_kill-top4_err)/top4_att
        
        Shot <- c(top1_shot,top2_shot,top3_shot,top4_shot,"All attempts")
        Attempts <- c(top1_att,top2_att,top3_att,top4_att,total_att)
        Kills <- c(top1_kill,top2_kill,top3_kill,top4_kill,kill_num)
        Errors <- c(top1_err,top2_err,top3_err,top4_err,err_num)
        Kill_Percentage <- c(top1_pct,top2_pct,top3_pct,top4_pct,scout_kill_pct)
        Shot_Chart <- data.frame(Shot,Attempts,Kills,Errors,Kill_Percentage)
        
        # Render table with shot chart
        output$text <- renderText("Table Rendered:")
        output$table <- renderTable(Shot_Chart)
        update
      }else{
        output$text <- renderText("Insufficient Data for Shot Chart")
        # If attempts but not enough shots, show overall stats
        if(total_att >0){
          Shot <- c("All attempts")
          Attempts <- c(total_att)
          Kills <- c(kill_num)
          Errors <- c(err_num)
          Kill_Percentage <- c(scout_kill_pct)
          Shot_Chart <- data.frame(Shot,Attempts,Kills,Errors,Kill_Percentage)
          output$table <- renderTable(Shot_Chart)
        }else{
          # No attempts condition
          Shot <- c("All attempts")
          Attempts <- c(0)
          Kills <- c(0)
          Errors <- c(0)
          Kill_Percentage <- c(0.00)
          Shot_Chart <- data.frame(Shot,Attempts,Kills,Errors,Kill_Percentage)
          output$table <- renderTable(Shot_Chart)
        }
      }
      
      if(nrow(top_four) >=4){
        plot_4 <- top_four
        plot_4$pct <- c(top1_pct, top2_pct, top3_pct, top4_pct)
        
        plot_4 <- plot_4 |> mutate(
          start_x = case_when(
            AttackPlay == "Go" ~ 19.5,
            AttackPlay == "Hut" ~ 16,
            AttackPlay == "X" ~ 1.5
          ),
          end_x = case_when(
            end_zone %in% c(1, 9, 2) ~ 16,
            end_zone %in% c(6, 8, 3) ~ 10,
            end_zone %in% c(5, 7, 4) ~ 4
          ),
          end_y = case_when(
            end_zone %in% c(5, 6, 1) ~ 5,
            end_zone %in% c(7, 8, 9) ~ 15,
            end_zone %in% c(4, 3, 2) ~ 25
          ),
          color = case_when(
            pct <= 0.150 ~ 'darkred',
            pct > 0.15 & pct <= 0.3 ~ 'yellow',
            pct > 0.3 ~ 'darkgreen'
          )
        )
        
        plot_4
      }
    })
    
    # Actually creates plot when necessary
    output$court <- renderPlot({
      x <- 1:20
      y <- rep(20, 20)
      
      plot(x, y, type = "l", xlim = c(2, 19), ylim = c(0, 29), xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
      
      plot_4 <- plot_data()
      if (!is.null(plot_4) && nrow(plot_4) >= 4) {
        start_y <- 30
        for (i in 1:4) {
          arrows(plot_4[[i, "start_x"]], start_y, plot_4[[i, "end_x"]], plot_4[[i, "end_y"]],
                 length = 0.05, lwd = 2, col = plot_4[[i, "color"]])
        }
      }
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)
