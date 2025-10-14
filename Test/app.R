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

    # Team Select dropdown menu
    selectInput("scout_team","Choose Team to Scout", choices = c("",available_teams), multiple = F),
    
    # Player Select populated in server based on team choice
    selectizeInput("scout_player","Choose Player to Scout", choices=NULL),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("court", width = "65%", height = "400px"),
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
  
    # Create Shot Chart table based on 4 most popular shots of scout player selected 
    observeEvent(input$scout_player, {
      if(input$scout_player != ""){
        output$text <- renderText(input$scout_player)
        scout_team_plays <- filter(data, team == input$scout_team)
        hits <- subset(data, select = c(Name, attack_code,AttackPlay, evaluation_code, start_zone, end_zone, skill_subtype)) 
        hits<- hits[complete.cases(hits), ]
        # Get complete list of scout player's hits
        scout_player_hits <- filter(hits, Name == input$scout_player)
        # Get overall kill pct
        kill_num <- scout_player_hits |> filter(evaluation_code=="#") |> tally() |> pull(n)
        total_att <- scout_player_hits |> tally() |> pull(n)
        err_num <- scout_player_hits |> filter(evaluation_code=='=') |> tally() |> pull(n)
        scout_kill_pct <- (kill_num-err_num)/total_att 
        # Get player's top 4 shots
        top_4 <- scout_player_hits |> dplyr::count(AttackPlay, skill_subtype, end_zone, sort = TRUE) |> slice(1:4)
        # Create a dataframe with info on each of top 4 shots
        # Rows = shot1, shot2, shot3, shot4
        # Columns = shot type, attempts, kills, errors, kill pct
        if(nrow(top_4) >=4){
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
      }else{
        output$text <- renderText("Select player to view Shot Chart")
      }
    })
    
    observeEvent(input$scout_player, {
      x = c(1:20)
      y= c(20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20)
      output$court <- renderPlot({
        plot(x,y,type = "l", xlim =c(2,19), ylim = c(0,29), xlab = "", ylab="")}) #xaxt='n',yaxt='n'
      
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
