#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyverse)
library(shiny)
library(readr)
library(dplyr)
data <- read_csv("~/COSC_6050_project/marquette_plays.csv")
teams <- data |> distinct(team)
teams <- na.omit(teams)
available_teams <- teams |>  pull(team)
available_teams <- sort(available_teams)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Player Patterns"),

    # Team Select dropdown menu
    selectInput("scout_team","Choose Team to Scout", choices = c("",available_teams), multiple = F),
    
    # Player Select populated in server based on team choice
    selectizeInput("scout_player","Choose Player to Scout", choices=NULL),
    
    
    
    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
        #sidebarPanel(
            #sliderInput("bins",
                        #"Number of bins:",
                        #min = 1,
                        #max = 50,
                        #value = 30)
        #),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("text"),
          #plotOutput("distPlot"),
          tableOutput("table")
        )
    #)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
  
    observeEvent(input$scout_player, {
      if(input$scout_player != ""){
        output$text <- renderText(input$scout_player)
        scout_team_plays <- filter(data, team == input$scout_team)
        hits <- subset(data, select = c(Name, attack_code,AttackPlay, evaluation_code, start_zone, end_zone, skill_subtype)) 
        hits<- hits[complete.cases(hits), ]
        # get list of scout player's hits
        scout_player_hits <- filter(hits, Name == input$scout_player)
        #get kill pct
        kill_num <- scout_player_hits |> filter(evaluation_code=="#") |> tally() |> pull(n)
        total_att <- scout_player_hits |> tally() |> pull(n)
        err_num <- scout_player_hits |> filter(evaluation_code=='=') |> tally() |> pull(n)
        scout_kill_pct <- (kill_num-err_num)/total_att 
        # further specification-- goal is top 4 shots
        top_4 <- scout_player_hits |> count(AttackPlay, skill_subtype, end_zone, sort = TRUE) |> slice(1:4)
        # create a dataframe with info on each of top 4 shots
        # rows = shot1, shot2, shot3, shot4
        # columns = attempts, kills, errors, kill pct
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
        
        #attempt to initiate table
        output$text <- renderText("Table Rendered:")
        output$table <- renderTable(Shot_Chart)
        update
        }else{
        output$text <- renderText("Insufficient Data for Shot Chart")
          if(total_att >0){
            Shot <- c("All attempts")
            Attempts <- c(total_att)
            Kills <- c(kill_num)
            Errors <- c(err_num)
            Kill_Percentage <- c(scout_kill_pct)
            Shot_Chart <- data.frame(Shot,Attempts,Kills,Errors,Kill_Percentage)
            output$table <- renderTable(Shot_Chart)
          }else{
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
    
    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
             #xlab = 'Waiting time to next eruption (in mins)',
             #main = 'Histogram of waiting times')
    #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
