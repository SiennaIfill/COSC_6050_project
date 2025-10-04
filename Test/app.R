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
players <- data |> filter(team == "Marquette University") |> distinct(Name)
players <- na.omit(players)
available_players <- players |> pull(Name)
available_players <- sort(available_players)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Player Patterns"),

    # Dropdown attempt
    selectInput("scout_team","Choose Team to Scout", choices = c("",available_teams), multiple = F),
    
    # this will be redone later
    #selectInput("Player", "Choose Player to Scout", choices = NULL),
    #selectizeInput("scout_player","Choose Player to Scout", choices=c("",available_players)),
    selectizeInput("scout_player","Choose Player to Scout", choices=NULL),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
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
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
