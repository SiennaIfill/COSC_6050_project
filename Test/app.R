#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
#available_teams <- c("marquette","ball_state","florida","jmu","hawaii","san_diego","minnesota","utah_state","uwm","wisconsin","wku")
data <- read_csv("~/COSC_6050_project/marquette_plays.csv")
teams <- data |> distinct(team)
teams <- na.omit(teams)
available_teams <- teams |> pull(team)
players <- data |> filter(team == "Marquette University") |> distinct(Name)
players <- na.omit(players)
available_players <- players |> pull(Name)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Player Patterns"),

    # Dropdown attempt
    selectInput("Team","Choose Team to Scout", choices = available_teams),
    
    # this will be redone later
    selectInput("Player", "Choose Player to Scout", choices = available_players),
    
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
server <- function(input, output) {

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
