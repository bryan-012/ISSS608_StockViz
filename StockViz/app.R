#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(PerformanceAnalytics)
library(quantmod)
library(tseries)

stocks <- new.env()

stocks_names <- c("GOOG", "AMZN", "AAPL", "MSFT", "IBM")

getSymbols(stocks_names, 
           env = stocks, 
           from = as.Date('2015-01-01'), 
           to = as.Date('2020-01-01'))

# Extraction des prix de clôture de chaque titre pour en former un seul ensemble de données au format xts

close_prices <- do.call(merge, lapply(stocks, Cl))

# Affichage des premières observations

head(close_prices)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

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
