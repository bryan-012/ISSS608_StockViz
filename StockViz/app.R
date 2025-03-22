library(shiny)

source("technical.R")
source("forecast.R")

ui <- navbarPage(
  title = "Stock Analytics App",
  
  tabPanel("Technical Analysis", 
           technical_ui("tech_module")),
  
  tabPanel("Time Series Forecasting", 
           forecast_ui("forecast_module"))
)

server <- function(input, output, session) {
  technical_server("tech_module")
  forecast_server("forecast_module")
}

shinyApp(ui, server)
