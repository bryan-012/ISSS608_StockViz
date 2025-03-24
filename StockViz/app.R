library(shiny)

source("technical.R")
source("forecast.R")
source("cda.R")

ui <- navbarPage(
  title = "Stock Analytics App",
  
  tabPanel("Technical Analysis", 
           technical_ui("tech_module")),
  
  tabPanel("Time Series Forecasting", 
           forecast_ui("forecast_module")),
  
  tabPanel("CDA + EDA",
           cda_ui("cda"))
)

server <- function(input, output, session) {
  technical_server("tech_module")
  forecast_server("forecast_module")
  cda_server("cda")
}

shinyApp(ui, server)
