library(shiny)

source("technical.R")
source("forecast.R")
source("cda_module.R")

ui <- navbarPage(
  title = "Stock Analytics App",
  
  tabPanel("Technical Analysis", 
           technical_ui("tech_module")),
  
  tabPanel("Time Series Forecasting", 
           forecast_ui("forecast_module")),
  
  tabPanel("CDA + EDA",  # 👈 Add new tab
           cda_ui("cda_module"))
)

server <- function(input, output, session) {
  technical_server("tech_module")
  forecast_server("forecast_module")
  cda_server("cda_module")
}

shinyApp(ui, server)
