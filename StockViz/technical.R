# technical.R

library(shiny)
library(quantmod)
library(TTR)
library(tidyquant)
library(ggplot2)
library(plotly)

# ---------- UI MODULE ----------
technical_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Technical Analysis"),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          textInput(ns("stock"), "Enter Stock Symbol:", "AAPL"),
          dateRangeInput(ns("date"), "Select Date Range:", 
                         start = Sys.Date() - 365, end = Sys.Date()),
          checkboxGroupInput(ns("indicators"), "Select Technical Indicators:", 
                             choices = list("SMA" = "SMA", "EMA" = "EMA", 
                                            "RSI" = "RSI", "Bollinger Bands" = "BB", 
                                            "MACD" = "MACD"),
                             selected = c("SMA", "EMA")),
          sliderInput(ns("sma_period"), "SMA Period:", min = 5, max = 200, value = 20),
          sliderInput(ns("ema_period"), "EMA Period:", min = 5, max = 200, value = 20),
          sliderInput(ns("bb_std"), "Bollinger Bands Std Dev:", min = 1, max = 3, value = 2, step = 0.1),
          sliderInput(ns("bb_period"), "Bollinger Bands Period:", min = 5, max = 50, value = 20),
          sliderInput(ns("rsi_overbought"), "RSI Overbought Threshold:", min = 50, max = 90, value = 70),
          sliderInput(ns("rsi_oversold"), "RSI Oversold Threshold:", min = 10, max = 50, value = 30),
          actionButton(ns("analyze"), "Analyze")
        )
      ),
      column(
        width = 9,
        h4("Price Chart with Indicators"),
        plotlyOutput(ns("price_chart"), height = "400px"),
        h4("MACD & RSI Chart"),
        plotlyOutput(ns("macd_rsi_chart"), height = "300px")
      )
    )
  )
}

# ---------- SERVER MODULE ----------
technical_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    stock_data <- eventReactive(input$analyze, {
      req(input$stock)
      
      stock <- tryCatch({
        getSymbols(input$stock, src = "yahoo", 
                   from = input$date[1], to = input$date[2], auto.assign = FALSE)
      }, error = function(e) {
        showNotification("Error: Unable to retrieve stock data.", type = "error")
        return(NULL)
      })
      
      if (is.null(stock)) return(NULL)
      
      df <- data.frame(Date = index(stock), Open = as.numeric(Op(stock)), High = as.numeric(Hi(stock)),
                       Low = as.numeric(Lo(stock)), Close = as.numeric(Cl(stock)))
      
      if ("SMA" %in% input$indicators) df$SMA <- as.numeric(SMA(df$Close, n = input$sma_period))
      if ("EMA" %in% input$indicators) df$EMA <- as.numeric(EMA(df$Close, n = input$ema_period))
      if ("RSI" %in% input$indicators) df$RSI <- as.numeric(RSI(df$Close))
      
      if ("BB" %in% input$indicators) {
        bb <- tryCatch({ BBands(df$Close, n = input$bb_period, sd = input$bb_std) }, error = function(e) NULL)
        if (!is.null(bb) && all(c("up", "dn") %in% colnames(bb))) {
          df$BB_Upper <- as.numeric(bb[,"up"])
          df$BB_Lower <- as.numeric(bb[,"dn"])
        } else {
          df$BB_Upper <- NA
          df$BB_Lower <- NA
        }
      } else {
        df$BB_Upper <- NA
        df$BB_Lower <- NA
      }
      
      if ("MACD" %in% input$indicators) {
        macd <- tryCatch({ MACD(df$Close) }, error = function(e) NULL)
        if (!is.null(macd) && all(c("macd", "signal") %in% colnames(macd))) {
          df$MACD <- as.numeric(macd[,"macd"])
          df$MACD_Signal <- as.numeric(macd[,"signal"])
        }
      }
      
      na.omit(df)
    })
    
    output$price_chart <- renderPlotly({
      stock <- stock_data()
      if (is.null(stock)) return(NULL)
      
      p <- ggplot(stock, aes(x = Date)) +
        geom_linerange(aes(ymin = Low, ymax = High), color = "black") +
        geom_segment(aes(y = Open, yend = Close, xend = Date, color = Open < Close), size = 1) +
        scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none")
      
      if ("BB" %in% input$indicators) {
        p <- p + geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "gray70", alpha = 0.3)
      }
      if ("SMA" %in% input$indicators && "SMA" %in% colnames(stock)) 
        p <- p + geom_line(aes(y = SMA, color = "SMA"), size = 0.8)
      if ("EMA" %in% input$indicators && "EMA" %in% colnames(stock)) 
        p <- p + geom_line(aes(y = EMA, color = "EMA"), size = 0.8)
      
      p <- p + labs(title = paste("Stock Price and Indicators for", input$stock),
                    x = "Date", y = "Value") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$macd_rsi_chart <- renderPlotly({
      stock <- stock_data()
      if (is.null(stock)) return(NULL)
      
      p2 <- ggplot(stock, aes(x = Date))
      
      if ("MACD" %in% input$indicators && all(c("MACD", "MACD_Signal") %in% colnames(stock))) {
        p2 <- p2 + geom_line(aes(y = MACD, color = "MACD"), size = 0.8) +
          geom_line(aes(y = MACD_Signal, color = "Signal Line"), size = 0.8)
      }
      if ("RSI" %in% input$indicators && "RSI" %in% colnames(stock)) {
        p2 <- p2 + geom_line(aes(y = RSI, color = "RSI"), size = 0.8) +
          geom_hline(yintercept = input$rsi_overbought, linetype = "dashed", color = "red") +
          geom_hline(yintercept = input$rsi_oversold, linetype = "dashed", color = "green")
      }
      
      p2 <- p2 + labs(title = "MACD and RSI Indicators", x = "Date", y = "Indicator Value") +
        theme_minimal()
      
      ggplotly(p2)
    })
  })
}
