# cda.R

library(tidyquant)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(gt)
library(tidyr)
library(lubridate)
library(corrplot)
library(zoo)
library(DT)

# --- UI Module ---
cda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("User Portfolio Input"),
        textInput(ns("stock_symbol"), "Stock Symbol (e.g., AAPL)", value = ""),
        numericInput(ns("quantity"), "Number of Shares", value = 10, min = 1),
        dateInput(ns("buy_date"), "Buy Date", value = Sys.Date() - 30),
        textInput(ns("buy_price"), "Buy Price (optional)", value = ""),
        actionButton(ns("add_stock"), "+ Add to Portfolio"),
        actionButton(ns("remove_selected"), "❌ Remove Selected Entries"),
        br(),
        DTOutput(ns("portfolio_table")),
        br(),
        downloadButton(ns("download_summary"), "📄 Download Portfolio Summary")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("📊 EDA",
                   plotlyOutput(ns("line_chart")),
                   plotlyOutput(ns("total_portfolio_plot")),
                   plotOutput(ns("rolling_vol_plot")),
                   plotOutput(ns("daily_return_corr_plot")),
                   plotOutput(ns("return_volatility_plot")),
                   plotOutput(ns("monthly_volume_plot"))
          ),
          tabPanel("📋 Portfolio Summary",
                   gt_output(ns("portfolio_summary_table"))
          ),
          tabPanel("📈 CDA",
                   selectInput(ns("cda_stock1"), "Select Stock 1:", choices = NULL),
                   selectInput(ns("cda_stock2"), "Select Stock 2:", choices = NULL),
                   verbatimTextOutput(ns("wilcox_test")),
                   verbatimTextOutput(ns("f_test")),
                   verbatimTextOutput(ns("correlation_test")),
                   verbatimTextOutput(ns("regression_summary")),
                   verbatimTextOutput(ns("anova1")),
                   verbatimTextOutput(ns("anova2")),
                   plotlyOutput(ns("event_study_plot"))
          )
        )
      )
    )
  )
}
# --- SERVER Module ---
cda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      portfolio_input = data.frame(symbol=character(), buy_date=as.Date(character()), quantity=numeric(), buy_price=numeric(), stringsAsFactors=FALSE),
      stock_data = NULL,
      portfolio_tracking_all = NULL,
      portfolio_summary = NULL
    )
    
    observeEvent(input$add_stock, {
      buy_price <- ifelse(input$buy_price == "", NA, as.numeric(input$buy_price))
      new_entry <- data.frame(
        symbol = toupper(input$stock_symbol),
        buy_date = input$buy_date,
        quantity = input$quantity,
        buy_price = buy_price
      )
      rv$portfolio_input <- rbind(rv$portfolio_input, new_entry)
      updateSelectInput(session, "cda_stock1", choices = unique(rv$portfolio_input$symbol))
      updateSelectInput(session, "cda_stock2", choices = unique(rv$portfolio_input$symbol))
    })
    
    output$portfolio_table <- renderDT({
      df <- rv$portfolio_input
      if (nrow(df) > 0) {
        df$buy_date <- format(as.Date(df$buy_date), "%Y-%m-%d")
        df$buy_price <- ifelse(is.na(df$buy_price), "NA (System Uses Market Price)", paste0("$", round(as.numeric(df$buy_price), 2)))
      }
      datatable(df, selection = 'multiple', options = list(pageLength = 5))
    })
    
    observeEvent(input$remove_selected, {
      selected_rows <- input$portfolio_table_rows_selected
      if (!is.null(selected_rows)) {
        rv$portfolio_input <- rv$portfolio_input[-selected_rows, ]
      }
    })
    
    observe({
      if (nrow(rv$portfolio_input) == 0) return(NULL)
      tryCatch({
        stock_symbols <- unique(rv$portfolio_input$symbol)
        data_start <- min(rv$portfolio_input$buy_date, na.rm = TRUE)
        stock_data_raw <- tq_get(stock_symbols, from = data_start, to = Sys.Date())
        
        if (nrow(stock_data_raw) == 0) stop("Stock data retrieval failed.")
        
        rv$stock_data <- stock_data_raw %>%
          group_by(symbol, date) %>%
          summarise(
            open = first(open),
            high = max(high, na.rm = TRUE),
            low = min(low, na.rm = TRUE),
            close = last(close),
            volume = sum(volume, na.rm = TRUE),
            adjusted = last(adjusted),
            .groups = "drop"
          ) %>%
          group_by(symbol) %>%
          arrange(date) %>%
          mutate(daily_return = adjusted / lag(adjusted) - 1) %>%
          ungroup()
        
        portfolio_results <- list()
        summary_list <- list()
        
        for (i in 1:nrow(rv$portfolio_input)) {
          stock <- rv$portfolio_input$symbol[i]
          buy_date <- rv$portfolio_input$buy_date[i]
          qty <- rv$portfolio_input$quantity[i]
          buy_price_user <- rv$portfolio_input$buy_price[i]
          
          stock_prices <- rv$stock_data %>% filter(symbol == stock)
          valid_buy_date <- max(stock_prices$date[stock_prices$date <= buy_date])
          
          if (is.infinite(valid_buy_date) | is.na(valid_buy_date)) next
          
          day_row <- stock_prices %>% filter(date == valid_buy_date)
          buy_price <- ifelse(!is.na(buy_price_user), buy_price_user, median(day_row$adjusted, na.rm = TRUE))
          buy_price_src <- ifelse(!is.na(buy_price_user), "User Input", "Market Price")
          
          track <- stock_prices %>% filter(date >= valid_buy_date) %>%
            mutate(
              return_since_buy = (adjusted / buy_price) - 1,
              stock = stock,
              buy_date = valid_buy_date,
              quantity = qty,
              buy_price = buy_price,
              holding_value = adjusted * qty,
              invested_amount = qty * buy_price,
              unrealized_return = (adjusted - buy_price) * qty,
              position_id = paste0(stock, "_", valid_buy_date)
            )
          
          portfolio_results[[i]] <- track
          
          summary_list[[i]] <- tibble(
            stock = stock,
            buy_date = valid_buy_date,
            quantity = qty,
            buy_price = buy_price,
            buy_price_src = buy_price_src,
            invested_amount = qty * buy_price,
            current_price = tail(track$adjusted, 1),
            current_value = tail(track$holding_value, 1),
            unrealized_return = tail(track$unrealized_return, 1),
            return_pct = tail(track$return_since_buy, 1)
          )
        }
        
        rv$portfolio_tracking_all <- bind_rows(portfolio_results)
        rv$portfolio_summary <- bind_rows(summary_list)
        
      }, error = function(e) {
        showNotification("❌ Error fetching stock data.", type = "error")
      })
    })
    
    output$portfolio_summary_table <- render_gt({
      if (is.null(rv$portfolio_summary)) return(NULL)
      
      df <- rv$portfolio_summary %>%
        mutate(
          `Return (%)` = percent(return_pct),
          `Unrealized Return ($)` = dollar(unrealized_return)
        ) %>%
        select(
          Stock = stock, `Buy Date` = buy_date, Quantity = quantity,
          `Buy Price ($)` = buy_price, `Buy Price Source` = buy_price_src,
          `Invested Amount ($)` = invested_amount, `Current Price ($)` = current_price,
          `Current Value ($)` = current_value, `Unrealized Return ($)`, `Return (%)`
        )
      
      gt(df) %>% tab_header(title = "📋 Portfolio Summary")
    })
    
    output$download_summary <- downloadHandler(
      filename = function() paste0("Portfolio_Summary_", Sys.Date(), ".csv"),
      content = function(file) write.csv(rv$portfolio_summary, file, row.names = FALSE)
    )
    
    output$line_chart <- renderPlotly({
      if (is.null(rv$portfolio_tracking_all)) return(NULL)
      p <- ggplot(rv$portfolio_tracking_all, aes(x = date, y = holding_value, color = position_id)) +
        geom_line() + labs(title="Stock Value Over Time", x="Date", y="Holding Value ($)") +
        scale_y_continuous(labels=dollar)
      ggplotly(p)
    })
    
    output$total_portfolio_plot <- renderPlotly({
      if (is.null(rv$portfolio_tracking_all)) return(NULL)
      total <- rv$portfolio_tracking_all %>% group_by(date) %>% summarise(total_value=sum(holding_value))
      ggplotly(ggplot(total, aes(x=date, y=total_value)) +
                 geom_line(color="forestgreen") +
                 labs(title="Total Portfolio Value", y="Value ($)") +
                 scale_y_continuous(labels=dollar))
    })
    
    output$rolling_vol_plot <- renderPlot({
      df <- rv$stock_data %>%
        group_by(symbol) %>%
        arrange(date) %>%
        mutate(rolling_vol = rollapply(daily_return, 20, sd, fill=NA, align="right")) %>%
        ungroup()
      ggplot(df %>% filter(!is.na(rolling_vol)), aes(x=date, y=rolling_vol, color=symbol)) +
        geom_line() +
        labs(title="20-Day Rolling Volatility (SD)", y="Volatility", x="Date") +
        scale_y_continuous(labels=percent)
    })
    
    output$daily_return_corr_plot <- renderPlot({
      cor_data <- rv$stock_data %>% select(symbol, date, daily_return) %>%
        filter(!is.na(daily_return)) %>%
        pivot_wider(names_from=symbol, values_from=daily_return)
      corrplot(cor(cor_data[,-1], use="pairwise.complete.obs"), method="color")
    })
    
    output$return_volatility_plot <- renderPlot({
      df <- rv$stock_data %>%
        group_by(symbol) %>%
        summarise(mean_return=mean(daily_return, na.rm=TRUE),
                  volatility=sd(daily_return, na.rm=TRUE))
      ggplot(df, aes(x=volatility, y=mean_return, label=symbol)) +
        geom_point(size=4) + geom_text(nudge_y=0.001) +
        labs(title="Return vs Volatility", x="Volatility (SD)", y="Mean Return") +
        scale_x_continuous(labels=percent) +
        scale_y_continuous(labels=percent)
    })
    
    output$monthly_volume_plot <- renderPlot({
      df <- rv$stock_data %>%
        filter(date >= Sys.Date() %m-% years(5)) %>%
        mutate(year=year(date), month=month(date, label=TRUE)) %>%
        group_by(symbol, year, month) %>%
        summarise(total_volume=sum(volume, na.rm=TRUE), .groups="drop")
      ggplot(df, aes(x=month, y=total_volume, fill=factor(year))) +
        geom_col(position="dodge") +
        facet_wrap(~symbol, scales="free_y") +
        labs(title="📊 Monthly Trading Volume by Stock (Past 5 Years)", x="Month", y="Total Volume", fill="Year") +
        scale_y_continuous(labels=comma)
    })
    
    # --- CDA Tests ---
    output$wilcox_test <- renderPrint({
      df <- rv$stock_data %>% filter(symbol %in% c(input$cda_stock1, input$cda_stock2))
      if (nrow(df) == 0) return("No data")
      wilcox.test(daily_return ~ symbol, data = df)
    })
    
    output$f_test <- renderPrint({
      df <- rv$stock_data %>% filter(symbol %in% c(input$cda_stock1, input$cda_stock2))
      var.test(daily_return ~ symbol, data=df)
    })
    
    output$correlation_test <- renderPrint({
      df <- rv$stock_data %>%
        select(date, symbol, daily_return) %>%
        filter(symbol %in% c(input$cda_stock1, input$cda_stock2)) %>%
        pivot_wider(names_from = symbol, values_from = daily_return)
      cor.test(df[[input$cda_stock1]], df[[input$cda_stock2]])
    })
    
    output$regression_summary <- renderPrint({
      df <- rv$stock_data %>%
        filter(symbol %in% c(input$cda_stock1, input$cda_stock2)) %>%
        select(date, symbol, daily_return, volume) %>%
        pivot_wider(names_from = symbol, values_from = c(daily_return, volume)) %>%
        rename(target_return = paste0("daily_return_", input$cda_stock1),
               target_volume = paste0("volume_", input$cda_stock1),
               market_return = paste0("daily_return_", input$cda_stock2)) %>%
        filter(!is.na(target_return) & !is.na(target_volume) & !is.na(market_return))
      summary(lm(target_return ~ target_volume + market_return, data = df))
    })
    
    output$anova1 <- renderPrint({
      summary(aov(daily_return ~ symbol, data = rv$stock_data))
    })
    
    output$anova2 <- renderPrint({
      df <- rv$stock_data %>% mutate(month = month(date, label=TRUE))
      summary(aov(daily_return ~ symbol + month, data = df))
    })
    
    output$event_study_plot <- renderPlotly({
      df <- tq_get(c(input$cda_stock1, input$cda_stock2), from="2020-01-01", to="2022-12-31") %>%
        group_by(symbol) %>%
        arrange(date) %>%
        mutate(daily_return = adjusted / lag(adjusted) - 1) %>%
        ungroup() %>%
        filter(!is.na(daily_return)) %>%
        select(date, symbol, daily_return) %>%
        pivot_wider(names_from=symbol, values_from=daily_return)
      model <- lm(df[[input$cda_stock1]] ~ df[[input$cda_stock2]])
      df$expected_return <- predict(model, newdata=df)
      df$abnormal_return <- df[[input$cda_stock1]] - df$expected_return
      df$CAR <- cumsum(df$abnormal_return)
      plot_ly(df, x=~date, y=~CAR, type='scatter', mode='lines') %>%
        layout(title=paste("Event Study - CAR for", input$cda_stock1),
               yaxis=list(title="Cumulative Abnormal Return"))
    })
  })
}

