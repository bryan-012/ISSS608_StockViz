library(shiny)
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
library(broom)
library(ggstatsplot)


cda_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(3,
             h4("User Portfolio Input"),
             textInput(ns("stock_symbol"), "Stock Symbol (e.g., AAPL)", value = ""),
             numericInput(ns("quantity"), "Number of Shares", value = 10, min = 1),
             dateInput(ns("buy_date"), "Buy Date", value = Sys.Date() - 30),
             textInput(ns("buy_price"), "Buy Price (optional)", value = ""),
             actionButton(ns("add_stock"), "+ Add to Portfolio", class = "btn btn-primary"),
             actionButton(ns("remove_selected"), "❌ Remove Selected Entries", class = "btn btn-danger"),
             br(),
             DTOutput(ns("portfolio_table")),
             textOutput(ns("remark_warning")),
             br(),
             downloadButton(ns("download_summary"), "📄 Download Portfolio Summary")
      ),
      column(9,
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
                        plotOutput(ns("wilcox_plot")),
                        verbatimTextOutput(ns("f_test")),
                        plotOutput(ns("correlation_chart")),
                        plotOutput(ns("rolling_correlation_chart")),
                        verbatimTextOutput(ns("regression_summary")),
                        plotOutput(ns("regression_diagnostic_plot")),
                        plotOutput(ns("regression_coef_plot")),
                        plotOutput(ns("return_density_plot")),
                        verbatimTextOutput(ns("anova1")),
                        verbatimTextOutput(ns("anova2")),
                        plotlyOutput(ns("event_study_plot"))
               )
             )
      )
    )
  )
}

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
      
      # ✅ FIX: Wrap inside isolate to avoid error when accessing reactiveValues directly
      isolate({
        updateSelectInput(session, "cda_stock1", choices = unique(rv$portfolio_input$symbol))
        updateSelectInput(session, "cda_stock2", choices = unique(rv$portfolio_input$symbol))
      })
    })
    
    output$portfolio_table <- renderDT({
      df <- rv$portfolio_input
      
      if (nrow(df) > 0) {
        df$buy_date <- format(as.Date(df$buy_date), "%Y-%m-%d")
        
        df$buy_price <- mapply(function(sym, date, price) {
          if (is.na(price)) {
            if (!is.null(rv$stock_data)) {
              valid_buy_date <- max(rv$stock_data$date[rv$stock_data$symbol == sym & rv$stock_data$date <= as.Date(date)])
              market_price <- rv$stock_data %>%
                filter(symbol == sym & date == valid_buy_date) %>%
                summarise(median_price = median(adjusted, na.rm = TRUE)) %>%
                pull(median_price)
              return(paste0("$", round(market_price, 2), " <small>(Used Market Price)</small>"))
            } else {
              return("NA <small>(Market Price)</small>")
            }
          } else {
            if (!is.null(rv$stock_data)) {
              valid_buy_date <- max(rv$stock_data$date[rv$stock_data$symbol == sym & rv$stock_data$date <= as.Date(date)])
              row_data <- rv$stock_data %>% filter(symbol == sym & date == valid_buy_date)
              if (nrow(row_data) == 0) return(paste0("$", round(price, 2)))  # fallback
              day_low <- min(row_data$low, na.rm = TRUE)
              day_high <- max(row_data$high, na.rm = TRUE)
              
              if (price < day_low || price > day_high) {
                return(paste0("$", round(price, 2), " <span style='color:red;font-weight:bold'>❌</span>"))
              }
            }
            return(paste0("$", round(price, 2)))
          }
        }, df$symbol, df$buy_date, df$buy_price)
      }
      
      datatable(df, selection = 'multiple', options = list(pageLength = 5), escape = FALSE)
      
    })
    output$remark_warning <- renderText({
      df <- rv$portfolio_input
      if (nrow(df) == 0 || is.null(rv$stock_data)) return(NULL)
      
      has_out_of_range <- mapply(function(sym, date, price) {
        if (is.na(price)) return(FALSE)
        valid_buy_date <- max(rv$stock_data$date[rv$stock_data$symbol == sym & rv$stock_data$date <= as.Date(date)])
        if (is.na(valid_buy_date)) return(FALSE)
        row_data <- rv$stock_data %>% filter(symbol == sym & date == valid_buy_date)
        if (nrow(row_data) == 0) return(FALSE)
        price < row_data$low | price > row_data$high
      }, df$symbol, df$buy_date, df$buy_price)
      
      if (any(has_out_of_range)) {
        return("❗ Please remove rows marked with ❌ to proceed with the analysis. Buy price exceeds trading range.❗")
      } else {
        return(NULL)
      }
    })
    
    
    observeEvent(input$remove_selected, {
      selected_rows <- input$portfolio_table_rows_selected
      if (!is.null(selected_rows)) {
        rv$portfolio_input <- rv$portfolio_input[-selected_rows, ]
      }
    })
    
    observeEvent(rv$portfolio_input, {
      if (nrow(rv$portfolio_input) == 0) return(NULL)
      tryCatch({
        stock_symbols <- unique(rv$portfolio_input$symbol)
        data_start <- min(rv$portfolio_input$buy_date, na.rm = TRUE)
        stock_data_raw <- tq_get(stock_symbols, from = data_start, to = Sys.Date())
        
        rv$stock_data <- stock_data_raw %>%
          group_by(symbol, date) %>%
          summarise(open=first(open), high=max(high, na.rm=TRUE), low=min(low, na.rm=TRUE),
                    close=last(close), volume=sum(volume, na.rm=TRUE), adjusted=last(adjusted), .groups="drop") %>%
          group_by(symbol) %>%
          arrange(date) %>%
          mutate(daily_return = adjusted / lag(adjusted) - 1) %>%
          ungroup()
        
        portfolio_results <- list()
        summary_results <- list()
        
        for (i in 1:nrow(rv$portfolio_input)) {
          stock <- rv$portfolio_input$symbol[i]
          buy_date <- rv$portfolio_input$buy_date[i]
          qty <- rv$portfolio_input$quantity[i]
          user_price <- rv$portfolio_input$buy_price[i]
          stock_prices <- rv$stock_data %>% filter(symbol == stock)
          valid_buy_date <- max(stock_prices$date[stock_prices$date <= buy_date])
          if (is.na(valid_buy_date)) next
          buy_price <- ifelse(!is.na(user_price), user_price, median(stock_prices$adjusted[stock_prices$date == valid_buy_date], na.rm=TRUE))
          price_src <- ifelse(!is.na(user_price), "User Input", paste0("Median Price on ", valid_buy_date))
          
          track <- stock_prices %>% filter(date >= valid_buy_date) %>%
            mutate(return_since_buy = (adjusted / buy_price) - 1,
                   stock=stock, buy_date=valid_buy_date, quantity=qty, buy_price=buy_price,
                   holding_value=adjusted * qty, invested_amount=qty * buy_price,
                   unrealized_return=(adjusted - buy_price) * qty,
                   position_id = paste0(stock, "_", valid_buy_date))
          
          summary <- track %>% filter(date == max(date)) %>%
            summarise(
              stock = first(stock),
              buy_date = first(buy_date),
              quantity = first(quantity),
              buy_price = first(buy_price),
              buy_price_src = price_src,
              day_low = min(stock_prices$low[stock_prices$date == valid_buy_date], na.rm = TRUE),
              day_high = max(stock_prices$high[stock_prices$date == valid_buy_date], na.rm = TRUE),
              price_out_of_range = ifelse(!is.na(user_price), user_price < day_low | user_price > day_high, FALSE),
              if (!is.na(price_out_of_range) && price_out_of_range) {
                showNotification(
                  paste0("⚠️ Skipped ", stock, ": Buy price $", user_price,
                         " is out of range [", round(day_low, 2), " – ", round(day_high, 2), "] on ", valid_buy_date),
                  type = "warning"
                )
                next  # 🚫 Skip this stock completely
              },
              invested_amount = first(invested_amount),
              current_price = first(adjusted),
              current_value = first(holding_value),
              unrealized_return = first(unrealized_return),
              return_pct = first(return_since_buy)
            )
          
          portfolio_results[[i]] <- track
          summary_results[[i]] <- summary
        }
        
        rv$portfolio_tracking_all <- bind_rows(portfolio_results)
        rv$portfolio_summary <- bind_rows(summary_results)
        
      }, error=function(e) {
        showNotification("❌ Error retrieving data. Check symbol or network.", type="error")
      })
    })
    
    output$portfolio_summary_table <- render_gt({
      req(rv$portfolio_summary)
      if (nrow(rv$portfolio_summary) == 0) {
        return(gt(data.frame(Note = "No summary data available yet.")))
      }
      
      df <- rv$portfolio_summary %>%
        mutate(
          `Buy Price Status` = case_when(
            is.na(price_out_of_range) ~ "N/A",
            price_out_of_range ~ "❌ Out of Range",
            TRUE ~ "✔ OK"
          ),
          `Return (%)` = percent(return_pct, accuracy = 0.1),
          `Unrealized Return ($)` = dollar(unrealized_return)
        ) %>%
        select(
          Stock = stock,
          `Buy Date` = buy_date,
          Quantity = quantity,
          `Buy Price ($)` = buy_price,
          `Price Source` = buy_price_src,
          `Day Low ($)` = day_low,
          `Day High ($)` = day_high,
          `Buy Price Status`,
          `Invested Amount ($)` = invested_amount,
          `Current Price ($)` = current_price,
          `Current Value ($)` = current_value,
          `Unrealized Return ($)`,
          `Return (%)`
        )
      
      gt(df) %>%
        tab_header(title = "📋 Portfolio Summary with $ and % Return") %>%
        fmt_currency(columns = c(
          `Buy Price ($)`, `Day Low ($)`, `Day High ($)`,
          `Invested Amount ($)`, `Current Price ($)`, `Current Value ($)`
        )) %>%
        data_color(
          columns = vars(`Buy Price Status`),
          colors = scales::col_factor(
            palette = c("lightgreen", "tomato", "grey"),
            domain = c("✔ OK", "❌ Out of Range", "N/A")
          )
        )
    })
    
    
    # ==== EDA Charts ====
    output$line_chart <- renderPlotly({
      if (is.null(rv$portfolio_tracking_all)) return(NULL)
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      p <- ggplot(rv$portfolio_tracking_all, aes(x = date, y = holding_value, color = position_id)) +
        geom_line(size=1) +
        labs(title="Stock Value Over Time", x="Date", y="Holding Value ($)") +
        scale_y_continuous(labels=dollar)
      ggplotly(p)
    })
    
    output$total_portfolio_plot <- renderPlotly({
      if (is.null(rv$portfolio_tracking_all)) return(NULL)
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      df <- rv$portfolio_tracking_all %>% group_by(date) %>% summarise(total_value = sum(holding_value), .groups = "drop")
      p <- ggplot(df, aes(x=date, y=total_value)) +
        geom_line(color="forestgreen") +
        labs(title="Total Portfolio Value", x="Date", y="Total Value") +
        scale_y_continuous(labels=dollar)
      ggplotly(p)
    })
    
    output$rolling_vol_plot <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
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
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      df <- rv$stock_data %>% select(date, symbol, daily_return) %>%
        filter(!is.na(daily_return)) %>%
        pivot_wider(names_from = symbol, values_from = daily_return)
      corrplot(cor(df %>% select(-date), use="pairwise.complete.obs"), method="color", addCoef.col="black")
    })
    
    output$return_volatility_plot <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      df <- rv$stock_data %>%
        group_by(symbol) %>%
        summarise(mean_return=mean(daily_return, na.rm=TRUE),
                  volatility=sd(daily_return, na.rm=TRUE))
      ggplot(df, aes(x=volatility, y=mean_return, label=symbol)) +
        geom_point(size=4, color="darkblue") +
        geom_text(nudge_y=0.0005, fontface="bold") +
        labs(title="Return vs Volatility (Risk–Return Tradeoff)", x="Volatility (SD)", y="Mean Return") +
        scale_x_continuous(labels=percent) +
        scale_y_continuous(labels=percent)
    })
    
    output$monthly_volume_plot <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      df <- rv$stock_data %>%
        filter(date >= Sys.Date() %m-% years(5)) %>%
        mutate(year=year(date), month=month(date, label=TRUE)) %>%
        group_by(symbol, year, month) %>%
        summarise(total_volume = sum(volume, na.rm=TRUE), .groups = "drop")
      ggplot(df, aes(x=month, y=total_volume, fill=factor(year))) +
        geom_col(position="dodge") +
        facet_wrap(~symbol, scales="free_y") +
        labs(title="📊 Monthly Trading Volume by Stock", x="Month", y="Volume", fill="Year") +
        scale_y_continuous(labels=comma)
    })
    
    # ==== CDA Tests ====
    # Visualize Wilcoxon Test across Stocks — e.g., AAPL vs GOOG daily returns
    output$wilcox_plot <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      req(input$cda_stock1, input$cda_stock2)
      
      df <- rv$stock_data %>% 
        filter(symbol %in% c(input$cda_stock1, input$cda_stock2)) %>%
        filter(!is.na(daily_return))
      
      if (nrow(df) == 0) return(NULL)
      
      ggbetweenstats(
        data = df,
        x = symbol,
        y = daily_return,
        type = "np",  # Non-parametric (Wilcoxon/Mann–Whitney)
        pairwise.display = "s",  # show significance
        pairwise.comparisons = TRUE,
        title = paste("Wilcoxon Test: Daily Return Comparison"),
        results.subtitle = TRUE
      )
    })
    
    
    #1 Daily Return Correlation
    plot_correlation_chart <- function(stock1, stock2, data = stock_data) {
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      cor_data <- data %>%
        filter(symbol %in% c(stock1, stock2)) %>%
        select(symbol, date, daily_return) %>%
        filter(!is.na(daily_return)) %>%
        pivot_wider(names_from = symbol, values_from = daily_return)
      
      if (!(stock1 %in% names(cor_data)) | !(stock2 %in% names(cor_data))) {
        return(ggplot() + ggtitle("❌ Missing daily return data for one or both stocks"))
      }
      
      cor_data <- cor_data %>%
        filter(!is.na(.data[[stock1]]), !is.na(.data[[stock2]]))
      
      if (nrow(cor_data) < 10) {
        return(ggplot() + ggtitle("⚠ Not enough data to compute correlation"))
      }
      
      corr_val <- round(cor(cor_data[[stock1]], cor_data[[stock2]], use = "complete.obs"), 3)
      
      ggplot(cor_data, aes(x = .data[[stock1]], y = .data[[stock2]])) +
        geom_point(alpha = 0.5, color = "steelblue") +
        geom_smooth(method = "lm", color = "darkred", se = FALSE) +
        labs(
          title = paste("📈 Daily Return Correlation:", stock1, "vs", stock2),
          subtitle = paste("Pearson Correlation =", corr_val),
          x = paste(stock1, "Daily Return"),
          y = paste(stock2, "Daily Return")
        ) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
    }
    output$correlation_chart <- renderPlot({
      plot_correlation_chart(input$cda_stock1, input$cda_stock2, rv$stock_data)
    })
    
    #2 Rolling Correlation
    plot_rolling_correlation <- function(stock1, stock2, data = stock_data, window = 30) {
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      cor_data <- data %>%
        filter(symbol %in% c(stock1, stock2)) %>%
        select(symbol, date, daily_return) %>%
        filter(!is.na(daily_return)) %>%
        pivot_wider(names_from = symbol, values_from = daily_return)
      
      if (!(stock1 %in% names(cor_data)) | !(stock2 %in% names(cor_data))) return(NULL)
      
      cor_data <- cor_data %>%
        arrange(date) %>%
        mutate(
          rolling_corr = zoo::rollapply(
            data = cbind(.data[[stock1]], .data[[stock2]]),
            width = window,
            FUN = function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"),
            by.column = FALSE,
            fill = NA,
            align = "right"
          )
        )
      
      ggplot(cor_data, aes(x = date, y = rolling_corr)) +
        geom_line(color = "dodgerblue") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          title = paste("🔁 Rolling Correlation (", window, " Days)", sep = ""),
          subtitle = paste(stock1, "vs", stock2),
          x = "Date",
          y = "Correlation"
        ) +
        ylim(-1, 1) +
        theme_minimal()
    }
    output$rolling_correlation_chart <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      plot_rolling_correlation(input$cda_stock1, input$cda_stock2, rv$stock_data, window = 30)
    })
    
    output$event_study_plot <- renderPlotly({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      df <- tq_get(c(input$cda_stock1, input$cda_stock2), from="2020-01-01", to="2022-12-31") %>%
        group_by(symbol) %>%
        arrange(date) %>%
        mutate(daily_return = adjusted / lag(adjusted) - 1) %>%
        ungroup() %>%
        select(date, symbol, daily_return) %>%
        filter(!is.na(daily_return)) %>%
        pivot_wider(names_from = symbol, values_from = daily_return)
      
      model <- lm(df[[input$cda_stock1]] ~ df[[input$cda_stock2]])
      df$expected_return <- predict(model, newdata = df)
      df$abnormal_return <- df[[input$cda_stock1]] - df$expected_return
      df$CAR <- cumsum(df$abnormal_return)
      
      plot_ly(df, x = ~date, y = ~CAR, type = "scatter", mode = "lines") %>%
        layout(title = paste("Event Study - CAR for", input$cda_stock1),
               yaxis = list(title = "Cumulative Abnormal Return"))
    })
    
    plot_regression_diagnostics <- function(target_stock, market_index_stock, data = stock_data) {
      reg_data <- data %>%
        filter(symbol %in% c(target_stock, market_index_stock)) %>%
        select(date, symbol, daily_return, volume) %>%
        pivot_wider(names_from = symbol, values_from = c(daily_return, volume)) %>%
        rename(
          target_return = paste0("daily_return_", target_stock),
          target_volume = paste0("volume_", target_stock),
          market_return = paste0("daily_return_", market_index_stock)
        ) %>%
        filter(!is.na(target_return), !is.na(target_volume), !is.na(market_return))
      
      if (nrow(reg_data) < 10) return(NULL)
      
      model <- lm(target_return ~ target_volume + market_return, data = reg_data)
      reg_data$fitted <- fitted(model)
      reg_data$residuals <- resid(model)
      
      ggplot(reg_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          title = paste("📉 Residuals vs Fitted -", target_stock),
          x = "Fitted Values",
          y = "Residuals"
        ) +
        theme_minimal()
    }
    
    output$regression_diagnostic_plot <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      plot_regression_diagnostics(input$cda_stock1, input$cda_stock2, data = rv$stock_data)
    })
    
    output$return_density_plot <- renderPlot({
      req(rv$stock_data)
      ggplot(rv$stock_data, aes(x = daily_return, fill = symbol)) +
        geom_density(alpha = 0.4) +
        scale_x_continuous(labels = percent) +
        labs(
          title = "📈 Density Plot of Daily Returns by Stock",
          x = "Daily Return",
          y = "Density",
          fill = "Stock Symbol"
        ) +
        theme_minimal()
    })
    
    
    
    
    plot_regression_coefficients <- function(target_stock, market_index_stock, data = stock_data) {
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      reg_data <- data %>%
        filter(symbol %in% c(target_stock, market_index_stock)) %>%
        select(date, symbol, daily_return, volume) %>%
        pivot_wider(names_from = symbol, values_from = c(daily_return, volume)) %>%
        rename(
          target_return = paste0("daily_return_", target_stock),
          target_volume = paste0("volume_", target_stock),
          market_return = paste0("daily_return_", market_index_stock)
        ) %>%
        filter(!is.na(target_return), !is.na(target_volume), !is.na(market_return))
      
      if (nrow(reg_data) < 10) return(NULL)
      
      model <- lm(target_return ~ target_volume + market_return, data = reg_data)
      coef_df <- broom::tidy(model)
      
      ggplot(coef_df, aes(x = term, y = estimate)) +
        geom_point(size = 4, color = "darkorange") +
        geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          title = paste("📌 Regression Coefficients -", target_stock),
          x = "Predictor",
          y = "Estimate (Beta)"
        ) +
        theme_minimal()
    }
    
    output$regression_coef_plot <- renderPlot({
      validate(
        need(!is.null(rv$portfolio_tracking_all) && nrow(rv$portfolio_tracking_all) > 0, "No data yet. Please add stocks to view the chart.")
      )
      plot_regression_coefficients(input$cda_stock1, input$cda_stock2, data = rv$stock_data)
    })
  })
}
