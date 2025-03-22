# forecast.R

library(shiny)
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(quantmod)
library(lubridate)
library(plotly)
library(DT)
library(rsample)
library(tidyquant)

forecast_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Stock Forecasting App"),
    sidebarLayout(
      sidebarPanel(
        textInput(ns("stock"), "Stock Symbol:", value = "AAPL"),
        dateInput(ns("start_date"), "Start Date:", value = Sys.Date() - 730),
        numericInput(ns("forecast_period"), "Forecast Period (months):", value = 2, min = 1, max = 12),
        sliderInput(ns("train_prop"), "Train Proportion:", min = 0.5, max = 0.95, value = 0.8, step = 0.05),
        checkboxGroupInput(ns("model_selection"), "Select Forecasting Models:",
                           choices = list("ARIMA" = "ARIMA", "Prophet" = "Prophet", "XGBoost" = "XGBoost",
                                          "ETS" = "ETS", "Random Forest" = "Random Forest"),
                           selected = c("ARIMA", "Prophet", "ETS")),
        hr(),
        conditionalPanel(
          condition = sprintf("input['%s'].indexOf('Prophet') > -1", ns("model_selection")),
          numericInput(ns("fourier_k"), "Fourier Terms (Prophet):", value = 1, min = 1, max = 12)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'].indexOf('XGBoost') > -1", ns("model_selection")),
          h4("XGBoost Parameters"),
          numericInput(ns("xgboost_trees"), "Number of Trees:", value = 300, min = 50),
          numericInput(ns("xgboost_depth"), "Max Depth:", value = 10, min = 1)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'].indexOf('Random Forest') > -1", ns("model_selection")),
          h4("Random Forest Parameters"),
          numericInput(ns("rf_trees"), "Number of Trees:", value = 500, min = 100),
          numericInput(ns("rf_min_n"), "Minimum Node Size:", value = 50, min = 1)
        ),
        actionButton(ns("run_forecast"), "Run Forecast")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Model Evaluation",
                   fluidRow(column(12, DTOutput(ns("eval_table")))),
                   fluidRow(
                     column(4, plotOutput(ns("accuracy_barplot_mape"))),
                     column(4, plotOutput(ns("accuracy_barplot_mae"))),
                     column(4, plotOutput(ns("accuracy_barplot_rmse")))
                   )
          ),
          tabPanel("Forecast Plot", plotlyOutput(ns("forecast_plot")))
        )
      )
    )
  )
}

forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    stock_data_reactive <- eventReactive(input$run_forecast, {
      req(input$stock, input$start_date)
      end_date <- Sys.Date()
      
      stock_data <- tryCatch(
        tq_get(input$stock, from = input$start_date, to = end_date),
        error = function(e) {
          showNotification("Error retrieving stock data.", type = "error")
          return(NULL)
        }
      )
      
      validate(
        need(!is.null(stock_data) && nrow(stock_data) > 0, "No data returned.")
      )
      
      stock_data %>%
        select(date, close) %>%
        rename(Date = date, Value = close) %>%
        mutate(Date = as.Date(Date)) %>%
        arrange(Date)
    })
    
    forecast_results <- eventReactive(input$run_forecast, {
      stock_tbl <- stock_data_reactive()
      splits <- initial_time_split(stock_tbl, prop = input$train_prop)
      training_data <- training(splits)
      testing_data  <- testing(splits)
      models_list <- list()
      
      if ("ARIMA" %in% input$model_selection) {
        model_arima <- arima_reg() %>%
          set_engine("auto_arima") %>%
          fit(Value ~ Date, data = training_data)
        models_list[[length(models_list) + 1]] <- model_arima
      }
      
      if ("Prophet" %in% input$model_selection) {
        recipe_prophet <- recipe(Value ~ Date, data = training_data) %>%
          step_timeseries_signature(Date) %>%
          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                  contains("second"), contains("xts"), contains("half"), contains(".iso")) %>%
          step_normalize(Date_index.num) %>%
          step_fourier(Date, period = 12, K = input$fourier_k) %>%
          step_dummy(all_nominal())
        
        workflow_fit_prophet <- workflow() %>%
          add_model(prophet_reg() %>% set_engine("prophet")) %>%
          add_recipe(recipe_prophet) %>%
          fit(training_data)
        
        models_list[[length(models_list) + 1]] <- workflow_fit_prophet
      }
      
      if ("XGBoost" %in% input$model_selection) {
        recipe_xgb <- recipe(Value ~ Date, data = training_data) %>%
          step_date(Date, features = c("year", "month", "doy", "week")) %>%
          step_rm(Date) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_normalize(all_numeric_predictors())
        
        workflow_fit_xgb <- workflow() %>%
          add_model(boost_tree(mode = "regression", trees = input$xgboost_trees, tree_depth = input$xgboost_depth) %>%
                      set_engine("xgboost")) %>%
          add_recipe(recipe_xgb) %>%
          fit(training_data)
        
        models_list[[length(models_list) + 1]] <- workflow_fit_xgb
      }
      
      if ("ETS" %in% input$model_selection) {
        model_ets <- exp_smoothing() %>%
          set_engine("ets") %>%
          fit(Value ~ Date, data = training_data)
        models_list[[length(models_list) + 1]] <- model_ets
      }
      
      if ("Random Forest" %in% input$model_selection) {
        recipe_rf <- recipe(Value ~ Date, data = training_data) %>%
          step_date(Date, features = c("year", "month", "dow")) %>%
          step_rm(Date) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_normalize(all_numeric_predictors())
        
        workflow_fit_rf <- workflow() %>%
          add_model(rand_forest(mode = "regression", trees = input$rf_trees, min_n = input$rf_min_n) %>%
                      set_engine("ranger")) %>%
          add_recipe(recipe_rf) %>%
          fit(training_data)
        
        models_list[[length(models_list) + 1]] <- workflow_fit_rf
      }
      
      calibrate_tbl <- do.call(modeltime_table, models_list) %>%
        modeltime_calibrate(new_data = testing_data)
      
      acc_tbl <- calibrate_tbl %>%
        modeltime_accuracy() %>%
        arrange(mape)
      
      forecast_tbl <- calibrate_tbl %>%
        modeltime_forecast(h = paste(input$forecast_period, "months"), actual_data = stock_tbl)
      
      list(acc_tbl = acc_tbl, forecast_tbl = forecast_tbl)
    })
    
    output$eval_table <- renderDT({
      req(forecast_results())
      datatable(forecast_results()$acc_tbl, options = list(pageLength = 5), rownames = FALSE)
    })
    
    output$accuracy_barplot_mape <- renderPlot({
      req(forecast_results())
      forecast_results()$acc_tbl %>%
        mutate(model_label = paste0(.model_id, ": ", .model_desc)) %>%
        ggplot(aes(x = reorder(model_label, mape), y = mape, fill = model_label)) +
        geom_col(show.legend = FALSE) + coord_flip() +
        labs(title = "MAPE by Model", x = "Model", y = "MAPE") +
        theme_minimal()
    })
    
    output$accuracy_barplot_mae <- renderPlot({
      req(forecast_results())
      forecast_results()$acc_tbl %>%
        mutate(model_label = paste0(.model_id, ": ", .model_desc)) %>%
        ggplot(aes(x = reorder(model_label, mae), y = mae, fill = model_label)) +
        geom_col(show.legend = FALSE) + coord_flip() +
        labs(title = "MAE by Model", x = "Model", y = "MAE") +
        theme_minimal()
    })
    
    output$accuracy_barplot_rmse <- renderPlot({
      req(forecast_results())
      forecast_results()$acc_tbl %>%
        mutate(model_label = paste0(.model_id, ": ", .model_desc)) %>%
        ggplot(aes(x = reorder(model_label, rmse), y = rmse, fill = model_label)) +
        geom_col(show.legend = FALSE) + coord_flip() +
        labs(title = "RMSE by Model", x = "Model", y = "RMSE") +
        theme_minimal()
    })
    
    output$forecast_plot <- renderPlotly({
      req(forecast_results())
      forecast_results()$forecast_tbl %>%
        plot_modeltime_forecast(.interactive = FALSE) %>%
        ggplotly()
    })
  })
}
