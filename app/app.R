### Required libraries ###
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(data.table)
library(shinymanager)
library(rhandsontable)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tseries)
library(tibble)
library(hablar)
library(plotly)
library(ggthemes)
library(writexl)
library(scales)
library(esquisse)
library(shinyFiles)
library(RCurl)
library(DBI)
library(odbc)
library(dbplyr)
library(forecast)
library(stats)



### Define UI ###
ui <- dashboardPage(
        dashboardHeader(title = "Time Series Forecast"),
        dashboardSidebar(
          
          # dynamically generated user panel
          uiOutput("userpanel"),
          
          ## Sidebar Menu
          sidebarMenu(
            menuItem("Data Import", tabName = "data_import", icon = icon("dashboard"), selected = T),
            menuItem("Visualization", tabName = "visualization", icon = icon("bar-chart-o")),
            menuItem("Modelling & Forecasting", tabName = "modelling", icon = icon("cog", library = "glyphycon"))
          )
        ),
        
        dashboardBody(
          tabItems(
          tabItem(
            tabName = "data_import",
            fluidPage(
              tabBox(side = "left", width = 12, selected = "Data Import",
                     tabPanel("Data Import",
                              fluidRow(
                                box(
                                  title = "Upload the time series data", width = 12, status = "primary", solidHeader = T,
                                  br(), fileInput("data1", "Upload your file here (.csv) :", accept = c(".csv")),
                                  br(), 
                                  p(style = "color:red", "Press the below button after uploading"),
                                  actionButton("nfrt_upload", "Uploaded"),
                                  shinyFeedback::useShinyFeedback()
                                )),
                              fluidRow(
                                box(title = "Uploaded data", width = 12, status = "success", solidHeader = T, 
                                collapsible = T, DT::dataTableOutput("data_uploaded")
                              )))))),
          
          tabItem(
            tabName = "visualization",
            uiOutput("time_id"),
            uiOutput("time_series"),
            fluidRow(box(width = 12, title = "Selected Data", status = "success", solidHeader = T, collapsible = T,
                         DT::dataTableOutput("data_uploaded_check"))),
            fluidRow(box(width = 12, title = "Time Series Plot", status = "success", solidHeader = T, 
                         plotlyOutput("time_series_plot")))),
          
          tabItem(
            tabName = "modelling",
            fluidPage(
              tabBox(side = "left", width = 12, selected = "Time Series Model",
                     tabPanel("Time Series Model",
                              fluidRow(
                                box(
                                  title = "Select the time series model:", width = 12, status = "primary", solidHeader = T,
                                  selectInput("models", "Models:", c("ARIMA", "MA", "Exponenctial Smoothing"), 
                                              selected = FALSE, multiple = TRUE),
                                  br(), 
                                  selectInput("errors", "Error Type:", c("RMSE"), selected = FALSE),
                                  br(), 
                                  numericInput("f_period", "Forecast period (must be an positive integer):", 7)),
                              fluidRow(
                                box(title = "Fitted model:", width = 12, status = "success", solidHeader = T,
                                    collapsible = T, verbatimTextOutput("sum", placeholder = TRUE))),
                              fluidRow(box(width = 12, title = "Time Series Forecast Plot", status = "success",
                                           solidHeader = T, collapsible = T,
                                           plotlyOutput("time_series_forecast_plot"))),
                              fluidRow(box(width = 12, title = "Forecasted Data", status = "success", solidHeader = T, collapsible = T,
                                           DT::dataTableOutput("forecast_data")))
                     )))
            )
          
        )
        )))

### Define server ###
server <- function(input, output) {
    
  ### data_import ###
    # Import data
    time_data_upload <- reactive({
      if(!is.null(input$data1$datapath)){
      data_upload <- read.csv(input$data1$datapath, header = TRUE)
      return(data_upload)}
    })
    
    observeEvent(input$nfrt_upload, time_data_upload())
    
    
    # Render imported data
    output$data_uploaded <- DT::renderDataTable(time_data_upload(), selection = "none", extensions = "AutoFill",
                                                rownames = FALSE, options = list(autoFill = TRUE, scrollX = TRUE))
    
  ### visualization ###
    # variable selection
    output$time_id = renderUI({
      temp <- time_data_upload()
      selectInput("x", "Time ID:", choices =  as.character(names(temp)), selected =  FALSE, multiple = TRUE)
    })
    
    output$time_series = renderUI({
      temp <- time_data_upload()
      selectInput("y", "Time Series variable:", choices = as.character(names(temp)), 
                  selected = FALSE, multiple = TRUE)
    })

    # Time series visualization
    output$time_series_plot <- renderPlotly({
        temp <- time_data_upload()
        temp <- temp[,c(input$x, input$y)]
        temp <- reshape2::melt(temp, id.vars = 1, variable.name = 'series')
        temp1 <- colnames(temp)[1]
        colnames(temp)[1] <- "time"
        
        output$data_uploaded_check <- DT::renderDataTable(temp, selection = "none", extensions = "AutoFill",
                                                    rownames = FALSE, options = list(autoFill = TRUE, scrollX = TRUE))
        
        ggplotly(ggplot(temp, aes(x = time, y = value, col = series)) + geom_line() + 
                   geom_point() + facet_grid(series~.) + xlab(temp1) + ylab("Series"))
    })
    
  ### modelling ###
    
    # Forecast plot
    output$time_series_forecast_plot <- renderPlotly({
    
    if(!is.null(input$models)){
        
    # data and parameters formatting
    df <- time_data_upload()
    df_filter <- df[,c(input$x, input$y)]
    df_cols <- colnames(df_filter)[-1]
    df_1 <- melt(df_filter, id.vars = 1, variable.name = 'series')
    df_forecast <- data.frame()
    h_forecast <- input$f_period
    fit_models <- input$models
    # mod_arima <- list()
    mod_arima_sum <- list()
    # model fitting and forecasting
    for(i in df_cols){
      temp_df <- df_1 %>% filter(series == i)
      # time id extension
      wd <- temp_df[2,1] - temp_df[1,1]
      ex_ts <- seq(temp_df[length(temp_df[,1]),1]+wd, by = wd, length.out = h_forecast)
      time_extended <- c(temp_df[,input$x], ex_ts)
      if("ARIMA" %in% fit_models){
        # model fitting
        mod_arima <- auto.arima(ts(temp_df$value))
        mod_arima_sum[[i]] <- summary(mod_arima)
        # Forecasting
        forecast_mean <- c(fitted(mod_arima), forecast::forecast(mod_arima, h = h_forecast)$mean)
        temp_arima <- data.frame(time = time_extended, series = i, value = forecast_mean, Type = "ARIMA Forecast")
        df_forecast <- dplyr::bind_rows(df_forecast, temp_arima)
      }
      
      
      temp <- data.frame(time = time_extended, series = i, value = c(temp_df$value, rep(NA, times = h_forecast)), 
                         Type = "Actual")
      df_forecast <- dplyr::bind_rows(df_forecast, temp)
    }
    
    # Fitted model summary
    forecast_final <-  df_forecast
    
    output$forecast_data <- DT::renderDataTable(forecast_final, selection = "none", extensions = "AutoFill",
                                                rownames = FALSE, options = list(autoFill = TRUE, scrollX = TRUE))
    
    output$sum <- renderPrint(as.character(mod_arima_sum))
    
    ggplotly(ggplot(forecast_final, aes(x = time, y = value, col = Type)) + geom_line() + 
                 geom_point() + facet_grid(series~.) + xlab(input$x) + ylab("Series"))
    
    
    
    }})
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
