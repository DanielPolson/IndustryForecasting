#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tsibble)
library(fable)
library(feasts)
library(urca)
library(shiny)
library(lubridate)
library(patchwork)
library(dplyr)
library(MASS)
library(openxlsx)
library(ggplot2)

##Only important variables for analysis  
ts_data <- readRDS("ts_data.rds")
ts_data_a <- ts_data[,c(6,9,10)]
model_fit <- readRDS("model_fit.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Seasonal-ARIMA Forecasting Proportion of Canadians Employed by Industry"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("industry", "Choose an Industry:",
                    choices = unique(ts_data$lmo_detailed_industry)),
        
        sliderInput("horizon", "Forecast Horizon (Months):",
                    min = 1, max = 24, value = 12)
      ),
      
      mainPanel(
        textOutput("last_forecast_value"),
        br(),  # line break for spacing
        plotOutput("forecastPlot", height = "600px")
      )
    )
  )
  
  server <- function(input, output) {
    
    rv <- reactiveValues(latest_forecast = NULL)
    
    
    output$forecastPlot <- renderPlot({
      req(input$industry)
      # Get the selected industry
      industry_input <- input$industry
      
      # Filter ts_data and model for the selected industry
      ts_subset <- ts_data %>%
        filter(lmo_detailed_industry == industry_input)
      
      model_subset <- model_fit %>%
        filter(lmo_detailed_industry == industry_input)
      
      # Forecast with selected horizon
      forecast_subset <- model_subset %>%
        forecast(h = input$horizon)
      
      rv$latest_forecast <- forecast_subset
      
      # Plot it
      autoplot(forecast_subset, ts_subset, level = NULL) +
        labs(title = industry_input,
             x = "Date", y = "Proportion") +
        theme_minimal()
    })
    
    output$last_forecast_value <- renderText({
      req(rv$latest_forecast)
      
      # Extract final month of forecast
      latest <- rv$latest_forecast %>%
        as_tibble() %>%
        arrange(desc(Date)) %>%
        slice(1)
      
      paste0("Forecast for ", format(latest$Date, "%B %Y"), ": ", latest$.mean, 0)
    })
  }
  
  shinyApp(ui = ui, server = server)

