library(shiny)
library(forecast)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(foreign)
library(zoo)
library(stats)
library(tseries)
library(haven)
library(fma)
library(expsmooth)

well_df <- read.csv('Visualization/combined_well.csv')
well_df <- well_df %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         rain_in = na.approx(rain_in, rule=2))

well_df$datetime <- as.POSIXct(well_df$datetime, "%Y-%m-%d %H:%M:%S", tz='EST')
well_df <- filter(well_df, datetime > '2016-10-16 22:00:00')
View(well_df)
#G-561 need finite 'xlim' values

####----------------------------------------UI-------------------------------------------##

# Define UI 
ui<-(fluidPage(
  
  # Title
  headerPanel("Well Depth Forecasting"),
  
  # Sidebar
  sidebarPanel(
    selectInput("wellInput", "Well:",
                list("F-45", "F-179", "F-319", "G-561", "G-580A", "G-852", "G-860", "G-1220", "G-1260", "G-2147", "G-2866", "G-3549", "PB-1680")),
    sliderInput("dateInput", "Date Time", min(well_df$datetime), max(well_df$datetime), c(min(well_df$datetime)+365*24*5, max(well_df$datetime)-365*24*5), step = 24*30),
    numericInput("fcastInput", "Forecast (Hours):", 7*24)
  ),
  
  # Plots
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Arima Forecast", plotOutput("forecastPlot")),
      tabPanel("Diagnostics", plotOutput("resPlot"), plotOutput("acfPlot"), plotOutput("pacfPlot")),
      tabPanel("Well Location", plotOutput("map"))
    )
  )
))


#####-----------------------------------SERVER--------------------------------------------####

# Define server logic

server<-(function(input, output) {
  
  #Filter the data based on date and well section
  filtered_data <- reactive({
    well_df %>% filter(datetime >= input$dateInput[1] & datetime <= input$dateInput[2], Well==input$wellInput)
  })
  
  #Create time series object: training and test
  well <- reactive({
    well_ts<-ts(filtered_data()$well_ft,frequency =365.25*24)
    training=subset(well_ts,end=length(filtered_data()$datetime)-24*7)
    test=subset(well_ts,start=length(filtered_data()$datetime)-(24*7-1))
    arima<-auto.arima(training)
    #arima <- Arima(training,order=c(2,0,0))
    return(list(well_ts=well_ts, arima=arima))
  })
  
  output$caption <- renderText({
    paste("Well: ", input$wellInput)
  })
  
  output$forecastPlot <- renderPlot({
    plot(forecast(well()$arima, h=input$fcastInput))
  })

  output$resPlot <- renderPlot({
    plot(well()$arima$residuals)
  })

  output$acfPlot <- renderPlot({
    Acf(well()$arima$residuals, lag=25)$acf
  })

  output$pacfPlot <- renderPlot({
    Pacf(well()$arima$residuals, lag=25)$acf
  })
  
})


####----------------------------------------RUN APP----------------------------------------------####

# Run the app 
shinyApp(ui, server)

