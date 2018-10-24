library(shiny)
####-----------------------LIBRARIES------------------------####
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
library(shinydashboard)
library(DT)
library(ggmap)
library(shinythemes)
library(leaflet)
#devtools::install_github("dkahle/ggmap")

####-----------------------LOAD DATA FILES---------------------####

setwd('~/Documents/GitHub/hw-fall2-blue5/Visualization/')

# well data
well_df <- read.csv('combined_well_hourly_2014.csv')
well_df$datetime <- as.POSIXct(well_df$datetime, tz='EST')

# well locations
well_loc <- read.csv('Well_Coordinates.csv',sep='')
states <- map_data( "state" )
coords <- well_loc
data <- well_df %>%
  group_by(Well)%>%
  summarise(avg_well_ft=mean(well_ft), sum_rain_in=sum(rain_in), avg_tide_ft=mean(tide_ft))
points <- merge( coords, data, by.x = 'Well', by.y = 'Well' )

####-----------------------UI---------------------------####

# Side Panel Function
specialSide <- function() {
  column(4,
  inputPanel(
    selectInput("wellInput", "Well:",
                list("F-45", "F-179", "F-319", "G-561", "G-580A", "G-852", "G-860", "G-1220", "G-1260", "G-2147", "G-2866", "G-3549", "PB-1680"),
                selected = "F-45"),
    selectInput("modelInput", "Model:",
                c("Arima(1,0,0)", "Arima(2,0,0)", "Arima(3,0,0)", "Arima(4,0,0)")),
    fluidRow(column(12,
                    sliderInput("dateInput", "Date Range:", min(well_df$datetime), max(well_df$datetime), c(as.POSIXct('2017-05-27 12:00:00'), as.POSIXct('2018-04-13 12:00:00')), step = 24*30))),
    checkboxGroupInput("measureInput", "View Additional Measurements:",
                       c("Rain Amount", "Tide"),
                       inline = TRUE),
    numericInput("fcastInput", "Forecast (Days):", 21)
  ),
  
  leafletOutput("mymap", height=500, width=400)
  )
}


# Define UI 
ui<-(fluidPage(theme = shinytheme("cerulean"),
  # shinythemes::themeSelector(), 
  # Custom CSS styling
  tags$style(HTML("
                  body {
                    background-color: rgba(	211,211,211,0.7);
                  }
                  .navbar {
                    background-image: linear-gradient(#	40c4ff, #0094cc) !important;
                  }
                  .navbar > div > ul > li.active > a {
                    background-color: 40c4ff !important;
                  }
                  .navbar > div > ul > li > a:hover {
                    background-color: 40c4ff !important;
                  }
                  #forecastPlot {
                    height: 600px !important;
                  }
                  #forecastPlot > div > div {
                    padding: 25px !important;
                    border: 1px solid !important;
                    margin: 10px 0 !important;
                    box-shadow: 3px 5px #222222 !important;
                  }
                  #resPlot, #acfPlot {
                    border: 1px solid !important;
                    margin: 10px 0 !important;
                    box-shadow: 3px 5px #222222 !important;
                  }
                  #mymap {
                    width: 100% !important;
                    border: 1px solid !important;
                    margin: 15px 0 !important;
                    box-shadow: 3px 5px #222222 !important;
                  }
                  #diagnosticTable {
                    display: block !important;
                    padding: 30px 0 0 0 !important;
                    margin: 0 auto !important;
                  }
                 #diagnosticTable table {
                    margin: 0 auto !important;
                    border: 1px solid !important;
                    box-shadow: 2px 4px #222222 !important;
                    background-color: white;
                  }
                  .shiny-input-panel {
                    background-color: rgba(0, 176, 255, 0.3) !important;
                    border: 1px solid !important;
                    padding: 10px !important;
                    box-shadow: 3px 5px #222222 !important;
                  }
                  ")),
  
  # Panels on Main Page
  navbarPage(title="VizuWell: Visualizing Well Volume",
             tabPanel("Arima Forecast", 
                        fluidRow(
                          specialSide(),
                          column(8, 
                            plotlyOutput("forecastPlot"),div(tableOutput("diagnosticTable"), style = "font-size:125%"),
                            HTML('<p><img src="https://s3.amazonaws.com/saltbox.solutions/cdn/annie/logo2.png", height="10%", width="10%", align="right"/></p>')
                          )
                        )
                      ),
             tabPanel("Diagnostics", 
                      fluidRow(
                        column(2 
                        ),
                        column(8, 
                               plotOutput("resPlot"), plotOutput("acfPlot")
                        ),
                        column(2 
                        )
                      )
             )
  )

))


#####----------------------SERVER-------------------------####

# Define server logic
server<-(function(input, output) {
  
  # Filter the data based on date and well section
  filtered_data <- reactive({
    well_df %>% filter(datetime >= input$dateInput[1] & datetime <= input$dateInput[2], Well==input$wellInput)
  })
  
  # Create time series objects, forecast, diagnostic calcs, summary stats
  well <- reactive({
    well_ts<-ts(filtered_data()$well_ft,frequency =365.25*24)
    fcast=input$fcastInput*24
    
    training=subset(well_ts,end=length(filtered_data()$datetime)-fcast)
    test=subset(well_ts,start=length(filtered_data()$datetime)-(fcast-1))
    
    if(input$modelInput=='Arima(1,0,0)') {
      arima <- Arima(training, order=c(1,0,0))
    }
    else if (input$modelInput=='Arima(2,0,0)') {
      arima <- Arima(training, order=c(2,0,0))
    }
    else if (input$modelInput=='Arima(3,0,0)') {
      arima <- Arima(training, order=c(2,0,0))
    }
    else if (input$modelInput=='Arima(4,0,0)') {
      arima <- Arima(training, order=c(3,0,0))
    }
    
    forecast<-forecast(arima, h=fcast)
    forecastdf<-data.frame(as.POSIXct(tail(filtered_data()$datetime,n=fcast),tz='EST'),round(forecast$mean,4),round(forecast$lower,4),round(forecast$upper,4), test)
    names(forecastdf)[1]<-'datetime'
    names(forecastdf)[2]<-'well_ft'
    names(forecastdf)[4]<-'lo95'
    names(forecastdf)[6]<-'hi95'
    testmape=mean(abs(forecastdf$test-forecastdf$well_ft)/abs(forecastdf$test))
    diagdf<-data.frame('Model'=input$modelInput,'Mape'=round(testmape,2), 'AIC'=round(arima$aic,2), 'AR1'=round(arima$coef[1],2),'AR2'=round(arima$coef[2],2), 'AR3'=round(arima$coef[3],2), 'AR4'=round(arima$coef[4],2))
    
    mapdf<-data.frame("Wellft"=mean(filtered_data()$well_ft), 'Rain'=sum(filtered_data()$rain_in), 'Tide'=mean(filtered_data()$tide_ft))
    locdf<-well_loc %>% filter(Well==input$wellInput)
    
    return(list(well_ts=well_ts, arima=arima, forecastdf=forecastdf, diagdf=diagdf, mapdf=mapdf, locdf=locdf))
  })
  
  # Plot Time Series with Forecast
  output$forecastPlot <- renderPlotly({
    p<-ggplot(data = filtered_data(), aes(x = datetime)) +
      geom_line(data=filtered_data(), aes(y = well_ft, colour='Actual Well Depth'), size = 1) +
      geom_line(data=well()$forecastdf, aes(y = well_ft, colour='Forecasted Well Depth'), size = 1) +
      {if((length(input$measureInput)==1 && input$measureInput=='Rain Amount'))geom_line(data=filtered_data(), aes(y = rain_in, colour='Rain (in)'), size = .4)} +
      {if((length(input$measureInput)==1 && input$measureInput=='Tide'))geom_line(data=filtered_data(), aes(y = tide_ft, colour='Tide (ft)'), size = .2)} +
      {if(length(input$measureInput)==2)geom_line(data=filtered_data(), aes(y = rain_in, colour='Rain (in)'), size = .4)} +
      {if(length(input$measureInput)==2)geom_line(data=filtered_data(), aes(y = tide_ft, colour='Tide (ft)'), size = .2)} +
      {if((length(input$measureInput)==1 && input$measureInput=='Rain Amount'))  scale_colour_manual("",breaks = c("Actual Well Depth","Forecasted Well Depth","Rain Amount"),values = c('#00b0ff',"orange","purple"))} +
      {if((length(input$measureInput)==1 && input$measureInput=='Tide')) scale_colour_manual("",breaks = c("Actual Well Depth","Forecasted Well Depth", "Tide"),values = c('#00b0ff',"orange","grey"))} +
      {if(length(input$measureInput)==2) scale_colour_manual("",breaks = c("Actual Well Depth","Forecasted Well Depth","Rain Amount", "Tide"),values = c('#00b0ff',"orange","purple","grey")) } +
      {if(length(input$measureInput)==0) scale_colour_manual("",breaks = c("Actual Well Depth","Forecasted Well Depth"),values = c('#00b0ff',"orange"))} +
      geom_ribbon(data=well()$forecastdf, aes(ymin = lo95, ymax = hi95), alpha = .25) +
      scale_y_continuous(name='Well Depth (ft)') +
      xlab("Date") +
      ggtitle(paste('Well:', input$wellInput, 'Arima Forecast')) +
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid.minor = element_blank(),
            plot.title=element_text(size=20, face="bold"),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=14,face="bold"),
            legend.text=element_text(size=8),
            legend.title=element_blank()) 
    ggplotly(p) %>%
      layout(
        legend=list(
          orientation="h",
          x=.35,
          y=.08
        ),
        margin=list(
          t=50,
          r=5,
          b=5,
          l=5)
      )
  })

  # Diagnostic Table with MAPE and AR terms
  output$diagnosticTable = renderTable({
    well()$diagdf
  })
  
  # Residual Plot
  output$resPlot <- renderPlot({
    plot(well()$arima$residuals, 
         main=paste('Well:', input$wellInput, 'Residual Stationarity Plot'),
         xlab="Date",
         ylab="Residuals")
  })

  # ACF Plot
  output$acfPlot <- renderPlot({
    plot(Acf(well()$arima$residuals, lag=25),
         main=paste('Well:', input$wellInput,'ACF Plot'))
  })
  
  # Map
  output$mymap <- renderLeaflet({
    m<-leaflet() %>%
      addTiles() %>%
      setView(lng=well()$locdf$Longitude, lat=well()$locdf$Latitude, zoom=9) %>%
      addMarkers(lng=well()$locdf$Longitude,
                 lat=well()$locdf$Latitude,
                 popup=paste("Well: ", well()$locdf$Well, "<br>",
                             "Well Depth (ft): ", round(well()$mapdf$Wellft,2), "<br>",
                             "Rain Amount (in): ", round(well()$mapdf$Rain,2), "<br>",
                             "Tide (ft): ", round(well()$mapdf$Tide,2)))
    m
  })

  
})


####-----------------------RUN APP----------------------####

# Run the app 
shinyApp(ui, server)

