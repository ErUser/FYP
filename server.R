#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinyjs)
library(prophet)
library(shinydashboard)
library(plyr)
library(dplyr)
library(tidyverse)
library(GGally)
library(cowplot)
library(mice)
library(VIM)
library(keras)
library(caret)
library(caTools)
library(datetime)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  loadData <- function(){
    files <- list.files("D:/SIM_BIG_DATA/Project/weatherModel/responses", full.names = TRUE)
    totaldataset <- lapply(files, read.csv, stringsAsFactors = FALSE)
    totaldataset <- do.call(rbind, totaldataset)
    
    colnames(totaldataset) <- c("index" ,"Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                                "Rainfall120min","MeanTemp","MaxTemp",
                                "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names
    totaldataset$MaxWindSpd <- as.numeric(as.character(totaldataset$MaxWindSpd))
    totaldataset$MeanWindSpd <- as.numeric(as.character(totaldataset$MeanWindSpd))
    
    
    
    for (i in 1:nrow(totaldataset))
    {
      if (nchar(totaldataset$Month[i]) == 1)
      {
        totaldataset$Month[i] = paste("0", totaldataset$Month[i],sep = "")
      }
    }
    
    for (i in 1:nrow(totaldataset))
    {
      if (nchar(totaldataset$Day[i]) == 1)
      {
        totaldataset$Day[i] = paste("0", totaldataset$Day[i],sep = "")
      }
    }
    
    v <- vector()
    counter <- 0
    
    for (i in 0:nrow(totaldataset))
    {
      v[counter] <- paste(totaldataset$Year[i],"/",totaldataset$Month[i],"/", totaldataset$Day[i],sep="")
      counter <- counter + 1
    }
    
    totaldataset$Date <- v # append new labels into dataset
    
    totaldataset <- select(totaldataset, -c(1,2,3,4,5,7,8,9))  
    
    totaldataset$Date <- as.Date(totaldataset$Date, "%Y/%m/%d")
    
    totaldataset <- totaldataset[order(totaldataset$Date),] #sort dates from 2010 to 2019
    
    totaldataset <- totaldataset[, c(7,1,2,3,4,5,6)]
    
    tempData <- mice(totaldataset,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
    
    
    totaldataset <- complete(tempData,1)#fill in the missing values in dataset
    
  }
  
  loadData_reactive <- reactive({
    files <- list.files("D:/SIM_BIG_DATA/Project/weatherModel/responses", full.names = TRUE)
    totaldataset <- lapply(files, read.csv, stringsAsFactors = FALSE)
    totaldataset <- do.call(rbind, totaldataset)
    
    colnames(totaldataset) <- c("index" ,"Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                                "Rainfall120min","MeanTemp","MaxTemp",
                                "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names
    totaldataset$MaxWindSpd <- as.numeric(as.character(totaldataset$MaxWindSpd))
    totaldataset$MeanWindSpd <- as.numeric(as.character(totaldataset$MeanWindSpd))
    
    
    
    for (i in 1:nrow(totaldataset))
    {
      if (nchar(totaldataset$Month[i]) == 1)
      {
        totaldataset$Month[i] = paste("0", totaldataset$Month[i],sep = "")
      }
    }
    
    for (i in 1:nrow(totaldataset))
    {
      if (nchar(totaldataset$Day[i]) == 1)
      {
        totaldataset$Day[i] = paste("0", totaldataset$Day[i],sep = "")
      }
    }
    
    v <- vector()
    counter <- 0
    
    for (i in 0:nrow(totaldataset))
    {
      v[counter] <- paste(totaldataset$Year[i],"/",totaldataset$Month[i],"/", totaldataset$Day[i],sep="")
      counter <- counter + 1
    }
    
    totaldataset$Date <- v # append new labels into dataset
    
    totaldataset <- select(totaldataset, -c(1,2,3,4,5,7,8,9))  
    
    totaldataset$Date <- as.Date(totaldataset$Date, "%Y/%m/%d")
    
    totaldataset <- totaldataset[order(totaldataset$Date),] #sort dates from 2010 to 2019
    
    totaldataset <- totaldataset[, c(7,1,2,3,4,5,6)]
    
    tempData <- mice(totaldataset,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
    
    
    totaldataset <- complete(tempData,1)#fill in the missing values in dataset
    
  })
 
  saveData <- function(data){
    #outputDir <- "responses"
    #data <- t(data)
    
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    str(data)
    
    write.csv(
      x = data,
      file = file.path("D:/SIM_BIG_DATA/Project/weatherModel/responses", fileName),
      quote = TRUE
    )
  }
  
  prophet_function <- function(ds, y) {
    df <- data.frame(ds,y)
    m <- prophet(df)
    futurepred <- make_future_dataframe(m, periods = 365)
    forecast <- predict(m,futurepred)
    return(forecast)
  }
  
  
  

  
  get_futureValue <- function(date, future_value){
    value <- filter(future_value, as.Date(ds) == date)
    value$yhat <- format(round(value$yhat, 0))
    return(value$yhat)
  }
  
 

  v <- reactiveValues(data = NULL)
  
  totaldataset <- loadData()
 
  meanwindspd <- prophet_function(totaldataset$Date, totaldataset$MeanWindSpd)
  meantemp <- prophet_function(totaldataset$Date, totaldataset$MeanTemp)
  maxtemp <- prophet_function(totaldataset$Date, totaldataset$MaxTemp)
  mintemp <- prophet_function(totaldataset$Date, totaldataset$MinTemp)

  
 # output$btn_future_day <- renderUI({
  #  lapply(0:6, function(val){
      
    #  actionButton(paste0("btn_", val), htmlOutput(paste0("future_day_", val)), class = "btn-action")
      
  #  })
#  })
 # 

  observe({
    output$current_datetime <- renderUI({
      
      if(is.null(v$data)) return()
      future_meanwindspd <- filter(meanwindspd, as.Date(ds) == v$data)
      future_meantemp <- filter(meantemp, as.Date(ds) == v$data)
      future_minTemp <- get_futureValue(v$data, mintemp)
      future_maxtemp <- get_futureValue(v$data, maxtemp)
      #curr_pred <- filter(forecast_2020, as.Date(ds) == v$data)
      future_meanwindspd$yhat <- format(round(future_meanwindspd$yhat, 2), nsmall = 2)
      future_meantemp$yhat <- format(round(future_meantemp$yhat, 2), nsmall = 2)
      HTML(paste(v$data, paste("Mean Wind Speed:",future_meanwindspd$yhat, "km/h"), paste("Mean Temperature:", future_meantemp$yhat, "\u00B0C"), paste(future_minTemp,"\u00B0C", " - ", future_maxtemp,"\u00B0C"), sep = '<br/>'))
    })
    
    output$display_temp <- renderUI({
      if(is.null(v$data)) return({
        invalidateLater(1000, session)
        current_date <- as.Date(as.POSIXct(Sys.Date(), tz="GMT"))
        paste(get_futureValue(current_date, meantemp))
      })
      HTML(paste(get_futureValue(v$data, meantemp)))
    })
    
    lapply(0:6, function(val){
      invalidateLater(1000, session)
      current_date <- as.Date(as.POSIXct(Sys.Date(), tz="GMT"))
      future_d <- current_date + val
      future_meantemp <- filter(meantemp, as.Date(ds) == future_d)
      future_meantemp$yhat <- format(round(future_meantemp$yhat, 2), nsmall = 2)
      output[[paste0("future_day_", val)]] <- renderUI(HTML(paste(weekdays(future_d, abbreviate = TRUE), paste(future_meantemp$yhat, "\u00B0C"), sep='<br/>')))
     
    })
    
    updateSliderInput(session, "year", value = max(year(totaldataset$Date)), min = min(year(totaldataset$Date)), max =  max(year(totaldataset$Date)))
    
  })

  lapply(0:6, function(val){
    observeEvent(input[[paste0("btn_", val)]], {
      current_date <- as.Date(as.POSIXct(Sys.Date(), tz="GMT"))
      future_d <- current_date + val
      v$data <- future_d
    })
  })
  
  output$btn_future_day <- renderUI({
    lapply(0:6, function(val){
      
      actionButton(paste0("btn_", val), htmlOutput(paste0("future_day_", val)), class = "btn-action")
      
    })
  })
  
  output$currentDay <- renderText({
    
    if(is.null(v$data)) return({
      invalidateLater(1000, session)
      current_date <- as.Date(as.POSIXct(Sys.Date(), tz="GMT"))
      
      paste(weekdays(current_date))
    })
    paste(weekdays(v$data))
  })
  
  output$degree <- renderText("\u00B0C" )
  

  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    #current_date <- as.Date(as.POSIXct(Sys.Date(), tz="GMT"))
    paste(Sys.time())
  })
  
  output$year_slider <- renderUI({
    min_year <- min(year(totaldataset$Date))
    max_year <- max(year(totaldataset$Date))
 
            
    sliderInput('year',
                'Select StartYear',
                round = FALSE,
                step = 1,
                min = min_year,
                max = max_year,
                value = max_year)
  })
  
  output$weatherSg <- renderPlot({
    dataset_year <- filter(totaldataset, year(Date)== input$year)
    MaxTemp_show <- dataset_year %>% 
      ggplot(aes(Date, MeanTemp)) + 
      geom_point() + 
      geom_line() +
      labs(
        title = paste(input$year, " Mean Temperature(Daily)")
      )
    plot(MaxTemp_show)
  })
  
  observeEvent(input$btn_file, {
    inFile <- input$dataset_file
    
    if (is.null(inFile))
      return(NULL)
    
    #dataset <- read.csv(inFile)
    dataset <- read.csv(inFile$datapath)
    
    saveData(dataset)
    totaldataset <<- loadData()
    
    meanwindspd <<- prophet_function(totaldataset$Date, totaldataset$MeanWindSpd)
    meantemp <<- prophet_function(totaldataset$Date, totaldataset$MeanTemp)
    maxtemp <<- prophet_function(totaldataset$Date, totaldataset$MaxTemp)
    mintemp <<- prophet_function(totaldataset$Date, totaldataset$MinTemp)
    
   
    #trigger table
    output$contents <- DT::renderDataTable({
      options = list(scrollx = TRUE)
      #totaldataset$Date <- format(totaldataset$date, '%Y-%m-%d')
      totaldataset 
    })
    
    #trigger button displaying temperature
    output$btn_future_day <- renderUI({
      lapply(0:6, function(val){
        actionButton(paste0("btn_", val), htmlOutput(paste0("future_day_", val)), class = "btn-action")
        
      })
    })
  })
})


