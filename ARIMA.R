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

#2019 dataset preprocess start
setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2019")# set directory where csv file is
dataset2019 <- read.csv("Changi2019.csv")

#2018 dataset preprocess start
setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2018")# set directory where csv file is
dataset2018 <- read.csv("Changi2018.csv")

#2017 dataset preprocess start
setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2017")# set directory where csv file is
dataset2017 <- read.csv("Changi2017.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2016")# set directory where csv file is
dataset2016 <- read.csv("Changi2016.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2015")# set directory where csv file is
dataset2015 <- read.csv("Changi2015.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2014")# set directory where csv file is
dataset2014 <- read.csv("Changi2014.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2013")# set directory where csv file is
dataset2013 <- read.csv("Changi2013.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2012")# set directory where csv file is
dataset2012 <- read.csv("Changi2012.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2011")# set directory where csv file is
dataset2011 <- read.csv("Changi2011.csv")

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2010")# set directory where csv file is
dataset2010 <- read.csv("Changi2010.csv")

totaldataset <- rbind(dataset2019, dataset2018, dataset2017, dataset2016, dataset2015, 
                      dataset2014, dataset2013, dataset2012, dataset2011, dataset2010)

colnames(totaldataset) <- c("Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                            "Rainfall120min","MeanTemp","MaxTemp",
                            "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names

glimpse(totaldataset)

process <- function(data)
{

data$MaxWindSpd <- as.numeric(as.character(data$MaxWindSpd))
data$MeanWindSpd <- as.numeric(as.character(data$MeanWindSpd))

data[data=="â"]<-NA # fill empty spaces with NA

for (i in 1:nrow(data))
{
  if (nchar(data$Month[i]) == 1)
  {
    data$Month[i] = paste("0", data$Month[i],sep = "")
  }
}

for (i in 1:nrow(data))
{
  if (nchar(data$Day[i]) == 1)
  {
    data$Day[i] = paste("0", data$Day[i],sep = "")
  }
}

v <- vector()
counter <- 0

for (i in 0:3652)
{
  v[counter] <- paste(data$Year[i],"/",data$Month[i],"/", data$Day[i],sep="")
  counter <- counter + 1
}

data$Date <- v # append new labels into dataset

data <- select(data, -c(1,2,3,4,6,7,8))

data$Date <- as.Date(data$Date, "%Y/%m/%d")

data <- data[order(data$Date),] #sort dates from 2010 to 2019

data <- data[, c(7,2,3,4,5,6)]

glimpse(data)

sum(is.na(data))

md.pattern(data)

aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

data <- complete(tempData,1)#fill in the missing values in dataset
return(data)
}

ntotaldataset <- process(totaldataset)

setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2020")# set directory where csv file is
dataset2020 <- read.csv("Changi2020.csv")

colnames(dataset2020) <- c("Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                           "Rainfall120min","MeanTemp","MaxTemp",
                           "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names

dataset2020$MaxWindSpd <- as.numeric(as.character(dataset2020$MaxWindSpd))

md.pattern(dataset2020)

aggr_plot <- aggr(dataset2020, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(dataset2020,m=5,maxit=20,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

dataset2020 <- complete(tempData,1)#fill in the missing values in dataset

library(zoo)
library(xts)
library(forecast)
#non-stationary data arima works badly
arima_model <- function(data)
{
trial <- data

model <- auto.arima(trial, trace = TRUE)

model

forecast = predict(model,152)

forecast$pred

return(accuracy(model))
}


arima_model(ntotaldataset$MeanTemp)
arima_model(ntotaldataset$MaxTemp)
arima_model(ntotaldataset$MinTemp)
arima_model(ntotaldataset$MeanWindSpd)
arima_model(ntotaldataset$MaxWindSpd)



