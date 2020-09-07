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

year1 <- dataset2019
year2 <- rbind(dataset2019, dataset2018)
year5 <- rbind(dataset2019, dataset2018, dataset2017, dataset2016, dataset2015)

colnames(year1) <- c("Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                     "Rainfall120min","MeanTemp","MaxTemp",
                     "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names

colnames(year2) <- c("Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                     "Rainfall120min","MeanTemp","MaxTemp",
                     "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names

colnames(year5) <- c("Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                     "Rainfall120min","MeanTemp","MaxTemp",
                     "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names

#10 years
totaldataset$MaxWindSpd <- as.numeric(as.character(totaldataset$MaxWindSpd))
totaldataset$MeanWindSpd <- as.numeric(as.character(totaldataset$MeanWindSpd))

totaldataset[totaldataset=="â"]<-NA # fill empty spaces with NA

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

for (i in 0:3652)
{
  v[counter] <- paste(totaldataset$Year[i],"/",totaldataset$Month[i],"/", totaldataset$Day[i],sep="")
  counter <- counter + 1
}

totaldataset$Date <- v # append new labels into dataset

totaldataset <- select(totaldataset, -c(1,2,3,4,6,7,8))

totaldataset$Date <- as.Date(totaldataset$Date, "%Y/%m/%d")

totaldataset <- totaldataset[order(totaldataset$Date),] #sort dates from 2010 to 2019

totaldataset <- totaldataset[, c(7,2,3,4,5,6)]

glimpse(totaldataset)

sum(is.na(totaldataset))

md.pattern(totaldataset)

aggr_plot <- aggr(totaldataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(totaldataset,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

totaldataset <- complete(tempData,1)#fill in the missing values in dataset

#1 years
year1$MaxWindSpd <- as.numeric(as.character(year1$MaxWindSpd))
year1$MeanWindSpd <- as.numeric(as.character(year1$MeanWindSpd))

year1[year1=="â"]<-NA # fill empty spaces with NA

for (i in 1:nrow(year1))
{
  if (nchar(year1$Month[i]) == 1)
  {
    year1$Month[i] = paste("0", year1$Month[i],sep = "")
  }
}

for (i in 1:nrow(year1))
{
  if (nchar(year1$Day[i]) == 1)
  {
    year1$Day[i] = paste("0", year1$Day[i],sep = "")
  }
}

v <- vector()
counter <- 0

for (i in 0:365)
{
  v[counter] <- paste(year1$Year[i],"/",year1$Month[i],"/", year1$Day[i],sep="")
  counter <- counter + 1
}

year1$Date <- v # append new labels into dataset

year1 <- select(year1, -c(1,2,3,4,6,7,8))

year1$Date <- as.Date(year1$Date, "%Y/%m/%d")

year1 <- year1[order(year1$Date),] #sort dates from 2010 to 2019

year1 <- year1[, c(7,2,3,4,5,6)]

glimpse(year1)

sum(is.na(year1))

md.pattern(year1)

aggr_plot <- aggr(year1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(year1,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

year1 <- complete(tempData,1)#fill in the missing values in dataset

#2 years
year2$MaxWindSpd <- as.numeric(as.character(year2$MaxWindSpd))
year2$MeanWindSpd <- as.numeric(as.character(year2$MeanWindSpd))

year2[year2=="â"]<-NA # fill empty spaces with NA

for (i in 1:nrow(year2))
{
  if (nchar(year2$Month[i]) == 1)
  {
    year2$Month[i] = paste("0", year2$Month[i],sep = "")
  }
}

for (i in 1:nrow(year2))
{
  if (nchar(year2$Day[i]) == 1)
  {
    year2$Day[i] = paste("0", year2$Day[i],sep = "")
  }
}

v <- vector()
counter <- 0

for (i in 0:730)
{
  v[counter] <- paste(year2$Year[i],"/",year2$Month[i],"/", year2$Day[i],sep="")
  counter <- counter + 1
}

year2$Date <- v # append new labels into dataset

year2 <- select(year2, -c(1,2,3,4,6,7,8))

year2$Date <- as.Date(year2$Date, "%Y/%m/%d")

year2 <- year2[order(year2$Date),] #sort dates from 2010 to 2019

year2 <- year2[, c(7,2,3,4,5,6)]

glimpse(year2)

sum(is.na(year2))

md.pattern(year2)

aggr_plot <- aggr(year2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(year2,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

year2 <- complete(tempData,1)#fill in the missing values in dataset

#5 years
year5$MaxWindSpd <- as.numeric(as.character(year5$MaxWindSpd))
year5$MeanWindSpd <- as.numeric(as.character(year5$MeanWindSpd))

year5[year5=="â"]<-NA # fill empty spaces with NA

for (i in 1:nrow(year5))
{
  if (nchar(year5$Month[i]) == 1)
  {
    year5$Month[i] = paste("0", year5$Month[i],sep = "")
  }
}

for (i in 1:nrow(year5))
{
  if (nchar(year5$Day[i]) == 1)
  {
    year5$Day[i] = paste("0", year5$Day[i],sep = "")
  }
}

v <- vector()
counter <- 0

for (i in 0:1826)
{
  v[counter] <- paste(year5$Year[i],"/",year5$Month[i],"/", year5$Day[i],sep="")
  counter <- counter + 1
}

year5$Date <- v # append new labels into dataset

year5 <- select(year5, -c(1,2,3,4,6,7,8))

year5$Date <- as.Date(year5$Date, "%Y/%m/%d")

year5 <- year5[order(year5$Date),] #sort dates from 2010 to 2019

year5 <- year5[, c(7,2,3,4,5,6)]

glimpse(year5)

sum(is.na(year5))

md.pattern(year5)

aggr_plot <- aggr(year5, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(year5,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

year5 <- complete(tempData,1)#fill in the missing values in dataset

year1 <- select(year1, -c(1,2,3,4,5,6,7,8))

year2 <- select(year2, -c(1,2,3,4,5,6,7,8))

year5 <- select(year5, -c(1,2,3,4,5,6,7,8))

glimpse(totaldataset)
glimpse(year1)
glimpse(year2)
glimpse(year5)


setwd("C:/Users/Lorfer/Desktop/Dataset/New Dataset/2020")# set directory where csv file is
dataset2020 <- read.csv("Changi2020.csv")

colnames(dataset2020) <- c("Station","Year","Month","Day","TotalRainfall","Rainfall30min","Rainfall60min",
                           "Rainfall120min","MeanTemp","MaxTemp",
                           "MinTemp","MeanWindSpd","MaxWindSpd") # change to easier names

dataset2020$MaxWindSpd <- as.numeric(as.character(dataset2020$MaxWindSpd))

md.pattern(dataset2020)

aggr_plot <- aggr(dataset2020, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(dataset2020,m=5,maxit=50,meth='pmm',seed=500)# uses imputation by predictive mean matching
summary(tempData)

dataset2020 <- complete(tempData,1)#fill in the missing values in dataset

#prophet model(GAM - generalized additive model)
library(prophet)
#meantemp, maxtemp, mintemp, meanwindspd, maxwindspd
ds <- year5$Date
y <- year5$MaxWindSpd
df <- data.frame(ds,y)
#view(df)

m <- prophet(df)
futurepred <- make_future_dataframe(m, periods = 365)
tail(futurepred)
forecast <- predict(m,futurepred)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# yhat is our predicted forecast, 
#yhat_lower is the lower bound for our predictions and 
#yhat_upper is the upper bound for our predictions.

plot(m,forecast)
prophet_plot_components(m, forecast)

df.cv <- cross_validation(m, initial = 730, period = 180, horizon = 365, units = 'days')
#horizon how many days u predict before using it as set
#period making a prediction every 180 days
head(df.cv)


df.p <- performance_metrics(df.cv)
head(df.p)

plot_cross_validation_metric(df.cv, metric = 'mape')
#maybe scale values so that it works with prophet

#2010 - 365
#2011 - 730
#2012 - 1096
#2013 - 1461
#2014 - 1826
#2015 - 2191
#2016 - 2557
#2017 - 2922
#2018 - 3287
#2019 - 3652

library(Metrics)

a <- tail(forecast$yhat, 365)
b <- head(a, 152)
c <- dataset2020$MaxWindSpd
show(b)
show(c)
mse(b,c)
rmse(b,c)
mae(b,c)
mape(b,c)

# trial <- function(x){(x-min(x))/(max(x)-min(x))}#scale data
# change <- function(x){x * (90.7 - 20.9) + 20.9}#unscale data
# g <-trial(totaldataset$MaxWindSpd)
# totaldataset$MaxWindSpd <- g
# h <- change(a)
# b <- head(h,n=152)
# summary(dataset2020)



