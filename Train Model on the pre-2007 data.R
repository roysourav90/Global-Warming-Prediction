# Q5 - Q7 

library(readxl)
library(dplyr)
library(tidyverse)
library(reshape2)
library(zoo)
library(forecast)
library(ggplot2)
library(ggpubr)
# Import and clean data
nasa_data <- read_excel(file.choose(),sheet = "NASA_Global-mean monthly, seaso", skip = 1)
met_data <- read_excel(file.choose(),sheet = "MET")

#NASA - Clean data
#Repair data
global_average_in_c <- 14.0
global_average_in_f <- 57.2


#Let's setup the data to be in a format TS can understand
just_months <- dplyr::select(nasa_data,2:13)

#Transpose
just_months_t <- t(just_months)

#Melt
nasa_stacked <-melt(just_months_t, id.vars=1)

#Re-hydrate the years
nasa_stacked$Var2 <- nasa_stacked$Var2 + 1879

#Date column
nasa_stacked$Date <- as.yearmon(paste(nasa_stacked$Var2,nasa_stacked$Var1), "%Y %b")

#Get it back to actual degrees 
nasa_stacked$c <- as.numeric(levels(nasa_stacked$value)[nasa_stacked$value]) + 14

str(nasa_stacked)
#Find missing values
which(is.na(nasa_stacked$c))

#Removed them as we don't have that data yet
nasa_clean <- nasa_stacked[1:1695,]

summary(nasa_clean)

str(nasa_clean)


######
#MET Group - clean data
######
summary(met_data)

## MET DATA
#Let's setup the data to be in a format TS can understand
just_months_met <- dplyr::select(met_data,2:13)

#Transpose
just_months_t_met <- t(just_months_met)

#Melt
met_stacked <-melt(just_months_t_met, id.vars=1)

#Re-hydrate the years
met_stacked$Var2 <- met_stacked$Var2 + 1849

#Date column
met_stacked$Date <- as.yearmon(paste(met_stacked$Var2,met_stacked$Var1), "%Y %b")

#Get it back to actual degrees c
met_stacked$c <- as.numeric(met_stacked$value) + 14

#Find missing values
which(is.na(met_stacked$c))

#Removing odd data for months 2021 Feb to 2021 Dec as the value = -9.99999.
n<-dim(met_stacked)[1]
met_stacked_clean <- met_stacked[1:(n-11),]
tail(met_stacked_clean)
head(met_stacked_clean)

str(met_stacked_clean)


#q5
NASA.2008 <- subset(nasa_clean,Var2 >= 2008)
NASA.2008_ts <-  ts(NASA.2008$c,start=2008,frequency=12)
NASA_fit2008_stl <- stl(NASA.2008_ts, t.window=12, s.window="periodic")
plot(NASA_fit2008_stl)


MET.2008 <- subset(met_stacked_clean,Var2 >= 2008)
MET.2008_ts <-  ts(MET.2008$c,start=2008,frequency=12)
MET_fit2008_stl <- stl(MET.2008_ts, t.window=12, s.window="periodic")
plot(MET_fit2008_stl)

#Q6
###### NASA #################
#build train data unitl 2007 and test data from 2007 - 2017 
NASA.train.pre2007 <- subset(nasa_clean,Var2 < 2007)
NASA.test.post2007 <- subset(nasa_clean,Var2 >= 2007)
NASA.test.2007_2017 <- subset(NASA.test.post2007,Var2 <= 2017)

nasa.train.pre2007_ts <- ts(NASA.train.pre2007$c,start=1880,frequency=12)

# Retrain and forcast NASA Data using ARIMA on residuals 
NASA_train2007 <- tslm(nasa.train.pre2007_ts ~ trend + season)
NASA_train2007.resid <- auto.arima(NASA_train2007$residuals)
NASA_train2007.resid_pred <- forecast(NASA_train2007.resid, h = 132) # forcast 2007-2017
NASA_train2007.resid_predF <- as.numeric(NASA_train2007.resid_pred$mean)

NASA_train2007.regressionForecast <- forecast(NASA_train2007, h = 132)
NASA_train2007.regressionF <- as.numeric(NASA_train2007.regressionForecast$mean)

NASA_train2007.forecastR <- data.frame( X = seq.Date(as.Date("2008-01-01"), as.Date("2017-12-31"),by = "month"),
                              Point.Forecast = NASA_train2007.resid_predF +NASA_train2007.regressionF)

NASA_train2007.error <- (NASA.test.2007_2017$c - NASA_train2007.forecastR$Point.Forecast)

mean(abs(NASA_train2007.error/NASA.test.2007_2017$c),na.rm=TRUE)*100 # MAPE: 0.9063
                             

# forcast NASA Data using Naive Forecasting 

NASA_train2007.naive <- naive(nasa.train.pre2007_ts, h = 132)
NASA_train2007.naiveF <- as.numeric(NASA_train2007.naive$mean[1:132])

NASA_train2007.naive.error <- (NASA.test.2007_2017$c - NASA_train2007.naiveF)
mean(abs(NASA_train2007.naive.error/NASA.test.2007_2017$c),na.rm=TRUE)*100 # MAPE: 1.0323


###### MET #################
#build train data unitl 2007 and test data from 2007 - 2017 
MET.train.pre2007 <- subset(met_stacked_clean,Var2 < 2007)
MET.test.post2007 <- subset(met_stacked_clean,Var2 >= 2007)
MET.test.2007_2017 <- subset(MET.test.post2007,Var2 <= 2017)

MET.train.pre2007_ts <- ts(MET.train.pre2007$c,start=1850,frequency=12)

# Retrain and forcast MET Data using ARIMA With seasonality

MET.train.arima <- auto.arima(MET.train.pre2007_ts,seasonal=TRUE)
MET_MET.train2007.pred <- forecast(MET.train.arima, h=132)

MET_train2007.error <- (MET.test.2007_2017$c - MET_MET.train2007.pred$mean)
mean(abs(MET_train2007.error/MET.test.2007_2017$c),na.rm=TRUE)*100 # MAPE: 0.9732

# forcast MET Data using Naive Forecasting 
MET_train2007.naive <- naive(MET.train.pre2007_ts, h = 132)
MET_train2007.naiveF <- as.numeric(MET_train2007.naive$mean[1:132])

MET_train2007.naive.error <- (MET.test.2007_2017$c - MET_train2007.naiveF)
mean(abs(MET_train2007.naive.error/MET.test.2007_2017$c),na.rm=TRUE)*100 # MAPE: 0.9787



#Q7
###### NASA #################
# build train data unitl 1999 and test data from 1999 - 2009 & 1999 - 2019
NASA.train.pre1999 <- subset(nasa_clean,Var2 < 1999)
NASA.test.post1999 <- subset(nasa_clean,Var2 >= 1999)
NASA.test.1999_2009 <- subset(NASA.test.post1999,Var2 <= 2009)
NASA.test.1999_2019 <- subset(NASA.test.post1999,Var2 <= 2019)

nasa.train.pre1999_ts <- ts(NASA.train.pre1999$c,start=1880,frequency=12)

# Retrain and forcast NASA Data using ARIMA on residuals 
NASA_train1999 <- tslm(nasa.train.pre1999_ts ~ trend + season)
NASA_train1999.resid <- auto.arima(NASA_train1999$residuals)
NASA_train2009.resid_pred <- forecast(NASA_train1999.resid, h = 132) # forcast 1999-2009
NASA_train2019.resid_pred <- forecast(NASA_train1999.resid, h = 252) # forcast 1999-2019

NASA_train2009.resid_predF <- as.numeric(NASA_train2009.resid_pred$mean)
NASA_train2019.resid_predF <- as.numeric(NASA_train2019.resid_pred$mean)


NASA_train2009.regressionForecast <- forecast(NASA_train1999, h = 132) # forcast 1999-2009
NASA_train2009.regressionF <- as.numeric(NASA_train2009.regressionForecast$mean)

NASA_train2019.regressionForecast <- forecast(NASA_train1999, h = 252) # forcast 1999-2019
NASA_train2019.regressionF <- as.numeric(NASA_train2019.regressionForecast$mean)


NASA_train2009.forecastR <- data.frame( X = seq.Date(as.Date("1999-01-01"), as.Date("2009-12-31"),by = "month"),
                                        Point.Forecast = NASA_train2009.resid_predF +NASA_train2009.regressionF)

NASA_train2009.error <- (NASA.test.1999_2009$c - NASA_train2009.forecastR$Point.Forecast)

mean(abs(NASA_train2009.error/NASA.test.1999_2009$c),na.rm=TRUE)*100 # MAPE: 1.0348

NASA_train2019.forecastR <- data.frame( X = seq.Date(as.Date("1999-01-01"), as.Date("2019-12-31"),by = "month"),
                                        Point.Forecast = NASA_train2019.resid_predF +NASA_train2019.regressionF)

NASA_train2019.error <- (NASA.test.1999_2019$c - NASA_train2019.forecastR$Point.Forecast)

mean(abs(NASA_train2019.error/NASA.test.1999_2019$c),na.rm=TRUE)*100 # MAPE: 1.4904



# forcast NASA Data using Naive Forecasting 

NASA_train2009.naive <- naive(nasa.train.pre1999_ts, h = 132)
NASA_train2009.naiveF <- as.numeric(NASA_train2009.naive$mean[1:132])

NASA_train2009.naive.error <- (NASA.test.1999_2009$c - NASA_train2009.naiveF)
mean(abs(NASA_train2009.naive.error/NASA.test.1999_2009$c),na.rm=TRUE)*100 # MAPE: 0.7936


NASA_train2019.naive <- naive(nasa.train.pre1999_ts, h = 252)
NASA_train2019.naiveF <- as.numeric(NASA_train2019.naive$mean[1:252])

NASA_train2019.naive.error <- (NASA.test.1999_2019$c - NASA_train2019.naiveF)
mean(abs(NASA_train2019.naive.error/NASA.test.1999_2019$c),na.rm=TRUE)*100 # MAPE: 1.2620


###### MET #################
#build train data unitl 1999 and test data from 1999 - 2010 & 1999 - 2020
MET.train.pre1999 <- subset(met_stacked_clean,Var2 < 1999)
MET.test.post1999 <- subset(met_stacked_clean,Var2 >= 1999)
MET.test.1999_2009 <- subset(MET.test.post1999,Var2 <= 2009)
MET.test.1999_2019 <- subset(MET.test.post1999,Var2 <= 2019)

MET.train.pre1999_ts <- ts(MET.train.pre1999$c,start=1850,frequency=12)

# Retrain and forcast MET Data using ARIMA With seasonality

MET.train.arima <- auto.arima(MET.train.pre1999_ts,seasonal=TRUE)
MET_MET.train2009.pred <- forecast(MET.train.arima, h=132) # forecast 1999 - 2009
MET_MET.train2019.pred <- forecast(MET.train.arima, h=252) # forecast 1999 - 2019

MET_train2009.error <- (MET.test.1999_2009$c - MET_MET.train2009.pred$mean)
mean(abs(MET_train2009.error/MET.test.1999_2009$c),na.rm=TRUE)*100 # MAPE: 1.0159

MET_train2019.error <- (MET.test.1999_2019$c - MET_MET.train2019.pred$mean)
mean(abs(MET_train2019.error/MET.test.1999_2019$c),na.rm=TRUE)*100 # MAPE: 1.4599

# forcast MET Data using Naive Forecasting 
MET_train2009.naive <- naive(MET.train.pre1999_ts, h = 132)
MET_train2009.naiveF <- as.numeric(MET_train2009.naive$mean[1:132])

MET_train2009.naive.error <- (MET.test.1999_2009$c - MET_train2009.naiveF)
mean(abs(MET_train2009.naive.error/MET.test.1999_2009$c),na.rm=TRUE)*100 # MAPE: 0.7522


MET_train2019.naive <- naive(MET.train.pre1999_ts, h = 252)
MET_train2019.naiveF <- as.numeric(MET_train2020.naive$mean[1:252])

MET_train2019.naive.error <- (MET.test.1999_2019$c - MET_train2019.naiveF)
mean(abs(MET_train2019.naive.error/MET.test.1999_2019$c),na.rm=TRUE)*100 # MAPE: 1.2092


