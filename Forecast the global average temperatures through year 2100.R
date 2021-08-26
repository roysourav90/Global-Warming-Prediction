library(readxl)
library(dplyr)
library(tidyverse)
library(reshape2)
library(zoo)
library(forecast)
library(ggplot2)
library(ggpubr)
# Q1-2
nasa_data <- read_excel(file.choose(),sheet = "NASA_Global-mean monthly, seaso", skip = 1)
met_data <- read_excel(file.choose(),sheet = "MET")

#NASA
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
#MET Group
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


##################### NASA  ##############################################

nasa_ts <- ts(nasa_clean$c,start=1880,frequency=12)
fit_m <- decompose(nasa_ts, type="multiplicative")
plot(fit_m)
summary(fit_m)
par(mfrow=c(1,2))
fit_a <- decompose(nasa_ts, type="additive")
plot(fit_a)
summary(fit_a)

fit_stl <- stl(nasa_ts, t.window=12, s.window="periodic")
plot(fit_stl)
summary(fit_stl)

plot(nasa_ts)

#####
#ETS, errors, trends & seasonailty
#####

nasa_ts_AAN <- ets(nasa_ts, model="AAN", damped=FALSE )
nasa_ts_AAA <- ets(nasa_ts, model="AAA", damped=FALSE)
nasa_ts_MMN <- ets(nasa_ts, model="MMN", damped=FALSE)
nasa_ts_MMM <- ets(nasa_ts, model="MMM", damped=FALSE)

nasa_ts_AAN #5134.759
nasa_ts_AAA #5164.554
nasa_ts_MMN #5136.171
nasa_ts_MMM #5287.600

#Create predictions for 2100 from 2021
nasa_ts_AAN_pred <- forecast(nasa_ts_AAN, h=980, level=0.90)
nasa_ts_AAA_pred <- forecast(nasa_ts_AAA, h=980, level=0.90)
nasa_ts_MMN_pred <- forecast(nasa_ts_MMN, h=980, level=0.90)
nasa_ts_MMM_pred <- forecast(nasa_ts_MMM, h=980, level=0.90)

par(mfrow=c(2,2))
plot(nasa_ts_AAN_pred, xlab="Year", ylab="Predicted Temp in C")
plot(nasa_ts_AAA_pred, xlab="Year", ylab="Predicted Temp in C")
plot(nasa_ts_MMN_pred, xlab="Year", ylab="Predicted Temp in C")
plot(nasa_ts_MMM_pred, xlab="Year", ylab="Predicted Temp in C")

#All 4 predict that values are going up
# Lets see what the TBATS looks like

nasa_tbats <- tbats(nasa_ts)
nasa_tbats #5055.628 AIC
nasa_tbats_pred <-forecast(nasa_tbats, h=980, level=0.90)
par(mfrow=c(1,1))
plot(nasa_tbats_pred, xlab="Year", ylab="Predicted Climate")

###
### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
### 

NASA_f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
NASA_errors_AAA <- tsCV(nasa_ts, NASA_f_AAA, h=1, window=60)

NASA_f_MMM  <- function(y, h) forecast(ets(y, model="MMM"), h = h)
NASA_errors_MMM <- tsCV(nasa_ts,NASA_f_MMM, h=1, window=60)

NASA_f_AAN <- function(y, h) forecast(ets(y, model="AAN"), h = h)
NASA_errors_AAN <- tsCV(nasa_ts,NASA_f_AAN, h=1, window=60)

NASA_f_MMN  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
NASA_errors_MMN <- tsCV(nasa_ts, NASA_f_MMN, h=1, window=60)

NASA_f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
NASA_errors_TBATS <- tsCV(nasa_ts, NASA_f_TBATS , h=1, window=60)



mean(abs(NASA_errors_AAA/nasa_ts),na.rm=TRUE)*100  # 0.7039574
mean(abs(NASA_errors_MMM/nasa_ts),na.rm=TRUE)*100  # 0.6764021
mean(abs(NASA_errors_AAN/nasa_ts),na.rm=TRUE)*100  # 0.6437717
mean(abs(NASA_errors_MMN/nasa_ts),na.rm=TRUE)*100  # 0.645194
mean(abs(NASA_errors_TBATS/nasa_ts),na.rm=TRUE)*100 #  0.6443757



#ARIMA
#1. A "plain vanilla" ARIMA
NASA_arima1_s <- auto.arima(nasa_ts,seasonal=TRUE)
NASA_arima1_s_pred <- forecast(NASA_arima1_s, h=980, level=0.9)
NASA_arima1_ns <- auto.arima(nasa_ts,seasonal=FALSE)
NASA_arima1_ns_pred <- forecast(NASA_arima1_ns, h=980, level=0.9)

AIC(NASA_arima1_s) # AIC:-2751.919
AIC(NASA_arima1_ns) # AIC -2729.349

summary(NASA_arima1_s) # MAPE: 0.5878291
#Training set error measures: 
#                      ME      RMSE        MAE          MPE      MAPE      MASE         ACF1
#Training set -0.0002572106 0.1066954 0.08257798 -0.008284301 0.5878291 0.5619617 0.0002544706
summary(NASA_arima1_ns) # MAPE: 0.5910357

#Training set error measures:
#                      ME      RMSE        MAE          MPE      MAPE      MASE          ACF1
#Training set -0.0001874986 0.1075445 0.08302762 -0.007878609 0.5910357 0.5650216 -0.0003917776


#2.ARIMA on residuals
NASA_arima3 <- tslm(nasa_ts ~ trend + season) # Build a linear model for trend and seasonality
summary(NASA_arima3)
NASA_residarima3 <- auto.arima(NASA_arima3$residuals) # Build ARIMA on it's residuals
NASA_residuals_arima3_pred <- forecast(NASA_residarima3,h = 980, level = 0.9 )
NASA_residualsF <- as.numeric(NASA_residuals_arima3_pred$mean)

NASA_regressionForecast <- forecast(NASA_arima3,h=980,level = 0.9) #forecast from lm
NASA_regressionF <- as.numeric(NASA_regressionForecast$mean)

par(mfrow=c(1,2))
plot(NASA_regressionForecast)
plot(NASA_residuals_arima3_pred)


NASA_forecastR <- data.frame( X = seq.Date(as.Date("2021-04-01"), as.Date("2102-11-01"),by = "month"),
                              Point.Forecast = NASA_regressionF+NASA_residualsF,
                              Lo.90 = NASA_residuals_arima3_pred$lower + NASA_regressionForecast$lower,	
                              Hi.90 = NASA_residuals_arima3_pred$upper + NASA_regressionForecast$upper)





# Cross - Valiation - Sliding Window


# set a couple of parameters we'll use to slice the series into chunks:
# window width (w) and the time step at which you want to end the first
# training set
w = 24 ; start = 1000

# now use those parameters to make a vector of the time steps at which each
# window will end
steps <- seq(start + w, length(nasa_ts), by = w)

# using lapply, iterate the forecasting-and-scoring process over the
# windows that created
cv_list <- lapply(steps, function(x) {
  
  train <- nasa_ts[1:(x - w)] 
  test <- nasa_ts[(x - w + 1):x]
  
  model <- auto.arima(train)
  fcst <- forecast(model, h = w, level = 90)
  accuracy(fcst, test)
  
})

cv_list[[2]]

#                      ME       RMSE        MAE         MPE      MAPE      MASE          ACF1
#Training set 0.001204035 0.10708692 0.08149703 0.002927927 0.5898176 0.9011183 -0.0006078649
#Test set     0.027372166 0.07518278 0.06271051 0.194046568 0.4495417 0.6933944            NA

#choose best model - ARIMA on residuals

write.csv(NASA_forecastR,"nasa.csv")


##################### UKMET  ##############################################

MET_ts <- ts(met_stacked_clean$c,start=1850,frequency=12)

MET_fit_stl <- stl(MET_ts, t.window=12, s.window="periodic")
plot(MET_fit_stl)


par(mfrow=c(1,2))
Acf(diff(MET_ts,12),main="")
Pacf(diff(MET_ts,12),main="")

MET_ts_AAN <- ets(MET_ts, model="AAN", damped=FALSE )
MET_ts_AAA <- ets(MET_ts, model="AAA", damped=FALSE)
MET_ts_MMN <- ets(MET_ts, model="MMN", damped=FALSE)
MET_ts_MMM <- ets(MET_ts, model="MMM", damped=FALSE)


#Create predictions for 2100 from 2021
MET_ts_AAN_pred <- forecast(MET_ts_AAN, h=980, level=0.90)
MET_ts_AAA_pred <- forecast(MET_ts_AAA, h=980, level=0.90)
MET_ts_MMN_pred <- forecast(MET_ts_MMN, h=980, level=0.90)
MET_ts_MMM_pred <- forecast(MET_ts_MMM, h=980, level=0.90)

par(mfrow=c(2,2))
plot(MET_ts_AAN_pred, xlab="Year", ylab="Predicted Temp in C")
plot(MET_ts_AAA_pred, xlab="Year", ylab="Predicted Temp in C")
plot(MET_ts_MMN_pred, xlab="Year", ylab="Predicted Temp in C")
plot(MET_ts_MMM_pred, xlab="Year", ylab="Predicted Temp in C")

#All 4 predict that values are going up
# Lets see what the TBATS looks like

MET_tbats <- tbats(MET_ts)
MET_tbats #5055.628 AIC
MET_tbats_pred <-forecast(MET_tbats, h=980, level=0.90)
par(mfrow=c(1,1))
plot(MET_tbats_pred, xlab="Year", ylab="Predicted Climate")

###
### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
### 

MET_f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
MET_errors_AAA <- tsCV(MET_ts, MET_f_AAA, h=1, window=60)

MET_f_MMM  <- function(y, h) forecast(ets(y, model="MMM"), h = h)
MET_errors_MMM <- tsCV(MET_ts,MET_f_MMM, h=1, window=60)

MET_f_AAN <- function(y, h) forecast(ets(y, model="AAN"), h = h)
MET_errors_AAN <- tsCV(MET_ts,MET_f_AAN, h=1, window=60)

MET_f_MMN  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
MET_errors_MMN <- tsCV(MET_ts, MET_f_MMN, h=1, window=60)

MET_f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
MET_errors_TBATS <- tsCV(MET_ts, MET_f_TBATS , h=1, window=60)



mean(abs(MET_errors_AAA/MET_ts),na.rm=TRUE)*100  # 0.7331699
mean(abs(MET_errors_MMM/MET_ts),na.rm=TRUE)*100  # 0.714072
mean(abs(MET_errors_AAN/MET_ts),na.rm=TRUE)*100  # 0.689412
mean(abs(MET_errors_MMN/MET_ts),na.rm=TRUE)*100  # 0.6969217
mean(abs(MET_errors_TBATS/MET_ts),na.rm=TRUE)*100 # 0.6802507


#ARIMA
#1. A "plain vanilla" ARIMA
MET_arima1_s <- auto.arima(MET_ts,seasonal=TRUE)
MET_arima1_s_pred <- forecast(MET_arima1_s, h=980, level=0.9)
MET_arima1_ns <- auto.arima(MET_ts,seasonal=FALSE)
MET_arima1_ns_pred <- forecast(MET_arima1_ns, h=980, level=0.9)

par(mfrow=c(1,1))
plot(MET_arima1_s_pred)

AIC(MET_arima1_s) # AIC:-2977.129
AIC(MET_arima1_ns) # AIC -2937.75

summary(MET_arima1_s) # MAPE: 0.6442235
#Training set error measures:
#                     ME      RMSE        MAE          MPE      MAPE      MASE         ACF1
#Training set 0.0003691604 0.1166156 0.08933057 -0.005054392 0.6442235 0.5851661 0.0002933387
summary(MET_arima1_ns) # MAPE: 0.6492604

#Training set error measures:
#                      ME      RMSE        MAE          MPE      MAPE      MASE        ACF1
#Training set 0.0004257429 0.1178083 0.09001698 -0.004786085 0.6492604 0.5896624 0.001948082




#2.ARIMA on residuals
MET_arima3 <- tslm(MET_ts ~ trend + season) # Build a linear model for trend and seasonality
summary(MET_arima3)

MET_residarima3 <- auto.arima(MET_arima3$residuals) # Build ARIMA on it's residuals
MET_residuals_arima3_pred <- forecast(MET_residarima3,h = 980, level = 0.9 )
MET_residualsF <- as.numeric(MET_residuals_arima3_pred$mean)

MET_regressionForecast <- forecast(MET_arima3,h=980,level = 0.9) #forecast from lm
MET_regressionF <- as.numeric(MET_regressionForecast$mean)

par(mfrow=c(1,2))
plot(MET_regressionForecast)
plot(MET_residuals_arima3_pred)


MET_forecastR <- data.frame( X = seq.Date(as.Date("2021-02-01"), as.Date("2102-09-01"),by = "month"),
                             Point.Forecast = MET_regressionF+MET_residualsF,
                             Lo.90 = MET_residuals_arima3_pred$lower + MET_regressionForecast$lower,	
                             Hi.90 = MET_residuals_arima3_pred$upper + MET_regressionForecast$upper)





# Cross - Valiation - Sliding Window


# set a couple of parameters we'll use to slice the series into chunks:
# window width (w) and the time step at which you want to end the first
# training set
w = 24 ; start = 1000

# now use those parameters to make a vector of the time steps at which each
# window will end
steps <- seq(start + w, length(MET_ts), by = w)

# using lapply, iterate the forecasting-and-scoring process over the
# windows that created
cv_list <- lapply(steps, function(x) {
  
  train <- MET_ts[1:(x - w)] 
  test <- MET_ts[(x - w + 1):x]
  
  model <- auto.arima(train)
  fcst <- forecast(model, h = w, level = 90)
  accuracy(fcst, test)
  
})

cv_list[[2]]
#                     ME      RMSE       MAE        MPE      MAPE      MASE        ACF1
#Training set 0.003139349 0.1310293 0.1005233 0.01403543 0.7371409 0.9068529 0.001344138
#Test set     0.063320966 0.1277786 0.1107583 0.45142695 0.7987783 0.9991865          NA


#choose best model - ARIMA With seasonality

write.csv(MET_arima1_s_pred,"met.csv")



