# Load the required libraries
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

#Create date column
citibike$time <- as.POSIXct(paste(citibike$year, citibike$month, citibike$day, citibike$hour),
                            format="%Y %m %d %H", tz="UTC")
# Filter the data from January 2023 to April 2023 and hence the rows corresponding to those postions in the data set.
row_position <- which(rownames(citibike) == "X2023.04.30.23")

train_data <- citibike[1:row_position, ]
test_data <- citibike[(row_position+1):nrow(citibike), ]

# Create time series objects for the training and testing set (frequency = 24 hours) for the original deman
train_ts <- ts(train_data$demand, start = c(2023,1), frequency = 24)
test_ts <- ts(test_data$demand, start = c(2023, 5), frequency = 24)

#Create the data for the series in first differences
citibike$diff_demand[1] <- 0
citibike$diff_demand[2:3624] <- diff(citibike$demand)

#Create time series objects for the training and testing
diff_train_ts <- ts(train_data$diff_demand, start = c(2023,1), frequency = 24)
diff_test_ts <- ts(test_data$diff_demand, start = c(2023, 5), frequency = 24)

# Plot ACF and PACF to identify the AR and MA terms
# The lags are scaled because of the hourly data
#Note that the ACF plot in particular shows seasonality but in PACF we could possibly recognise a pattern
par(mfrow = c(1, 2))
acf(train_ts, main = "ACF of Demand", lag.max = 100)
pacf(train_ts, main = "PACF of Demand", lag.max = 100)

par(mfrow = c(1, 2))
acf(diff_train_ts, main = "ACF of Change in Demand", lag.max = 100)
pacf(diff_train_ts, main = "PACF of Change in Demand", lag.max = 100)

par(mfrow = c(1, 2))
acf(diff_train_ts, main = "ACF of Change in  Demand", lag.max = 12)
pacf(diff_train_ts, main = "PACF of Change in Demand", lag.max = 12)

#Manual fitting
fit_arima <- arima(train_ts, order = c(2, 1, 1))
summary(fit_arima)

#Automatic fitting we restrict to non seasonal for now
fit_arima_auto <- auto.arima(train_ts , seasonal = FALSE)
summary(fit_arima_auto)

