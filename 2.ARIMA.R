# Load the required libraries
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

#Not really necessary but is cleaner
citibike$time <- as.POSIXct(paste(citibike$year, citibike$month, citibike$day, citibike$hour),
                            format="%Y %m %d %H", tz="UTC")

# Filter the data from January 2023 to April 2023
train_data <- citibike %>%
  filter(time >= "2023-01-01" & time <= "2023-04-30")

# Create a time series object for the 'demand' variable (frequency = 24 hours)
train_ts <- ts(train_data$demand, start = c(2023, 1), frequency = 24)

adf_test <- adf.test(train_ts)
print(adf_test)

# Plot ACF and PACF to identify the AR and MA terms
# The lags are scaled because of the hourly data
#Note that the ACF plot in particular shows seasonality but in PACF we could possibly recognise a pattern
par(mfrow = c(1, 2))
acf(train_ts, main = "ACF of Demand")
pacf(train_ts, main = "PACF of Demand")

#Plots for one day
acf(train_ts, lag.max = 24, main = "ACF of Demand")
pacf(train_ts, lag.max = 24, main = "PACF of Demand")

#Manual fitting
#Not sure about this fit still 
# Fit ARIMA model, assuming AR(1), MA(6) components as a starting point
fit_arima <- arima(train_ts, order = c(1, 0, 6))
summary(fit_arima)

#Automatic fitting we restrict to non seasonal for now
fit_arima_auto <- auto.arima(train_ts , seasonal = FALSE)
summary(fit_arima_auto)


#Maybe we should include frequency monthly and yearly?

