# Plot ACF and PACF to identify the AR and MA terms again and now we should also account for seasonality
# The lags are scaled because of the hourly data
par(mfrow = c(1, 2))
acf(train_ts, main = "ACF of Demand")
pacf(train_ts, main = "PACF of Demand")

#Manual
#We should still decide what is best
#Because we see seasonal patterns in both plots add a seasonal AR(1) and MA(1)
#Based on this link analysis:
#https://spureconomics.com/interpreting-acf-and-pacf-plots/
sarima_model <- arima(train_ts,
                      order = c(1, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                      seasonal = list(order = c(1, 0, 1), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168

# Summary of the fitted model
summary(sarima_model)

#Automatic fitting including seasonal
fit_arima_auto <- auto.arima(train_ts)
summary(fit_arima_auto)
