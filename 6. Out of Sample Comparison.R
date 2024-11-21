#Useful link:
#https://www.r-bloggers.com/2017/11/formal-ways-to-compare-forecasting-models-rolling-windows/

# Initialize models on the full training dataset initially
initial_arima <- arima(train_ts, order = c(2, 1, 0))
initial_sarima <- arima(train_ts,
                        order = c(1, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                        seasonal = list(order = c(1, 0, 1), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168

initial_ets <- ets(train_ts, model = "ANA")

# Length of training data and test data
n_train <- length(train_ts)
n_test <- length(test_ts)

# Initialize vectors for forecasts
arima_forecasts <- numeric(n_test)
sarima_forecasts <- numeric(n_test)
ets_forecasts <- numeric(n_test)

# Rolling Window Forecasting with Incremental Updates
for (i in 1:n_test) {
  # Define the current training window using integer indices
  current_train <- train_ts[(i):(n_train + i - 1)]
  
  # Fit models on the updated training data
  arima_fit <- arima(current_train, order = c(2, 1, 0))
  sarima_fit <-  arima(current_train,
                       order = c(1, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                       seasonal = list(order = c(1, 0, 1), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168
  
  ets_fit <- ets(current_train, model = "ANA")
  
  # Forecast the next 24 hours
  arima_forecasts[i] <- forecast(arima_fit, h = h)$mean[h]
  sarima_forecasts[i] <- forecast(sarima_fit, h = h)$mean[h]
  ets_forecasts[i] <- forecast(ets_fit, h = h)$mean[h]
}

# Calculate Error Metrics
arima_rmse <- sqrt(mean((arima_forecasts - test_ts)^2))
sarima_rmse <- sqrt(mean((sarima_forecasts - test_ts)^2))
ets_rmse <- sqrt(mean((ets_forecasts - test_ts)^2))

# Print RMSE results
cat("ARIMA RMSE: ", arima_rmse, "\n")
cat("SARIMA RMSE: ", sarima_rmse, "\n")
cat("ETS RMSE: ", ets_rmse, "\n")
