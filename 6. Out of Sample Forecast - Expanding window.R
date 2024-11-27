#Useful link:
#https://www.r-bloggers.com/2017/11/formal-ways-to-compare-forecasting-models-rolling-windows/

# Initialize models on the full training dataset initially
initial_arima <- arima(train_ts, order = c(2, 1, 0))
initial_sarima <- arima(train_ts,
                        order = c(1, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                        seasonal = list(order = c(1, 0, 1), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168

initial_ets <- ets(train_ts, model = "ANA")

data_ts <- ts(citibike$demand, start = c(2023, 1), frequency = 24)

# Filter the test data (after April 2023)
test_data <- citibike[(row_position+1):nrow(citibike), ]

# Create the test time series object
test_ts <- ts(test_data$demand, start = c(2023, 5), frequency = 24)

# Length of training data and test data
n_train <- length(train_ts)
n_test <- length(test_ts)

sequence <- seq(24, n_test, by = 24)
h <- 24

# Initialize vectors for forecasts
arima_forecasts <- c()
sarima_forecasts <- c()
ets_forecasts <- c()

index <- 0
# Rolling Window Forecasting with Incremental Updates
for (i in sequence) {
  # Define the current training window using integer indices
  current_train <- data_ts[(i):(n_train + i - 1)]
  # To change into expanding widow 
  #current_train <- data_ts[(1):(n_train + i - 1)]
  
  current_train <- ts(current_train, start = c(2023, 1 + index), frequency = 24)
  # Fit models on the updated training data
  arima_fit <- arima(current_train, order = c(2, 1, 0))
  sarima_fit <-  arima(current_train,
                       order = c(1, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                       seasonal = list(order = c(1, 0, 1), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168
  
  ets_fit <- ets(current_train, model = "ANA")
  index <- index + 1
  # Forecast the next 24 hours
  # Save the forecasts
  arima_forecasts <- c(arima_forecasts, forecast(arima_fit, h = h)$mean)
  sarima_forecasts <- c(sarima_forecasts,forecast(sarima_fit, h = h)$mean)
  ets_forecasts <- c(ets_forecasts,forecast(ets_fit, h = h)$mean)
}


# Calculate Error Metrics (Should still consider more + ones in paper)
# Delete last observation of test_ts as for 24 hour forecasts there is one observation extra (should check if this is accurate) 
arima_rmse <- sqrt(mean((arima_forecasts - test_ts)^2))
sarima_rmse <- sqrt(mean((sarima_forecasts - test_ts)^2))
ets_rmse <- sqrt(mean((ets_forecasts - test_ts)^2))

# Print RMSE results
cat("ARIMA RMSE: ", arima_rmse, "\n")
cat("SARIMA RMSE: ", sarima_rmse, "\n")
cat("ETS RMSE: ", ets_rmse, "\n")

