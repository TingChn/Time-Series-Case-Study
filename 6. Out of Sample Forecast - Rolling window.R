#Useful link:
#https://www.r-bloggers.com/2017/11/formal-ways-to-compare-forecasting-models-rolling-windows/

# Initialize models on the full training dataset initially
initial_arima <- arima(train_ts, order = c(2, 1, 0))
initial_sarima <- arima(train_ts,
                        order = c(2, 0, 0),     # Non-seasonal AR(1), MA(1), differencing=0
                        seasonal = list(order = c(2, 1, 0), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168

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
arima_forecasts_roll <- c()
sarima_forecasts_roll <- c()
ets_forecasts_roll <- c()

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
                       order = c(2, 0, 0),     # Non-seasonal AR(1), MA(1), differencing=0
                       seasonal = list(order = c(2, 1, 0), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168
  
  ets_fit <- ets(current_train, model = "ANA")
  index <- index + 1
  # Forecast the next 24 hours
  # Save the forecasts
  arima_forecasts_roll <- c(arima_forecasts_roll, forecast(arima_fit, h = h)$mean)
  sarima_forecasts_roll <- c(sarima_forecasts_roll,forecast(sarima_fit, h = h)$mean)
  ets_forecasts_roll <- c(ets_forecasts_roll,forecast(ets_fit, h = h)$mean)
  
  cat(i/24)
}


#This automatically computes more comparison measures between the  
accuracy(arima_forecasts_roll, test_ts)
accuracy(sarima_forecasts_roll, test_ts)
accuracy(ets_forecasts_roll, test_ts)

par(mfrow = c(1, 1))
plot(test_ts)
lines( ts(arima_forecasts_roll, start = c(2023, 5), frequency = 24), col= "red")
lines( ts(sarima_forecasts_roll, start = c(2023, 5), frequency = 24), col= "green")
lines( ts(ets_forecasts_roll, start = c(2023, 5), frequency = 24), col= "blue")

par(mfrow = c(1, 1))
plot(test_ts[0:23], type='l')
lines( arima_forecasts_roll[0:23], col= "red")
lines( sarima_forecasts_roll[0:23], col= "green")
lines( ets_forecasts_roll[0:23], col= "blue")
