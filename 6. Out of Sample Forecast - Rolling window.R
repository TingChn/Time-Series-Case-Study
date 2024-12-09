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



#Create the plot of the 3 different forecasts
p_1 <- ggplot() + 
  geom_line(aes(x = 1: 744, y = test_data$demand), color = "black", size = 1.0) + 
  geom_line(aes(x = 1: 744, y = arima_forecasts_roll), color = "blue", size = 1.2, linetype = "dashed") + 
  labs(title = "ARIMA Rolling Window Forecast", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(24, 144, 264, 384, 504, 600, 744)) +  # Manually specify breaks
  theme_minimal()

p_2 <- ggplot() + 
  geom_line(aes(x = 1: 744, y = test_data$demand), color = "black", size = 1.0) + 
  geom_line(aes(x = 1: 744, y = sarima_forecasts_roll), color = "red", size = 1.2, linetype = "dashed") + 
  labs(title = "SARIMA Rolling Window Forecast", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(24, 144, 264, 384, 504, 600, 744)) +  # Manually specify breaks
  theme_minimal()

p_3 <- ggplot() + 
  geom_line(aes(x = 1: 744, y = test_data$demand), color = "black", size = 1.0) + 
  geom_line(aes(x = 1: 744, y = ets_forecasts_roll), color = "green", size = 1.2, linetype = "dashed") + 
  labs(title = "ETS Rolling Window Forecast", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(24, 144, 264, 384, 504, 600, 744)) +  # Manually specify breaks
  theme_minimal()

# Arrange the plots in a 3x1 layout
grid.arrange(p_1, p_2, p_3, ncol = 1)


#Redo the analysis for the weekly frequency Sarima separately
sarima_week_forecasts_roll <- c()

# Rolling Window Forecasting with Incremental Updates
for (i in sequence) {
  current_train <- data_ts[(i):(n_train + i - 1)]
  
  current_train <- ts(current_train, start = c(2023, 1), frequency = 24*7)
  # Fit models on the updated tr
  sarima_fit <-  arima(current_train,
                       order = c(2, 0, 1),     
                       seasonal = list(order = c(0, 1, 0), period = 168)) 
  
  # Forecast the next 24 hours
  # Save the forecasts
  sarima_week_forecasts_roll <- c(sarima_week_forecasts_roll,forecast(sarima_fit, h = h)$mean)
  
}
