#Useful link:
#https://www.r-bloggers.com/2017/11/formal-ways-to-compare-forecasting-models-rolling-windows/
library(ggplot2)
library(gridExtra)


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

# Initialize vectors for forecasts ("_exp") is there 
arima_forecasts_exp <- c()
sarima_forecasts_exp <- c()
ets_forecasts_exp <- c()

# Expanding Window Forecasting with Incremental Updates
for (i in sequence) {
  current_train <- data_ts[(1):(n_train + i - 1)]
  
  current_train <- ts(current_train, start = c(2023, 1), frequency = 24)
  # Fit models on the updated training data
  arima_fit <- arima(current_train, order = c(2, 1, 0))
  sarima_fit <-  arima(current_train,
                       order = c(2, 0, 0),     # Non-seasonal AR(1), MA(1), differencing=0
                       seasonal = list(order = c(2, 1, 0), period = 24))  # Seasonal AR(1), MA(1), differencing=0, S=168
  
  ets_fit <- ets(current_train, model = "ANA")
  # Forecast the next 24 hours
  # Save the forecasts
  arima_forecasts_exp <- c(arima_forecasts_exp, forecast(arima_fit, h = h)$mean)
  sarima_forecasts_exp <- c(sarima_forecasts_exp,forecast(sarima_fit, h = h)$mean)
  ets_forecasts_exp <- c(ets_forecasts_exp,forecast(ets_fit, h = h)$mean)
  
  cat(i/24)
}

#This automatically computes more comparison forecast accuracy measures  
accuracy(arima_forecasts_exp, test_ts)
accuracy(sarima_forecasts_exp, test_ts)
accuracy(ets_forecasts_exp, test_ts)


#Create the plot of the 3 different forecasts
p1 <- ggplot() + 
  geom_line(aes(x = 1: 744, y = test_data$demand), color = "black", size = 1.0) + 
  geom_line(aes(x = 1: 744, y = arima_forecasts_exp), color = "blue", size = 1.2, linetype = "dashed") + 
  labs(title = "ARIMA Expanding Window Forecast", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(24, 144, 264, 384, 504, 600, 744)) +  # Manually specify breaks
  theme_minimal()

p2 <- ggplot() + 
  geom_line(aes(x = 1: 744, y = test_data$demand), color = "black", size = 1.0) + 
  geom_line(aes(x = 1: 744, y = sarima_forecasts_exp), color = "red", size = 1.2, linetype = "dashed") + 
  labs(title = "SARIMA Expanding Window Forecast", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(24, 144, 264, 384, 504, 600, 744)) +  # Manually specify breaks
  theme_minimal()

p3 <- ggplot() + 
  geom_line(aes(x = 1: 744, y = test_data$demand), color = "black", size = 1.0) + 
  geom_line(aes(x = 1: 744, y = ets_forecasts_exp), color = "green", size = 1.2, linetype = "dashed") + 
  labs(title = "ETS Expanding Window Forecast", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(24, 144, 264, 384, 504, 600, 744)) +  # Manually specify breaks
  theme_minimal()

# Arrange the plots in a 3x1 layout
grid.arrange(p1, p2, p3, ncol = 1)


#Redo the analysis for the weekly frequency Sarima separately
sarima_week_forecasts_exp <- c()

# Expanding Window Forecasting with Incremental Updates
for (i in sequence) {
  current_train <- data_ts[(1):(n_train + i - 1)]
  
  current_train <- ts(current_train, start = c(2023, 1), frequency = 24*7)
  # Fit models on the updated tr
  sarima_fit <-  arima(current_train,
                       order = c(2, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                       seasonal = list(order = c(0, 1, 0), period = 168)) 
  
  # Forecast the next 24 hours
  # Save the forecasts
  sarima_week_forecasts_exp <- c(sarima_week_forecasts_exp,forecast(sarima_fit, h = h)$mean)
  
  cat(i/24)
}
