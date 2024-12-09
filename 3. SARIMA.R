#Create the series in seasonal differences
for (i in 1:3624){
  citibike$diff24_demand[i] <- 0
}
for (i in 25:3624){
  citibike$diff24_demand[i] <- citibike$demand[i] - citibike$demand[i-24]
}

#Look at the plot of the seasonal first difference
ggplot(citibike, aes(x = time, y = diff24_demand)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Adding regression line
  labs(title = "Seasonal First Difference in Hourly Demand for Citi Bikes (Jan-May 2023)",
       x = "Time", y = "Demand") +
  theme_minimal()

#Create a time series object for this series 
diff24_train_ts <- ts(train_data$diff24_demand, start = c(2023,1), frequency = 24)

#Look at the ACF and PACF of the data set
par(mfrow = c(1, 2))
acf(diff24_train_ts, main = "ACF of Change in Demand", lag.max = 75)
pacf(diff24_train_ts, main = "PACF of Change in Demand", lag.max = 75)

#Fit a first step model only including the seasonal component of the SARIMA
sarima_model1 <- arima(train_ts, order = c(0, 0, 0),    
                       seasonal = list(order = c(3, 1, 0), period = 24))  

#Collect the residuals of the first model
residualss = sarima_model1$residuals

#Look at the ACF and PACF of the model 
par(mfrow = c(1, 2))
acf(residualss, main = "ACF of Residuals in M1", lag.max = 24)
pacf(residualss, main = "PACF of Residuals in M1", lag.max = 24)

#Fit a complete ARIMA model
sarima_model2 <- arima(train_ts,
                       order = c(2, 0, 0),     
                       seasonal = list(order = c(3, 1, 0), period = 24)) 
summary(sarima_model2)


#Automatic fitting including seasonal
fit_arima_auto <- auto.arima(train_ts)
summary(fit_arima_auto)

#Create ts objects with different frequencies
train_ts_week = ts(train_data$demand, start = c(2023, 1), frequency = 24*7) #weekly
train_ts_month = ts(train_data$demand, start = c(2023, 1), frequency = 24*7*30) #monthly
  
#Try to fit new SARIMA models to this series
fit_sarima_week = auto.arima(train_ts_week)
fit_sarima_month = auto.arima(train_ts_month)

#Look at the summaries of the new models
summary(fit_sarima_week)
summary(fit_sarima_month)
