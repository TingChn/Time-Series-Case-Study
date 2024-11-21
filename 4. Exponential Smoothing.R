#Link for some explanation
#https://otexts.com/fpp2/holt-winters.html

holt_winters_model <- ets(train_ts, model = "ZZZ")  # "ZZZ" allows automatic selection of error, trend, and seasonal components

# Print the model summary
summary(stlf_model)

# Fit the Holt-Winters model with additive seasonality 
fit_additive <- hw(train_ts, seasonal = "additive")

# Plot the results
autoplot(train_ts) +
  autolayer(fit_additive, series = "HW Additive Forecasts", PI = FALSE) +
  xlab("Time (Hourly)") +
  ylab("Demand (Number of Bikes)") +
  ggtitle("Bike Demand with Additive Holt-Winters Forecast") +
  guides(colour = guide_legend(title = "Forecast"))

#We cannot use multiplactive seasonality because there are 0 values in demand
zero_values <- train_ts[train_ts == 0]
num_zero_values <- length(zero_values)
num_zero_values

