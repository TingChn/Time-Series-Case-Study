# Load necessary libraries
library(forecast)

# Fit the ARIMA models
arima_fit <- auto.arima(train_ts, seasonal = FALSE)
arima_manual <- arima(train_ts, order = c(1, 0, 6))

# Fit the SARIMA models
sarima_fit <- auto.arima(train_ts)
sarima_manual <- arima(train_ts,
                       order = c(1, 0, 1),     # Non-seasonal AR(1), MA(1), differencing=0
                       seasonal = list(order = c(1, 0, 1), period = 24))  # Seasonal AR(1), MA(1), differencing=0

# Fit Exponential Smoothing models
ets_additive <- ets(train_ts, model = "ZZZ")  # Automatically selects the best ETS model
ets_manual <- ets(train_ts, model = "AAA")  # Additive model with seasonality

# Extracting the AIC and BIC values
arima_aic <- AIC(arima_fit)
sarima_aic <- AIC(sarima_fit)
ets_additive_aic <- AIC(ets_additive)

arima_bic <- BIC(arima_fit)
sarima_bic <- BIC(sarima_fit)
ets_additive_bic <- BIC(ets_additive)

# Calculate RMSE for each model
arima_rmse <- sqrt(mean(residuals(arima_fit)^2))
sarima_rmse <- sqrt(mean(residuals(sarima_fit)^2))
ets_additive_rmse <- sqrt(mean(residuals(ets_additive)^2))

# Calculate MAE for each model
arima_mae <- mean(abs(residuals(arima_fit)))
sarima_mae <- mean(abs(residuals(sarima_fit)))
ets_additive_mae <- mean(abs(residuals(ets_additive)))

# Calculate errors for each model
arima_errors <- residuals(arima_fit)
sarima_errors <- residuals(sarima_fit)
ets_additive_errors <- residuals(ets_additive)

# Align lengths (ensure errors are comparable)
arima_errors <- arima_errors[-1]  # Remove first point to match benchmark
sarima_errors <- sarima_errors[-1]
ets_additive_errors <- ets_additive_errors[-1]

# Define the Benchmark (Naïve) Method
naive_forecast <- stats::lag(train_ts, -1)  # Use the last observation as the forecast
naive_forecast <- naive_forecast[-length(naive_forecast)]  # Remove the last NA
benchmark_error <- train_ts[-1] - naive_forecast  # Forecast errors for naive method

# Calculate q_t for each model
calculate_q_t <- function(errors, time_series) {
  n <- length(time_series)
  denominator <- (1 / (n - 1)) * sum(abs(diff(time_series)))
  q_t <- errors / denominator
  return(q_t)
}

arima_q_t <- calculate_q_t(arima_errors, train_ts)
sarima_q_t <- calculate_q_t(sarima_errors, train_ts)
ets_additive_q_t <- calculate_q_t(ets_additive_errors, train_ts)

# Calculate MASE for each model
MASE <- c(abs(mean(arima_q_t, na.rm = TRUE)),
          abs(mean(sarima_q_t, na.rm = TRUE)),
          abs(mean(ets_additive_q_t, na.rm = TRUE)))

# Calculate AIC and BIC for manual models
arima_manual_aic <- AIC(arima_manual)
sarima_manual_aic <- AIC(sarima_manual)
ets_manual_aic <- AIC(ets_manual)

arima_manual_bic <- BIC(arima_manual)
sarima_manual_bic <- BIC(sarima_manual)
ets_manual_bic <- BIC(ets_manual)

# Calculate RMSE for manual models
arima_manual_rmse <- sqrt(mean(residuals(arima_manual)^2))
sarima_manual_rmse <- sqrt(mean(residuals(sarima_manual)^2))
ets_manual_rmse <- sqrt(mean(residuals(ets_manual)^2))

# Calculate MAE for manual models
arima_manual_mae <- mean(abs(residuals(arima_manual)))
sarima_manual_mae <- mean(abs(residuals(sarima_manual)))
ets_manual_mae <- mean(abs(residuals(ets_manual)))

# Calculate errors for manual models
arima_manual_errors <- residuals(arima_manual)
sarima_manual_errors <- residuals(sarima_manual)
ets_manual_errors <- residuals(ets_manual)

# Align lengths (ensure errors are comparable)
arima_manual_errors <- arima_manual_errors[-1]  # Remove first point to match benchmark
sarima_manual_errors <- sarima_manual_errors[-1]
ets_manual_errors <- ets_manual_errors[-1]

# Calculate q_t for manual models
arima_manual_q_t <- calculate_q_t(arima_manual_errors, train_ts)
sarima_manual_q_t <- calculate_q_t(sarima_manual_errors, train_ts)
ets_manual_q_t <- calculate_q_t(ets_manual_errors, train_ts)

# Calculate MASE for manual models
MASE_manual <- c(abs(mean(arima_manual_q_t, na.rm = TRUE)),
                 abs(mean(sarima_manual_q_t, na.rm = TRUE)),
                 abs(mean(ets_manual_q_t, na.rm = TRUE)))

# Create a comparison table for all models
model_comparison_manual <- data.frame(
  Model = c("ARIMA", "SARIMA", "ETS", "ARIMA Manual", "SARIMA Manual", "ETS Manual"),
  AIC = c(arima_aic, sarima_aic, ets_additive_aic, arima_manual_aic, sarima_manual_aic, ets_manual_aic),
  BIC = c(arima_bic, sarima_bic, ets_additive_bic, arima_manual_bic, sarima_manual_bic, ets_manual_bic),
  RMSE = c(arima_rmse, sarima_rmse, ets_additive_rmse, arima_manual_rmse, sarima_manual_rmse, ets_manual_rmse),
  MAE = c(arima_mae, sarima_mae, ets_additive_mae, arima_manual_mae, sarima_manual_mae, ets_manual_mae),
  MASE = c(MASE, MASE_manual)
)

# Display the updated comparison table
print(model_comparison_manual)


# Load necessary libraries
library(forecast)
library(ggplot2)

# Generate in-sample forecasts for each model
arima_in_sample <- fitted(arima_fit)  # In-sample forecast from ARIMA
sarima_in_sample <- fitted(sarima_fit)  # In-sample forecast from SARIMA
ets_in_sample <- fitted(ets_additive)  # In-sample forecast from ETS

# Subset the first 168 observations from the training data and forecasts
train_subset <- window(train_ts, end = time(train_ts)[168])
arima_in_sample_subset <- window(arima_in_sample, end = time(arima_in_sample)[168])
sarima_in_sample_subset <- window(sarima_in_sample, end = time(sarima_in_sample)[168])
ets_in_sample_subset <- window(ets_in_sample, end = time(ets_in_sample)[168])

# Plot the True values and ARIMA in-sample values for the first 168 observations
ggplot() + 
  geom_line(aes(x = 1:168, y = train_subset), color = "black", size = 1.2) + 
  geom_line(aes(x = 1:168, y = arima_in_sample_subset), color = "blue", size = 1.2, linetype = "dashed") + 
  labs(title = "ARIMA In-Sample Fit", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144, 168)) +  # Manually specify breaks
  theme_minimal()

# Plot the True values and SARIMA in-sample values for the first 168 observations
ggplot() + 
  geom_line(aes(x = 1:168, y = train_subset), color = "black", size = 1.2) + 
  geom_line(aes(x = 1:168, y = sarima_in_sample_subset), color = "red", size = 1.2, linetype = "dashed") + 
  labs(title = "SARIMA In-Sample Fit", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144, 168)) +  # Manually specify breaks
  theme_minimal()

# Plot the True values and ETS in-sample values for the first 168 observations
ggplot() + 
  geom_line(aes(x = 1:168, y = train_subset), color = "black", size = 1.2) + 
  geom_line(aes(x = 1:168, y = ets_in_sample_subset), color = "green", size = 1.2, linetype = "dashed") + 
  labs(title = "ETS In-Sample Fit ", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144, 168)) +  # Manually specify breaks
  theme_minimal()


# Manual models 
# Generate in-sample forecasts for each model
arima_in_sample_manual <- fitted(arima_manual)  # In-sample forecast from ARIMA
sarima_in_sample_manual <- fitted(sarima_manual)  # In-sample forecast from SARIMA
ets_in_sample_manual <- fitted(ets_manual)  # In-sample forecast from ETS

# Subset the first 168 observations from the training data and forecasts
train_subset_manual <- window(train_ts, end = time(train_ts)[168])
arima_in_sample_subset_manual <- window(arima_in_sample_manual, end = time(arima_in_sample_manual)[168])
sarima_in_sample_subset_manual <- window(sarima_in_sample_manual, end = time(sarima_in_sample_manual)[168])
ets_in_sample_subset_manual <- window(ets_in_sample_manual, end = time(ets_in_sample_manual)[168])

# Plot the True values and ARIMA in-sample values for the first 168 observations
ggplot() + 
  geom_line(aes(x = 1:168, y = train_subset_manual), color = "black", size = 1.2) + 
  geom_line(aes(x = 1:168, y = arima_in_sample_subset_manual), color = "blue", size = 1.2, linetype = "dashed") + 
  labs(title = "ARIMA In-Sample Fit (Manual)", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144, 168)) +  # Manually specify breaks
  theme_minimal()

# Plot the True values and SARIMA in-sample values for the first 168 observations
ggplot() + 
  geom_line(aes(x = 1:168, y = train_subset_manual), color = "black", size = 1.2) + 
  geom_line(aes(x = 1:168, y = sarima_in_sample_subset_manual), color = "red", size = 1.2, linetype = "dashed") + 
  labs(title = "SARIMA In-Sample Fit (Manual)", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144, 168)) +  # Manually specify breaks
  theme_minimal()

# Plot the True values and ETS in-sample values for the first 168 observations
ggplot() + 
  geom_line(aes(x = 1:168, y = train_subset_manual), color = "black", size = 1.2) + 
  geom_line(aes(x = 1:168, y = ets_in_sample_subset_manual), color = "green", size = 1.2, linetype = "dashed") + 
  labs(title = "ETS In-Sample Fit (Manual)", 
       x = "Time", y = "Values") + 
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144, 168)) +  # Manually specify breaks
  theme_minimal()





