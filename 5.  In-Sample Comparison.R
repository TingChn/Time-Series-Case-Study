# For now I have taken the automatic fittings only

# Fit the ARIMA model
arima_fit <- auto.arima(train_ts, seasonal = FALSE)

# Fit the SARIMA model
sarima_fit <- auto.arima(train_ts)

# Fit Exponential Smoothing models
ets_additive <- ets(train_ts, model = "ZZZ")  # Automatically selects the best ETS model

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

# Create a comparison table
model_comparison <- data.frame(
  Model = c("ARIMA", "SARIMA", "ETS Additive"),
  AIC = c(arima_aic, sarima_aic, ets_additive_aic),
  BIC = c(arima_bic, sarima_bic, ets_additive_bic),
  RMSE = c(arima_rmse, sarima_rmse, ets_additive_rmse),
  MAE = c(arima_mae, sarima_mae, ets_additive_mae)
)

# Display the comparison table
print(model_comparison)

# Define the Benchmark (Naïve) Method
naive_forecast <- stats::lag(train_ts, -1)  # Use the last observation as the forecast
naive_forecast <- naive_forecast[-length(naive_forecast)]  # Remove the last NA
benchmark_error <- train_ts[-1] - naive_forecast  # Forecast errors for naive method

# Calculate residuals (errors) for each model
arima_errors <- residuals(arima_fit)
sarima_errors <- residuals(sarima_fit)
ets_additive_errors <- residuals(ets_additive)

# Align lengths (ensure errors are comparable)
arima_errors <- arima_errors[-1]  # Remove first point to match benchmark
sarima_errors <- sarima_errors[-1]
ets_additive_errors <- ets_additive_errors[-1]

# Step 7: Calculate q_t for each model
calculate_q_t <- function(errors, time_series) {
  n <- length(time_series)
  denominator <- (1 / (n - 1)) * sum(abs(diff(time_series)))
  q_t <- errors / denominator
  return(q_t)
}

arima_q_t <- calculate_q_t(arima_errors, train_ts)
sarima_q_t <- calculate_q_t(sarima_errors, train_ts)
ets_additive_q_t <- calculate_q_t(ets_additive_errors, train_ts)

# Add MASE to the comparison table
MASE <- c(abs(mean(arima_q_t, na.rm = TRUE)),
              abs(mean(sarima_q_t, na.rm = TRUE)),
                  abs(mean(ets_additive_q_t, na.rm = TRUE)))

model_comparison$MASE <- MASE

# Print the updated comparison table
print(model_comparison)




