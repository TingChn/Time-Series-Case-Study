#For now I have taken the automatic fittings only

# Fit the ARIMA model
arima_fit <- auto.arima(train_ts , seasonal = FALSE)

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


#From the suggested paper
#https://www.sciencedirect.com/science/article/pii/S0169207006000239

# Required Libraries
library(forecast)

# Step 1: Define the Benchmark (Naïve) Method
naive_forecast <- stats::lag(train_ts, -1)  # Use the last observation as the forecast
naive_forecast <- naive_forecast[-length(naive_forecast)]  # Remove the last NA
benchmark_error <- train_ts[-1] - naive_forecast  # Forecast errors for naive method

# Step 2: Calculate Forecast Errors for Each Model
# Calculate residuals (errors) for each model
arima_errors <- residuals(arima_fit)
sarima_errors <- residuals(sarima_fit)
ets_additive_errors <- residuals(ets_additive)

# Align lengths (ensure errors are comparable)
arima_errors <- arima_errors[-1]  # Remove first point to match benchmark
sarima_errors <- sarima_errors[-1]
ets_additive_errors <- ets_additive_errors[-1]

# Step 3: Compute Relative Errors
# Compute relative errors
arima_relative_error <- arima_errors / benchmark_error
sarima_relative_error <- sarima_errors / benchmark_error
ets_additive_relative_error <- ets_additive_errors / benchmark_error

# Winsorize (optional) to handle extreme values in benchmark_error
winsorize <- function(x, cutoff = 0.05) {
  quantiles <- quantile(x, probs = c(cutoff, 1 - cutoff), na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

arima_relative_error <- winsorize(arima_relative_error)
sarima_relative_error <- winsorize(sarima_relative_error)
ets_additive_relative_error <- winsorize(ets_additive_relative_error)

# Step 4: Compute Relative Measures
# Relative Absolute Errors
arima_rae <- abs(arima_errors / benchmark_error)
sarima_rae <- abs(sarima_errors / benchmark_error)
ets_additive_rae <- abs(ets_additive_errors / benchmark_error)

# GMRAE (Geometric Mean of Relative Absolute Errors)
arima_gmrae <- exp(mean(log(arima_rae), na.rm = TRUE))
sarima_gmrae <- exp(mean(log(sarima_rae), na.rm = TRUE))
ets_additive_gmrae <- exp(mean(log(ets_additive_rae), na.rm = TRUE))

# MdRAE (Median of Relative Absolute Errors)
arima_mdrae <- median(arima_rae, na.rm = TRUE)
sarima_mdrae <- median(sarima_rae, na.rm = TRUE)
ets_additive_mdrae <- median(ets_additive_rae, na.rm = TRUE)

# MRAE (Median of Relative Absolute Errors)
arima_relmae <- mean(arima_rae, na.rm = TRUE)
sarima_relmae <- mean(sarima_rae, na.rm = TRUE)
ets_additive_relmae <- mean(ets_additive_rae, na.rm = TRUE)

# Step 5: Compute Scaled Errors (MASE)
# MAE of the benchmark method
benchmark_mae <- mean(abs(benchmark_error), na.rm = TRUE)

# Scaled Errors
arima_scaled_error <- abs(arima_errors) / benchmark_mae
sarima_scaled_error <- abs(sarima_errors) / benchmark_mae
ets_additive_scaled_error <- abs(ets_additive_errors) / benchmark_mae

#MASE (Mean Absolute Scaled Error)
arima_mase <- mean(arima_scaled_error, na.rm = TRUE)
sarima_mase <- mean(sarima_scaled_error, na.rm = TRUE)
ets_additive_mase <- mean(ets_additive_scaled_error, na.rm = TRUE)

# Step 6: Summarize the Comparison
# Compile results into a data frame
model_comparison <- data.frame(
  Model = c("ARIMA", "SARIMA", "ETS Additive"),
  GMRAE = c(arima_gmrae, sarima_gmrae, ets_additive_gmrae),
  MdRAE = c(arima_mdrae, sarima_mdrae, ets_additive_mdrae),
  RelMAE = c(arima_relmae, sarima_relmae, ets_additive_relmae),
  MASE = c(arima_mase, sarima_mase, ets_additive_mase)
)

# Print the comparison table
print(model_comparison)

#I left out scale and percentage as no different datasets are used for this analysis



