library(lubridate)
library(ggplot2)
library(tseries)
library(zoo)
library(forecast)


load("citibike.RData")

# Inspect the structure of the data
str(citibike)
summary(citibike)

citibike <- as.data.frame(citibike)

 # Plot the hourly demand
# Create a datetime variable
citibike$time <- make_datetime(year = citibike$year, 
                               month = citibike$month, 
                               day = citibike$day, 
                               hour = citibike$hour)

ggplot(citibike, aes(x = time, y = demand)) +
  geom_line(color = "blue") +
  labs(title = "Hourly Demand for Citi Bikes (Jan-May 2023)",
       x = "Time", y = "Demand") +
  theme_minimal()

# Investigate Seasonality

# Aggregate demand by hour
hourly_demand <- aggregate(demand ~ hour, citibike, mean)

# Plot average demand for each hour
ggplot(hourly_demand, aes(x = hour, y = demand)) +
  geom_line(color = "darkorange") +
  labs(title = "Average Hourly Demand", x = "Hour of the Day", y = "Average Demand") +
  theme_minimal()

# Boxplot for hour-of-the-day pattern
ggplot(citibike, aes(x = factor(hour), y = demand)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Demand by Hour of Day", 
       x = "Hour of Day", 
       y = "Demand") +
  theme_minimal()


# Aggregate demand by week
# Calculate the average demand by day of the week (wkday)
weekly_avg <- citibike %>%
  group_by(wkday) %>%
  summarise(avg_demand = mean(demand))

# Plot the average weekly demand
ggplot(weekly_avg, aes(x = factor(wkday), y = avg_demand)) +
  geom_line(group = 1, color = "green") +
  geom_point(color = "green") +
  labs(title = "Average Weekly Demand", x = "Day of Week", y = "Average Demand") +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_minimal()

ggplot(citibike, aes(x = factor(wkday), y = demand)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Demand by Day of Week", 
       x = "Day of Week", 
       y = "Demand") +
  theme_minimal()

# Aggregate demand by month
monthly_demand <- aggregate(demand ~ month, citibike, mean)

# Plot average demand for each month
ggplot(monthly_demand, aes(x = factor(month), y = demand)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average Monthly Demand", x = "Month", y = "Average Demand") +
  theme_minimal()

# Boxplot for monthly pattern
ggplot(citibike, aes(x = factor(month), y = demand)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Demand by Month", 
       x = "Month", 
       y = "Demand") +
  theme_minimal()

# Calculate the average demand by month
monthly_avg <- citibike %>%
  group_by(month) %>%
  summarise(avg_demand = mean(demand))

# Plot the average monthly demand
ggplot(monthly_avg, aes(x = month, y = avg_demand)) +
  geom_line(color = "red") +
  labs(title = "Average Monthly Demand", x = "Month", y = "Average Demand") +
  theme_minimal()

#Stationarity

#What to Look For in original plot:
#Trends: A noticeable increase or decrease in the mean over time suggests non-stationarity.
#Variance Changes: Increasing or decreasing spread of the values over time indicates heteroscedasticity, which violates stationarity.
#Seasonality: Repeated cycles (e.g., daily or weekly patterns) may also indicate non-stationarity.
# Perform ADF test
adf.test(citibike$demand)

#Decomposing the series separates it into trend, seasonal, and residual components. This helps in identifying sources of non-stationarity:
#What to Look For:
#A strong trend component indicates non-stationarity.
#Seasonality patterns may also suggest periodic changes affecting stationarity.

#Let's check if we can fit a clear linear trend to the hourly data 
ggplot(citibike, aes(x = time, y = demand)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Adding regression line
  labs(title = "Hourly Demand for Citi Bikes (Jan-May 2023)",
       x = "Time", y = "Demand") +
  theme_minimal()

#Add the first difference of the demand data
citibike$diff_demand[1] <- 0
citibike$diff_demand[2:3624] <- diff(citibike$demand)

#Try and fit a linear trend to the first difference in demand
ggplot(citibike, aes(x = time, y = diff_demand)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Adding regression line
  labs(title = "First Difference Hourly Demand for Citi Bikes (Jan-May 2023)",
       x = "Time", y = "Demand") +
  theme_minimal()

#Note that now the data looks "more" stationary and there is no clear linear trend 

#Transform the data set into ts series
citibike_ts <- ts(citibike$demand, frequency = 24)  # daily seasonality assumed
diff_citibike_ts <- ts(citibike$diff_demand, frequency = 24)

# Decompose the series
decomposed <- decompose(citibike_ts)
decomposed_diff <- decompose(diff_citibike_ts)

# Plot decomposition
plot(decomposed)
plot(decomposed_diff)

#Lag plots show the relationship between a time series and its lagged values. For a stationary series, the relationships should be consistent:
#What to Look For:
#If points are scattered without a distinct pattern, the series is more likely to be stationary. Clear patterns suggest non-stationarity due to seasonality or trend.
# Lag plot for demand

lag.plot(citibike$demand, lags = 12, do.lines = FALSE)

#Across all lags, the spread of points widens at higher demand levels, especially above 1000. This could be indicative of heteroscedasticity (non-constant variance), which violates stationarity.
#No Random Scattering:
#In a stationary series, lag plots tend to show random scatter with no clear structure, as the values are independent of their lags. Here, the structure persists, suggesting non-stationarity.

#Other features

#Calculate and plot rolling averages and standard deviations to observe changes in the mean or variance over time:
#Extra for stationarity too
#If the rolling mean or standard deviation changes significantly over time, the series is likely non-stationary.
# Compute rolling standard deviation
citibike$rolling_sd <- rollapply(citibike$demand, width = 24, FUN = sd, fill = NA)

# Calculate rolling mean and rolling standard deviation
citibike$rolling_mean <- rollapply(citibike$demand, width = 24, FUN = mean, fill = NA)
citibike$rolling_sd <- rollapply(citibike$demand, width = 24, FUN = sd, fill = NA)

# Plot rolling mean
ggplot(citibike, aes(x = time)) +
  geom_line(aes(y = demand), color = "blue", alpha = 0.5) +
  geom_line(aes(y = rolling_mean), color = "red", size = 1) +
  labs(title = "Hourly Demand with Rolling Mean",
       x = "Time", y = "Demand / Rolling Mean") +
  theme_minimal()

# Plot rolling standard deviation
#Can investigate heteroskedacity too
ggplot(citibike, aes(x = time, y = rolling_sd)) +
  geom_line(color = "red") +
  labs(title = "Rolling Standard Deviation of Demand",
       x = "Time", y = "Standard Deviation") +
  theme_minimal()


#Outliers
boxplot(citibike$demand, main = "Boxplot of Demand", ylab = "Demand", col = "lightblue")

#Presence of Outliers: There are many high-demand outliers above the upper whisker. These demand spikes warrant further investigation to identify their causes (e.g., temporal patterns, events, weather).
#Central Distribution: Most demand is concentrated below ~250 bikes per hour, as suggested by the compact box and whisker length.

Q1 <- quantile(citibike$demand, 0.25)
Q3 <- quantile(citibike$demand, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- citibike$demand[citibike$demand > upper_bound]
length(outliers)  # Count of outliers

citibike$outlier <- citibike$demand > upper_bound

# Plot demand highlighting outliers
ggplot(citibike, aes(x = time, y = demand, color = outlier)) +
  geom_point() +
  labs(title = "Outliers in Citi Bike Demand Over Time", x = "Time", y = "Demand") +
  theme_minimal()


