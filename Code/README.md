# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Load the USAccDeaths dataset
data("USAccDeaths")
ts_data <- USAccDeaths

# Plot the original time series
plot(ts_data, main="Monthly Accidental Deaths in the US (1973-1978)", ylab="Number of Deaths", xlab="Year")

# Perform the Augmented Dickey-Fuller Test for stationarity
adf_result <- adf.test(ts_data)
print(adf_result)

# If the p-value is significant (p < 0.05), the data is stationary. If not, we may need to difference it.

# Apply differencing to make the data stationary (if needed)
ts_data_diff <- diff(ts_data)
plot(ts_data_diff, main="Differenced Accidental Deaths", ylab="Differenced Deaths", xlab="Year")

# Plot ACF and PACF to identify ARIMA parameters
acf(ts_data_diff, main="ACF of Differenced Data")
pacf(ts_data_diff, main="PACF of Differenced Data")

# Based on the ACF and PACF plots, choose the best p, d, q parameters.

# Fit the ARIMA model
model <- arima(ts_data, order=c(1, 1, 1))

# Display model summary
summary(model)

# Check the residuals to see if the model is good
residuals <- residuals(model)
plot(residuals, main="Residuals of the ARIMA Model", ylab="Residuals", xlab="Time")

# Perform Ljung-Box test for autocorrelation in residuals
Box.test(residuals, lag=20, type="Ljung-Box")

# Forecast the next 12 months
forecasted_values <- forecast(model, h=12)

# Plot the forecast
plot(forecasted_values, main="Forecast of Monthly Accidental Deaths", ylab="Forecasted Deaths", xlab="Year")

