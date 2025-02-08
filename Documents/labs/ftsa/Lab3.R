# Load necessary libraries
library(readxl)
library(ggplot2)
library(scales)
library(tidyr)
library(forecast)
library(tseries)
library(dplyr)
library(fGarch)
library(FinTS)

# Read the dataset
cryptodata <- read_excel("Documents/labs/ftsa/cryptodata2024data.xlsx")
# Convert Date column to Date format
cryptodata$Date <- as.Date(cryptodata$Date)

# List of cryptocurrency names (excluding 'Date')
crypto_names <- colnames(cryptodata)[-1]

# Function to calculate log returns
log_return <- function(prices) {
  log(prices[-1] / prices[-length(prices)]) * 100  # Log returns in percentage
}

# Calculate log returns for each cryptocurrency
log_returns_list <- lapply(crypto_names, function(name) log_return(cryptodata[[name]]))

# Convert list of log returns to dataframe
log_returns_df <- as.data.frame(log_returns_list)
log_returns_df$Date <- cryptodata$Date[-1]  

# Set proper column names
colnames(log_returns_df) <- c(crypto_names, "Date")


# Step 1: Check for stationarity - Apply differencing if necessary
btc_log_returns <- log_returns_df$BTC.USD  # Adjust this to the relevant column
diff_series <- diff(btc_log_returns)  # Differencing to make it stationary

# Step 2: ADF test to check stationarity
adf_result <- adf.test(diff_series, alternative = "stationary", k = 8)
print(adf_result)

# Step 3: Fit ARIMA model (adjust order based on ACF/PACF)
arima_model <- arima(diff_series, order = c(1, 1, 1))  
summary(arima_model)

# Step 4: Residual diagnostics
checkresiduals(arima_model)

# Step 5: Perform the Ljung-Box test on residuals
ljung_box_test <- Box.test(arima_model$residuals, lag = 20, type = "Ljung-Box")
print(ljung_box_test)

# Step 6: Test for ARCH effects in residuals
ArchTest(arima_model$residuals, lags = 10)

#############################################################
#####Question 1
# Select one cryptocurrency (e.g., BTC.USD)
# Select another cryptocurrency (LTC.USD instead of BTC.USD)
ltc_log_returns <- log_returns_df$LTC.USD  # Replace with another crypto if needed

# Fix margin issue before plotting
par(mfrow = c(1, 2))  # Set up 2 plots in 1 row
par(mar = c(4, 4, 2, 1))  # Adjust margins to prevent "figure margins too large" error

# Step 1: Plot ACF and PACF
acf(ltc_log_returns, main = "ACF of LTC Log Returns")  
pacf(ltc_log_returns, main = "PACF of LTC Log Returns")  

# Step 2: Ljung-Box Test for serial correlation
ljung_box_results <- Box.test(ltc_log_returns, lag = 20, type = "Ljung-Box")

# Print test results
print(ljung_box_results)

# Step 3: Interpretation
if (ljung_box_results$p.value < 0.05) {
  print("Significant serial correlation detected (reject null hypothesis).")
} else {
  print("No significant serial correlation (fail to reject null hypothesis).")
}


###trial
# Fit ARIMA model on LTC returns (you can adjust the order based on ACF/PACF)
ppc_log_returns <- log_returns_df$PPC.USD  # Replace with another crypto if needed
arima_model_ppc <- arima(ppc_log_returns, order = c(1, 0, 1))  # Example ARIMA model

# Step 1: Plot ACF and PACF of raw returns
par(mfrow = c(1, 2))  # Set up 2 plots in 1 row
par(mar = c(4, 4, 2, 1))  # Adjust margins to prevent "figure margins too large" error
acf(ppc_log_returns, main = "ACF of LTC Log Returns")  
pacf(ppc_log_returns, main = "PACF of LTC Log Returns")  

# Step 2: Ljung-Box Test on residuals of the ARIMA model for serial correlation
ljung_box_residuals <- Box.test(arima_model_ppc$residuals, lag = 20, type = "Ljung-Box")

# Print test results for residuals
print(ljung_box_residuals)

# Step 3: Interpretation of Ljung-Box Test on residuals
if (ljung_box_residuals$p.value < 0.05) {
  print("Significant serial correlation detected (reject null hypothesis).")
} else {
  print("No significant serial correlation (fail to reject null hypothesis).")
}
 ##Queston 2
# Square the log returns
btc_squared_returns <- btc_log_returns^2  

# Ljung-Box test on squared returns
ljung_box_arch <- Box.test(btc_squared_returns, lag = 20, type = "Ljung-Box")
print(ljung_box_arch)


# Perform ARCH test (lags = 10 for short-term volatility effects)
arch_test_result <- ArchTest(btc_log_returns, lags = 10)

# Print result
print(arch_test_result)

###question 3  ANOVA Analysis on Log Returns
log_returns_long <- gather(log_returns_df, key = "Crypto", value = "LogReturn", -Date)

# Perform ANOVA
anova_result <- aov(LogReturn ~ Crypto, data = log_returns_long)
summary(anova_result)  # Display ANOVA table

### Question 4
#Perform Jarque-Bera test on BTC log returns
jarque_bera_test <- jarque.bera.test(btc_log_returns)

# Print the result
print(jarque_bera_test)

# Perform Shapiro-Wilk test on BTC log returns
shapiro_wilk_test <- shapiro.test(btc_log_returns)

# Print the result
print(shapiro_wilk_test)

###Question 5
aarima_model <- arima(btc_log_returns, order = c(1, 1, 1))  # Example order

# Extract Information Criterion Statistics
AIC(arima_model)  # Akaike Information Criterion
BIC(arima_model)  # Bayesian Information Criterion



# Fit a GARCH(1,1) model
garch_model <- garchFit(~ garch(1,1), data = btc_log_returns, trace = FALSE)

# Print the model summary to check AIC and BIC
summary(garch_model)
