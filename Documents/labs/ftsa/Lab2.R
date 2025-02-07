# Load necessary library
library(readxl)
library(ggplot2)
library(scales)
library(gridExtra)
library(tidyr)
library(fBasics) # For summary statistics and skewness/kurtosis
library(kableExtra)
library(tseries)
library(dplyr)
library(forecast)
library(fGarch)

# Read the dataset from Excel
cryptodata2024data <- read_excel("Documents/labs/ftsa/cryptodata2024data.xlsx")

# Save as a .dat file
write.table(cryptodata2024data, "crypto2024data.dat", row.names=FALSE)
# Convert Date column to Date format
cryptodata2024data$Date <- as.Date(cryptodata2024data$Date)

## List of cryptocurrency names (assuming they are all columns except 'Date')
crypto_names <- colnames(cryptodata2024data)[-1]

# Function to calculate log returns
log_return <- function(prices) {
  log(prices[-1] / prices[-length(prices)]) * 100  # Calculate log returns in percentage
}

# Calculate log returns for each cryptocurrency
log_returns_list <- lapply(crypto_names, function(name) {
  log_return(cryptodata2024data[[name]])
})

# Convert list of log returns to a dataframe and add the Date column
log_returns_df <- as.data.frame(log_returns_list)
log_returns_df$Date <- cryptodata2024data$Date[-1]  # Remove the first row of Date for consistency

# Set proper column names for the log returns dataframe
colnames(log_returns_df) <- c(crypto_names, "Date")

# Convert data into long format for ggplot
log_returns_long <- pivot_longer(log_returns_df, cols = -Date, names_to = "Cryptocurrency", values_to = "Log_Return")

# Plot all log returns in one plot
ggplot(log_returns_long, aes(x = Date, y = Log_Return, color = Cryptocurrency)) +
  geom_line() +
  labs(title = "Time Series of Cryptocurrency Log Returns",
       x = "Date", y = "Log Returns (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")

#question 1b
# Compute summary statistics for each cryptocurrency log return
summary_stats <- data.frame(
  Cryptocurrency = crypto_names,
  Mean = sapply(log_returns_list, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(log_returns_list, function(x) sd(x, na.rm = TRUE)),
  Skewness = sapply(log_returns_list, function(x) skewness(x, na.rm = TRUE)),
  Kurtosis = sapply(log_returns_list, function(x) kurtosis(x, na.rm = TRUE)),
  Min = sapply(log_returns_list, function(x) min(x, na.rm = TRUE)),
  Max = sapply(log_returns_list, function(x) max(x, na.rm = TRUE))
)

# Perform t-tests for each cryptocurrency's log return (testing if mean is different from zero)
t_test_results <- data.frame(
  Cryptocurrency = crypto_names,
  t_statistic = sapply(log_returns_list, function(x) t.test(x, mu = 0)$statistic),
  p_value = sapply(log_returns_list, function(x) t.test(x, mu = 0)$p.value)
)

# Combine both summary statistics and t-test results
final_results <- merge(summary_stats, t_test_results, by = "Cryptocurrency")
print(final_results)


#Question1c
# Function to calculate simple returns (daily % change)
simple_return <- function(prices) {
  returns <- diff(prices) / head(prices, -1) * 100  # Compute returns in percentage
  return(returns)
}

# Compute simple returns for each cryptocurrency
simple_returns_list <- lapply(crypto_names, function(name) {
  simple_return(cryptodata2024data[[name]])
})

# Convert list of simple returns to a dataframe and align Date column
simple_returns_df <- as.data.frame(simple_returns_list)
simple_returns_df$Date <- cryptodata2024data$Date[-1]  # Remove first row for consistency

# Set proper column names
colnames(simple_returns_df) <- c(crypto_names, "Date")

# Function to plot histogram with normal distribution overlay
plot_histogram <- function(data, title) {
  mean_val <- mean(data, na.rm = TRUE)
  sd_val <- sd(data, na.rm = TRUE)
  
  ggplot(data.frame(Returns = data), aes(x = Returns)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "blue", alpha = 0.5, color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                  color = "red", linetype = "dashed", size = 1) +
    labs(title = title, x = "Returns (%)", y = "Density") +
    theme_minimal()
}

# Generate histograms for both return types
for (crypto in crypto_names) {
  print(plot_histogram(simple_returns_df[[crypto]], paste("Histogram of Simple Returns -", crypto)))
  print(plot_histogram(log_returns_df[[crypto]], paste("Histogram of Log Returns -", crypto)))
}

###Question 1 part 4
# Assuming 'cryptodata2024data' contains the USD/KES exchange rate as one of the columns
# Let's say we use BTC.USD as a proxy for USD/KES (adjust if you know which one it is)
usd_log_returns <- log_returns_df$BTC.USD  # Replace BTC.USD with the correct cryptocurrency column if necessary

# Step 1: Calculate the mean and standard deviation of the USD/KES log returns
mean_usd <- mean(usd_log_returns, na.rm = TRUE)
sd_usd <- sd(usd_log_returns, na.rm = TRUE)

# Step 2: Generate 4600 random samples from a normal distribution using the calculated mean and SD
simulated_returns <- rnorm(4600, mean = mean_usd, sd = sd_usd)

# Prepare the data for plotting
data_for_plot <- data.frame(
  Returns = c(usd_log_returns, simulated_returns),
  Type = c(rep("Actual USD/KES Log Returns", length(usd_log_returns)),
           rep("Simulated Normal Returns", length(simulated_returns)))
)

# Plot the density
ggplot(data_for_plot, aes(x = Returns, fill = Type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot: Actual vs Simulated Log Returns",
       x = "Log Returns", y = "Density") +
  theme_minimal()

 ###Question 1 part 4 complete
# Step 2: Perform the Jarque-Bera test on the log returns (using USD/KES log returns as an example)
# Replace `usd_log_returns` with the actual log returns for the cryptocurrency of interest
jb_test <- jarque.bera.test(usd_log_returns)

# Step 3: Print the results of the test
print(jb_test)

# Step 4: Interpret the p-value
if (jb_test$p.value < 0.05) {
  print("Reject the null hypothesis: The log returns are not normally distributed.")
} else {
  print("Fail to reject the null hypothesis: The log returns are normally distributed.")
}
##Question 2

# Parameters for the random walk
n <- 1000       # Length of the random walk
delta <- 0.01   # Drift term
sigma_w <- 1    # Standard deviation of the noise

# Generate white noise
w <- rnorm(n, mean = 0, sd = sigma_w)

# Initialize the random walk with drift
x <- numeric(n)
x[1] <- w[1]  # First value is just the noise

# Generate the random walk with drift
for (t in 2:n) {
  x[t] <- x[t-1] + delta + w[t]
}

# Create time variable (t)
time <- 1:n

# Fit the linear regression model: x_t = β * t + w_t
reg_model <- lm(x ~ time)

# Prepare data for plotting the original random walk
df <- data.frame(time = time, x = x, fitted = reg_model$fitted.values)

# Create the original plot
plot1 <- ggplot(df, aes(x = time)) +
  geom_line(aes(y = x), color = "blue", size = 1, alpha = 0.7) + # Random walk with drift
  geom_line(aes(y = fitted), color = "red", size = 1, alpha = 0.7) + # Fitted regression line
  geom_abline(slope = delta, intercept = 0, color = "green", linetype = "dashed", size = 1) + # Mean function µ_t = 0.01t
  labs(title = "Random Walk with Drift and Fitted Regression Line (Original)",
       x = "Time (t)",
       y = "Value (x_t)") +
  theme_minimal() +
  theme(legend.position = "none")

# Repeat the experiment 1 time for the second plot (since the question only needs 2 plots)
set.seed(124)  # Change the seed for a different simulation

# Generate the random walk again for the second plot
w_sim <- rnorm(n, mean = 0, sd = sigma_w)
x_sim <- numeric(n)
x_sim[1] <- w_sim[1]
for (t in 2:n) {
  x_sim[t] <- x_sim[t-1] + delta + w_sim[t]
}

# Fit the linear regression model for the second plot
reg_model_sim <- lm(x_sim ~ time)

# Prepare data for plotting the simulation
df_sim <- data.frame(time = time, x = x_sim, fitted = reg_model_sim$fitted.values)

# Create the second plot for the simulation
plot2 <- ggplot(df_sim, aes(x = time)) +
  geom_line(aes(y = x), color = "blue", size = 1, alpha = 0.7) + # Random walk with drift
  geom_line(aes(y = fitted), color = "red", size = 1, alpha = 0.7) + # Fitted regression line
  geom_abline(slope = delta, intercept = 0, color = "green", linetype = "dashed", size = 1) + # Mean function µ_t = 0.01t
  labs(title = "Random Walk with Drift and Fitted Regression Line (Simulation)",
       x = "Time (t)",
       y = "Value (x_t)") +
  theme_minimal() +
  theme(legend.position = "none")

# Display both plots side by side
grid.arrange(plot1, plot2, ncol = 2)


### Question 3: Fitting ARIMA Model

# Let's assume we are working with a cryptocurrency data column, e.g., BTC-USD log returns
data_series <- log_returns_df$BTC.USD  # Replace with your actual series if different

# Step 1: Check for stationarity - Apply differencing if necessary
diff_series <- diff(data_series)  # Apply differencing to make it stationary (if needed)

# Step 2: Plot ACF and PACF to identify p and q values
par(mfrow = c(1, 2))  # Set the layout for two plots side by side
acf(diff_series, main = "ACF of Differenced Series")
pacf(diff_series, main = "PACF of Differenced Series")

# Step 3: Fit ARIMA model (for example, if ACF cuts off at lag 1 and PACF cuts off at lag 1)
arima_model <- arima(diff_series, order = c(1, 1, 1))  # Adjust (p, d, q) based on ACF/PACF

# Step 4: Print the model summary
summary(arima_model)

# Step 5: Check residuals for white noise (model diagnostics)
checkresiduals(arima_model)

####Question 3 part ii
# Perform ADF test with 8 lags, including intercept and trend
adf_result <- adf.test(diff_series, alternative = "stationary", k = 8)

# Report the p-value
print(adf_result)

# Check the p-value and draw conclusions
if (adf_result$p.value < 0.05) {
  print("The series is stationary (reject the null hypothesis of unit root).")
} else {
  print("The series is not stationary (fail to reject the null hypothesis of unit root).")
}

###Question 3 part ii section 2
# Perform the Ljung-Box test on the residuals of the ARIMA model
ljung_box_test <- Box.test(arima_model$residuals, lag = 20, type = "Ljung-Box")

# Print the Ljung-Box test results
print(ljung_box_test)

###Question 3 part iii
# Assuming you have already calculated log returns (or stock returns) as `log_returns`

# Plot ACF and PACF of the log returns to decide the AR and MA terms
par(mfrow = c(1, 2)) # Split the plotting area into 2 (ACF and PACF side by side)

acf(log_returns_df$BTC.USD, main = "ACF of Log Returns (BTC.USD)")  # Replace BTC.USD with your column name
pacf(log_returns_df$BTC.USD, main = "PACF of Log Returns (BTC.USD)") # Same here

# Fit ARMA(1, 1) model manually
arma_model <- arima(log_returns_df$BTC.USD, order = c(1, 0, 1)) # Adjust the column as needed

# View the model summary
summary(arma_model)

# Fit the ARMA model automatically using auto.arima
auto_arma_model <- auto.arima(log_returns_df$BTC.USD)  # Replace BTC.USD with your actual column

# View the summary of the automatically selected model
summary(auto_arma_model)

# Check the ARMA model order
auto_arma_model$order

####Question 3 part v
# Residuals from the ARMA(0, 0, 2) model (auto.arima)
residuals_arma <- auto_arma_model$residuals

# Perform ARCH test for the first 10 lags
arch_test_10 <- ArchTest(residuals_arma, lags = 10)
print(arch_test_10)

# Perform ARCH test for every lag that is a multiple of 5 up to 40 lags
arch_test_5_40 <- ArchTest(residuals_arma, lags = seq(5, 40, by = 5))
print(arch_test_5_40)




