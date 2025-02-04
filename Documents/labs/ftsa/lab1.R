# Load necessary library
library(readxl)
library(ggplot2)
library(scales)
library(gridExtra)
library(tidyr)
library(fBasics) # For summary statistics and skewness/kurtosis
library(kableExtra)

# Read the dataset from Excel
cryptodata2024data <- read_excel("Documents/labs/ftsa/cryptodata2024data.xlsx")

# Save as a .dat file
write.table(cryptodata2024data, "crypto2024data.dat", row.names=FALSE)
# Convert Date column to Date format
cryptodata2024data$Date <- as.Date(cryptodata2024data$Date)

## List of cryptocurrency names (assuming they are all columns except 'Date')
crypto_names <- colnames(cryptodata2024data)[-1]

# Function to plot time series for each cryptocurrency
plot_crypto <- function(data, crypto_name) {
  ggplot(data, aes(x = Date, y = .data[[crypto_name]])) +
    geom_line(color = "blue") +
    labs(
      title = paste("Time Series of", crypto_name, "Exchange Rate"),
      x = "Date",
      y = paste(crypto_name, "Price (in USD)")
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar)
}

# Create a list of plots
crypto_plots <- lapply(crypto_names, function(name) plot_crypto(cryptodata2024data, name))

# Arrange the plots in a grid
grid.arrange(grobs = crypto_plots, ncol = 2)

##question 4
crypto_long <- pivot_longer(cryptodata2024data, cols = -Date, names_to = "Cryptocurrency", values_to = "Price")

# Plot all cryptocurrencies in one plot
ggplot(crypto_long, aes(x = Date, y = Price, color = Cryptocurrency)) +
  geom_line() +
  labs(title = "Time Series of Cryptocurrency Exchange Rates",
       x = "Date", y = "Price (in USD)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "bottom")

# question 5
#Extract cryptocurrency prices and exclude the Date column
crypto_prices <- cryptodata2024data[-1]

# Calculate simple returns
simple_returns <- crypto_prices[-1, ] / crypto_prices[-nrow(crypto_prices), ] - 1
simple_returns <- simple_returns * 100  # Convert to percentages

# Assign the adjusted Date column to simple_returns
simple_returns <- as.data.frame(simple_returns)
simple_returns$Date <- cryptodata2024data$Date[-1]  # Remove the first row of Date

# Compute summary statistics for the simple returns
summary_stats <- basicStats(simple_returns[-ncol(simple_returns)])  # Exclude Date column

# Print the summary statistics
print(summary_stats)


kable(summary_stats) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
###Question 6
# Calculate log returns
log_returns <- log(crypto_prices[-1, ] / crypto_prices[-nrow(crypto_prices), ]) * 100

# Convert to a data frame and assign the Date column
log_returns <- as.data.frame(log_returns)
log_returns$Date <- cryptodata2024data$Date[-1]  # Remove the first row of Date

# Compute summary statistics for the log returns
log_summary_stats <- basicStats(log_returns[-ncol(log_returns)])  # Exclude Date column

# Print results
print(log_summary_stats)

#question 7
# Perform one-sample t-tests for each cryptocurrency log return
t_test_results <- data.frame(
  Cryptocurrency = colnames(log_returns)[-ncol(log_returns)],  # Exclude 'Date'
  t_statistic = NA,
  p_value = NA
)

for (i in 1:(ncol(log_returns) - 1)) {  # Loop through each cryptocurrency
  test_result <- t.test(log_returns[[i]], mu = 0, alternative = "two.sided")
  t_test_results$t_statistic[i] <- test_result$statistic
  t_test_results$p_value[i] <- test_result$p.value
}

# Print the results
print(t_test_results)

#Question 8
# Function to plot histogram with normal curve
plot_histogram <- function(data, crypto_name, return_type) {
  mean_val <- mean(data[[crypto_name]], na.rm = TRUE)
  sd_val <- sd(data[[crypto_name]], na.rm = TRUE)
  
  ggplot(data, aes(x = .data[[crypto_name]])) +
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", size = 1) +
    labs(title = paste("Histogram of", return_type, "for", crypto_name),
         x = paste(crypto_name, return_type),
         y = "Density") +
    theme_minimal()
}

# Plot histograms for Simple Returns
simple_return_plots <- lapply(colnames(simple_returns)[-ncol(simple_returns)], 
                              function(name) plot_histogram(simple_returns, name, "Simple Returns"))

# Plot histograms for Log Returns
log_return_plots <- lapply(colnames(log_returns)[-ncol(log_returns)], 
                           function(name) plot_histogram(log_returns, name, "Log Returns"))

# Display the plots
grid.arrange(grobs = simple_return_plots, ncol = 2)
grid.arrange(grobs = log_return_plots, ncol = 2)


###questions 9
# Extract Bitcoin log returns (use the correct column name)
btc <- as.numeric(log_returns$`BTC.USD`)  # Ensure it's numeric

# Check if btc contains valid numeric values
if (all(is.na(btc))) {
  stop("Error: BTC log return column contains only NA values. Check your data.")
}

# Compute mean and standard deviation
btc_mean <- mean(btc, na.rm = TRUE)
btc_sd <- sd(btc, na.rm = TRUE)

# Generate 5000 random samples from a normal distribution with the same mean and standard deviation
simulated_returns <- rnorm(5000, mean = btc_mean, sd = btc_sd)

# Convert to data frames for plotting
btc_df <- data.frame(Returns = btc, Type = "BTC Log Returns")
sim_df <- data.frame(Returns = simulated_returns, Type = "Simulated Normal Returns")

# Combine datasets
combined_df <- rbind(btc_df, sim_df)

# Plot density of BTC log returns vs. simulated normal returns
ggplot(combined_df, aes(x = Returns, fill = Type, color = Type)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Density Comparison: BTC Log Returns vs. Normal Distribution",
       x = "Returns",
       y = "Density") +
  theme(legend.title = element_blank())

