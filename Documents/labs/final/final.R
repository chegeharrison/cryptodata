library(quantmod)
library(readxl)
library(writexl)

# Set start and end dates
start_date <- as.Date("2016-08-15")
end_date <- as.Date("2024-12-31")

# Download historical data for selected cryptocurrencies
symbols <- c("BTC-USD", "LTC-USD", "NMC-USD", "PPC-USD", "FTC-USD", "DOGE-USD")
crypto_list <- lapply(symbols, function(sym) {
  getSymbols(sym, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
})

# Extract closing prices
crypto_prices <- do.call(merge, lapply(crypto_list, Cl))

# Assign column names for clarity
colnames(crypto_prices) <- gsub(".Close", "", colnames(crypto_prices))

# Convert to data frame and add dates
crypto_df <- data.frame(Date = index(crypto_prices), coredata(crypto_prices))

# Get the current working directory
current_dir <- getwd()

# Specify the file path for saving the Excel file in the current directory
file_path <- file.path(current_dir, "cryptodata2024data.xlsx")

# Check if the file already exists in the current working directory
if (!file.exists(file_path)) {
  # If the file does not exist, save the data as an Excel file
  write_xlsx(crypto_df, file_path)
} else {
  message("The file already exists at: ", file_path)
}

# Now, read the Excel file from the current working directory
crypto_df <- read_excel(file_path)

# Save the data as a binary .dat file in the current working directory
save(crypto_df, file = file.path(current_dir, "crypto2024data.dat"))

# Display the structure and first few rows of the data
str(crypto_df)
head(crypto_df)

library(ggplot2)

ggplot(data = crypto_df, aes(x = Date)) +
  geom_line(aes(y = BTC), color = "blue") +
  ggtitle("Time Series Plot of Bitcoin Prices") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()
