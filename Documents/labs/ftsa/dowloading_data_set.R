library(quantmod)
library(ggplot2)
library(readxl)
library(dplyr)
library(gridExtra)

#QUestion 1

# Set start and end dates
start_date <- as.Date("2016-08-15")
end_date <- as.Date("2024-12-31")

# Download historical data for selected cryptocurrencies
symbols <- c("BTC-USD", "LTC-USD", "NMC-USD", "PPC-USD", "FTC-USD", "DOGE-USD")
crypto_list <- lapply(symbols, function(sym) {
  getSymbols(sym, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
})

# Extract closing prices and assign column names
crypto_prices <- do.call(merge, lapply(crypto_list, Cl))
colnames(crypto_prices) <- gsub(".Close", "", colnames(crypto_prices))


# Convert to data frame and add Date column
crypto_df <- data.frame(Date = index(crypto_prices), coredata(crypto_prices))

# Save the data as "crypto2024data.dat" (only after confirming the data structure)
save(crypto_df, file = "crypto2024data.dat")