library(httr)
library(jsonlite)
library(openxlsx)

# List of cryptocurrencies (IDs) launched before 2016
crypto_ids <- c('bitcoin', 'ethereum', 'ripple', 'litecoin', 'dogecoin', 'dash', 'monero', 'stellar')

# Date range
start_date <- as.numeric(as.POSIXct("2016-08-08", tz="UTC"))
end_date <- as.numeric(as.POSIXct("2024-12-31", tz="UTC"))

# Initialize an empty data frame
all_data <- data.frame(date = as.Date(character()), crypto = character(), price = numeric(), stringsAsFactors = FALSE)

for (crypto_id in crypto_ids) {
  url <- paste0('https://api.coingecko.com/api/v3/coins/', crypto_id, '/market_chart/range?vs_currency=usd&from=', start_date, '&to=', end_date)
  print(paste("Fetching data for:", crypto_id))  # Debugging: Print current cryptocurrency ID
  response <- GET(url)
  
  # Inspect response status and content
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    
    if (!is.null(data$prices) && length(data$prices) > 0) {
      prices <- data$prices
      df <- data.frame(timestamp = prices[, 1], price = prices[, 2])
      df$date <- as.Date(as.POSIXct(df$timestamp / 1000, origin="1970-01-01"))
      df$crypto <- crypto_id
      
      # Append to all_data
      all_data <- rbind(all_data, df[, c("date", "crypto", "price")])
    } else {
      message(paste("No data found for", crypto_id))
    }
  } else {
    message(paste("Failed to fetch data for", crypto_id))
    print(content(response, "text"))  # Print error message for debugging
  }
  
  Sys.sleep(1)  # Delay between requests to avoid hitting rate limits
}

# Save to Excel file if there is data
if (nrow(all_data) > 0) {
  write.xlsx(all_data, "cryptocurrency_data.xlsx", row.names = FALSE)
} else {
  message("No data was collected.")
}
