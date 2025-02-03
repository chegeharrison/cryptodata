library(readxl)
library(data.table)

# Specify the file name
file_name <- "/home/harrry/Documents/labs/cryptodata2024data.xlsx"

# Import the data from the Excel file
crypto_data <- read_excel(file_name)

# Save the data as "crypto2024data.dat"
fwrite(crypto_data, file = "crypto2024data.dat", sep = ",")

# Load the saved data (read from the .dat file)
crypto_data_loaded <- fread("crypto2024data.dat")

# Display the first few rows of the loaded data
head(crypto_data_loaded)

