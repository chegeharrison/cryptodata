# Load necessary library
library(readxl)

# Read the dataset from Excel
cryptodata2024data <- read_excel("Documents/labs/ftsa/cryptodata2024data.xlsx")

# Save as a .dat file
write.table(cryptodata2024data, "crypto2024data.dat", row.names=FALSE)
