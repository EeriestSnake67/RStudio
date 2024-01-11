# Load the necessary library
library(readr)

# Specify the path to the file
file_path <- "Data/BAC.csv"

# Read the CSV file
data <- read_csv(file_path)

# View the first few rows of the data
head(data)