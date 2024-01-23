# Load the necessary libraries
library(readr)
library(dplyr)

# Read the CSV file
BAC <- read_csv("Data/BAC.csv")
C <- read_csv('Data/C.csv')
GS <- read_csv('Data/GS.csv')
JPM <- read_csv('Data/JPM.csv')
MS <- read_csv('Data/MS.csv')
WFC <- read_csv('Data/WFC.csv')

# Define the tickers
tickers <- c("BAC", "C", "GS", "JPM", "MS", "WFC")

# Combine the data frames
bank_stocks <- cbind(BAC, C, GS, JPM, MS, WFC)

# Set the column names
colnames(bank_stocks) <- paste(rep(tickers, each = ncol(BAC)), colnames(BAC), sep = "_")

# Establish returns dataframe
returns <- data.frame(matrix(ncol = length(tickers), nrow = nrow(bank_stocks)))
names(returns) <- paste(tickers, 'Return', sep = ' ')
for (tick in tickers) {
  returns[[paste(tick, 'Return', sep = ' ')]] <- c(NA, diff(log(bank_stocks[[paste(tick, 'Close', sep = '_')]])))
}

pairs(returns[2:nrow(returns),])