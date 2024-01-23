library(ggplot2)
library(reshape2)
library(dplyr)
# Read the CSV file
BAC <- read_csv("Data/BAC.csv")
C <- read_csv('Data/C.csv')
GS <- read_csv('Data/GS.csv')
JPM <- read_csv('Data/JPM.csv')
MS <- read_csv('Data/MS.csv')
WFC <- read_csv('Data/WFC.csv')

# Assuming BAC, C, GS, JPM, MS, WFC are data frames with the same columns
bank_stocks <- bind_cols(BAC = BAC, C = C, GS = GS, JPM = JPM, MS = MS, WFC = WFC)

# Assuming bank_stocks is a data frame with 'Close', 'Bank Ticker' and other columns
# First, we melt the data frame to long format
bank_stocks_melt <- melt(bank_stocks, variable.name = "Stock Info", value.name = "Close")

# Then, we plot it using ggplot2
ggplot(bank_stocks_melt, aes(x = "Stock Info", y = "Close")) +
  geom_area() +
  facet_wrap(~ `Bank Ticker`, ncol = 2) +
  theme_minimal()




