install.packages("tidyverse")
library(tidyverse)
install.packages(
  c("arrow", "babynames", "curl", "duckdb", "gapminder", 
    "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
    "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
    "repurrrsive", "tidymodels", "writexl")
)
library(ggrepel)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)

#Combine the banks to one dataframe
BAC <- read_csv("Data/BAC.csv")
C <- read_csv('Data/C.csv')
GS <- read_csv('Data/GS.csv')
JPM <- read_csv('Data/JPM.csv')
MS <- read_csv('Data/MS.csv')
WFC <- read_csv('Data/WFC.csv')
tickers <- c("BAC", "C", "GS", "JPM", "MS", "WFC")
bank_stocks <- cbind(BAC, C, GS, JPM, MS, WFC)
colnames(bank_stocks) <- paste(rep(tickers, each = ncol(BAC)), colnames(BAC), sep = "_")
bank_stocks

BAC3 <- read_csv("BAC.csv")
BAC3

std_df <- data.frame(ticker = tickers, std = sapply(tickers, function(tick) sd(bank_stocks[[paste(tick, 'Close', sep = '_')]], na.rm = TRUE)))

# Create the graph for general standard deviation
ggplot(std_df, aes(x = ticker, y = std)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Bank Acronym", y = "Standard Deviation", title = "General Standard Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15))

#trying to figure out the returns
returns <- data.frame(matrix(ncol = length(tickers), nrow = nrow(bank_stocks)))
names(returns) <- paste(tickers, 'Return', sep = ' ')
for (tick in tickers) {
  returns[[paste(tick, 'Return', sep = ' ')]] <- c(NA, diff(log(bank_stocks[[paste(tick, 'Close', sep = '_')]])))
}
head(returns, 3)

# Calculate standard deviation for each bank
std_df <- data.frame(ticker = colnames(returns), std = apply(returns, 2, sd, na.rm = TRUE))

# Create the graph for returns stand dev (which stocks are the riskiest)
ggplot(std_df, aes(x = ticker, y = std)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Bank Acronym", y = "Standard Deviation", title = "Returns Standard Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15))

#data set goes from 2006-2020 (looked at the riskiest stock in 2006 compared to 2020)

BAC
#moving average
install.packages("zoo")
library(zoo)
BAC3$Date <- as.Date(BAC3$Date)

# Filter for the year 2008
BAC3_2008 <- BAC3 %>% filter(Date >= as.Date('2008-01-01') & Date <= as.Date('2008-12-31'))

# Calculate 30 Day Moving Average
BAC3_2008$MA30 <- rollmean(BAC3_2008$Close, 30, fill = NA)

# Plot
ggplot(BAC3_2008, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'BAC Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'Bank Of America Moving Average', y = 'Close Price', x = '', color = '') +
  theme_minimal()