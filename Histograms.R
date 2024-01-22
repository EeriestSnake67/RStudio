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

#____Packages and create one overall data set____________________________________________________________________________________________________________________________________________

std_df <- data.frame(ticker = tickers, std = sapply(tickers, function(tick) sd(bank_stocks[[paste(tick, 'Close', sep = '_')]], na.rm = TRUE)))


# Create the graph for general standard deviation
ggplot(std_df, aes(x = ticker, y = std)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Bank Acronym", y = "Standard Deviation", title = "Riskiest Stocks Overall") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15))

bank_stocks
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

#_____________Overall Standard Deviation___________________________________________________________________________________________________________________________

#Figured out how to get the returns to have the date too because for some reason above it deleted the dates
returns2 <- data.frame(Date = bank_stocks$BAC_Date)

# Calculate returns for each ticker
for (tick in tickers) {
  close_col <- paste(tick, 'Close', sep = '_')
  returns2[[paste(tick, 'Return2', sep = ' ')]] <- c(NA, diff(log(bank_stocks[[close_col]])))
}

# Display the first 3 rows
head(returns2, 3)

returns2$Date <- as.Date(returns2$Date)

# Filter for the year 2008
returns2_2008 <- returns2 %>% filter(format(Date, "%Y") == "2008")

# Calculate standard deviation
std_dev <- sapply(returns2_2008[-1], sd, na.rm = TRUE)

# Create a dataframe for plotting
std_dev_df <- data.frame(Ticker = names(std_dev), StdDev = std_dev)

# Plot
ggplot(std_dev_df, aes(x = reorder(Ticker, StdDev), y = StdDev)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = 'Riskiest Stocks to Invest in 2008', y = 'Standard Deviation', x = '') +
  theme_minimal()

# Filter for the year 2014
returns2_2014 <- returns2 %>% filter(format(Date, "%Y") == "2014")

# Calculate standard deviation
std_dev <- sapply(returns2_2014[-1], sd, na.rm = TRUE)

# Create a dataframe for plotting
std_dev_df <- data.frame(Ticker = names(std_dev), StdDev = std_dev)

# Plot
ggplot(std_dev_df, aes(x = reorder(Ticker, StdDev), y = StdDev)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = 'Riskiest Stocks to Invest in 2014', y = 'Standard Deviation', x = '') +
  theme_minimal()

# Filter for the year 2020
returns2_2020 <- returns2 %>% filter(format(Date, "%Y") == "2020")

# Calculate standard deviation
std_dev <- sapply(returns2_2020[-1], sd, na.rm = TRUE)

# Create a dataframe for plotting
std_dev_df <- data.frame(Ticker = names(std_dev), StdDev = std_dev)

# Plot
ggplot(std_dev_df, aes(x = reorder(Ticker, StdDev), y = StdDev)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = 'Riskiest Stocks to Invest in 2020', y = 'Standard Deviation', x = '') +
  theme_minimal()


#________looked at the riskiest stock in 2008, 2014, 2020____________________________________________________________________________________________________________________________________________-

#moving average
install.packages("zoo")
library(zoo)
BAC3$Date <- as.Date(BAC3$Date)

# Filter for the year 2008
BAC3_2008 <- BAC3 %>% filter(Date >= as.Date('2008-01-01') & Date < as.Date('2008-12-31'))

# Calculate 30 Day Moving Average
BAC3_2008$MA30 <- rollmean(BAC3_2008$Close, 30, fill = NA)

# Plot
ggplot(BAC3_2008, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'BAC Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'Bank Of America Moving Average (2008)', y = 'Close Price', x = '', color = '') +
  theme_minimal()

BAC3$Date <- as.Date(BAC3$Date)

# Filter for the year 2014
BAC3_2014 <- BAC3 %>% filter(Date >= as.Date('2014-01-01') & Date < as.Date('2014-12-31'))

# Calculate 30 Day Moving Average
BAC3_2014$MA30 <- rollmean(BAC3_2014$Close, 30, fill = NA)

# Plot
ggplot(BAC3_2014, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'BAC Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'Bank Of America Moving Average (2014)', y = 'Close Price', x = '', color = '') +
  theme_minimal()

BAC3$Date <- as.Date(BAC3$Date)

# Filter for the year 2020
BAC3_2020 <- BAC3 %>% filter(Date >= as.Date('2020-01-01') & Date < as.Date('2020-12-31'))

# Calculate 30 Day Moving Average
BAC3_2020$MA30 <- rollmean(BAC3_2020$Close, 30, fill = NA)

# Plot
ggplot(BAC3_2020, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'BAC Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'Bank Of America Moving Average (2020)', y = 'Close Price', x = '', color = '') +
  theme_minimal()

C$Date <- as.Date(C$Date)

# Filter for the year 2008
C_2008 <- C %>% filter(Date >= as.Date('2008-01-01') & Date < as.Date('2008-12-31'))

# Calculate 30 Day Moving Average for 2008
C_2008$MA30 <- rollmean(C_2008$Close, 30, fill = NA)

# Plot for 2008
ggplot(C_2008, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'C Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'Citigroup Bank Moving Average (2008)', y = 'Close Price', x = '', color = '') +
  theme_minimal()

C_2014 <- C %>% filter(Date >= as.Date('2014-01-01') & Date < as.Date('2014-12-31'))

# Calculate 30 Day Moving Average for 2014
C_2014$MA30 <- rollmean(C_2014$Close, 30, fill = NA)

# Plot for 2014
ggplot(C_2014, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'C Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'CitiGroup Bank Moving Average (2014)', y = 'Close Price', x = '', color = '') +
  theme_minimal()

C_2020 <- C %>% filter(Date >= as.Date('2020-01-01') & Date < as.Date('2020-12-31'))

# Calculate 30 Day Moving Average for 2020
C_2020$MA30 <- rollmean(C_2020$Close, 30, fill = NA)

# Plot for 2020
ggplot(C_2020, aes(x = Date)) +
  geom_line(aes(y = Close, color = 'C Close Price')) +
  geom_line(aes(y = MA30, color = '30 Day Moving Average')) +
  labs(title = 'CitiGroup Bank Moving Average (2020)', y = 'Close Price', x = '', color = '') +
  theme_minimal()

#__________moving average _________________________________________________________________________________________________________

BAC_data <- data.frame(Date = bank_stocks$BAC_Date, Close = bank_stocks$BAC_Close, Bank = "BAC")
C_data <- data.frame(Date = bank_stocks$C_Date, Close = bank_stocks$C_Close, Bank = "C")
GS_data <- data.frame(Date = bank_stocks$GS_Date, Close = bank_stocks$GS_Close, Bank = "GS")
JPM_data <- data.frame(Date = bank_stocks$JPM_Date, Close = bank_stocks$JPM_Close, Bank = "JPM")
MS_data <- data.frame(Date = bank_stocks$MS_Date, Close = bank_stocks$MS_Close, Bank = "MS")
WFC_data <- data.frame(Date = bank_stocks$WFC_Date, Close = bank_stocks$WFC_Close, Bank = "WFC")

# Combine all the data frames into one
all_data <- rbind(BAC_data, C_data, GS_data, JPM_data, MS_data, WFC_data)

# Plot the data
ggplot(all_data, aes(x = Date, y = Close, color = Bank)) +
  geom_line() +
  labs(title = "Closing Prices") +
  theme(legend.title = element_blank())

#______closing prices trends over time_______________________________________________________________________________________________________________________

# load the dataMaid library
library(dataMaid)
# use makeDataReport with HTML as output
makeDataReport(bank_stocks, output = "html", replace = TRUE)

#________automated EDA_______________________________________________________________________________________________________
mean_BAC_Close <- mean(bank_stocks$BAC_Close, na.rm = TRUE)
mean_C_Close <- mean(bank_stocks$C_Close, na.rm = TRUE)
mean_GS_Close <- mean(bank_stocks$GS_Close, na.rm = TRUE)
mean_JPM_Close <- mean(bank_stocks$JPM_Close, na.rm = TRUE)
mean_MS_Close <- mean(bank_stocks$MS_Close, na.rm = TRUE)
mean_WFC_Close <- mean(bank_stocks$WFC_Close, na.rm = TRUE)

# Print the mean closing prices
print(paste("Mean closing price for BAC: ", mean_BAC_Close))
print(paste("Mean closing price for C: ", mean_C_Close))
print(paste("Mean closing price for GS: ", mean_GS_Close))
print(paste("Mean closing price for JPM: ", mean_JPM_Close))
print(paste("Mean closing price for MS: ", mean_MS_Close))
print(paste("Mean closing price for WFC: ", mean_WFC_Close))


returns3 <- na.omit(returns2)
returns3
returns3$Date <- as.character(returns3$Date)
min_dates <- apply(returns3[-1], 2, function(x) returns3$Date[which.min(x)])
min_dates

min_values <- sapply(names(min_dates), function(x) returns3[returns3$Date == min_dates[x], x])
min_values