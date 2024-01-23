#Code For Quantmod Graphs
library(ggplot2)
library(tidyverse)
library(reshape2)
library(dplyr)
library(quantmod)
# Read the CSV file
BAC <- read_csv("Data/BAC.csv")
C <- read_csv('Data/C.csv')
GS <- read_csv('Data/GS.csv')
JPM <- read_csv('Data/JPM.csv')
MS <- read_csv('Data/MS.csv')
WFC <- read_csv('Data/WFC.csv')

getSymbols("BAC")

# Clean the data by removing missing values
BAC_clean <- na.omit(BAC)

# Calculate daily returns
BAC_clean$Returns <- dailyReturn(BAC_clean$BAC.Adjusted)

# Remove outliers using z-score (assuming you have 'zoo' package installed)
library(zoo)
z_scores <- scale(BAC_clean$Returns)
BAC_clean <- BAC_clean[abs(z_scores) < 3, ]

# Check summary statistics of the cleaned data
summary(BAC_clean$Returns)

# Calculate 50-day and 200-day moving averages
BAC_clean$MA_50 <- SMA(BAC_clean$BAC.Adjusted, n = 50)
BAC_clean$MA_200 <- SMA(BAC_clean$BAC.Adjusted, n = 200)

# Calculate historical volatility
BAC_clean$Volatility <- runSD(BAC_clean$Returns, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data = BAC_clean, aes(x = Index)) +
  geom_line(aes(y = BAC.Adjusted), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "BAC Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

# Plot the closing price of BAC
chartSeries(BAC$BAC.Close, type = "line", name = "BAC Closing Price")

# Calculate 20-day, 50-day and 200-day moving averages
BAC$SMA_20 <- SMA(BAC$BAC.Close, n = 20)
BAC$SMA_50 <- SMA(BAC$BAC.Close, n = 50)
BAC$SMA_100 <- SMA(BAC$BAC.Close, n = 100)

# Plot the moving averages
chart_Series(BAC, name = "BAC Moving Averages")
add_TA(BAC$SMA_20, on = 1, col = "blue", lty = 1)
add_TA(BAC$SMA_50, on = 1, col = "red", lty = 1)
add_TA(BAC$SMA_100, on = 1, col = "green", lty = 1)

# Repeat for C
getSymbols("C")

# Clean the data by removing missing values
C_clean <- na.omit(C)

# Calculate daily returns
C_clean$Returns <- dailyReturn(C_clean$C.Adjusted)

# Remove outliers using z-score (assuming you have 'zoo' package installed)
library(zoo)
z_scores <- scale(C_clean$Returns)
C_clean <- C_clean[abs(z_scores) < 3, ]

# Check summary statistics of the cleaned data
summary(C_clean$Returns)

# Calculate 50-day and 200-day moving averages
C_clean$MA_50 <- SMA(C_clean$C.Adjusted, n = 50)
C_clean$MA_200 <- SMA(C_clean$C.Adjusted, n = 200)

# Calculate historical volatility
C_clean$Volatility <- runSD(C_clean$Returns, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data = C_clean, aes(x = Index)) +
  geom_line(aes(y = C.Adjusted), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "C Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

# Plot the closing price of C
chartSeries(C$C.Close, type = "line", name = "C Closing Price")

# Calculate 20-day, 50-day and 200-day moving averages
C$SMA_20 <- SMA(C$C.Close, n = 20)
C$SMA_50 <- SMA(C$C.Close, n = 50)
C$SMA_100 <- SMA(C$C.Close, n = 100)

# Plot the moving averages
chart_Series(C, name = "C Moving Averages")
add_TA(C$SMA_20, on = 1, col = "blue", lty = 1)
add_TA(C$SMA_50, on = 1, col = "red", lty = 1)
add_TA(C$SMA_100, on = 1, col = "green", lty = 1)

# Repeat for GS
getSymbols("GS")

# Clean the data by removing missing values
GS_clean <- na.omit(GS)

# Calculate daily returns
GS_clean$Returns <- dailyReturn(GS_clean$GS.Adjusted)

# Remove outliers using z-score (assuming you have 'zoo' package installed)
library(zoo)
z_scores <- scale(GS_clean$Returns)
GS_clean <- GS_clean[abs(z_scores) < 3, ]

# Check summary statistics of the cleaned data
summary(GS_clean$Returns)

# Calculate 50-day and 200-day moving averages
GS_clean$MA_50 <- SMA(GS_clean$GS.Adjusted, n = 50)
GS_clean$MA_200 <- SMA(GS_clean$GS.Adjusted, n = 200)

# Calculate historical volatility
GS_clean$Volatility <- runSD(GS_clean$Returns, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data = GS_clean, aes(x = Index)) +
  geom_line(aes(y = GS.Adjusted), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "GS Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

# Plot the closing price of GS
chartSeries(GS$GS.Close, type = "line", name = "GS Closing Price")

# Calculate 20-day, 50-day and 200-day moving averages
GS$SMA_20 <- SMA(GS$GS.Close, n = 20)
GS$SMA_50 <- SMA(GS$GS.Close, n = 50)
GS$SMA_100 <- SMA(GS$GS.Close, n = 100)

# Plot the moving averages
chart_Series(GS, name = "GS Moving Averages")
add_TA(GS$SMA_20, on = 1, col = "blue", lty = 1)
add_TA(GS$SMA_50, on = 1, col = "red", lty = 1)
add_TA(GS$SMA_100, on = 1, col = "green", lty = 1)

# Repeat for JPM
getSymbols("JPM")

# Clean the data by removing missing values
JPM_clean <- na.omit(JPM)

# Calculate daily returns
JPM_clean$Returns <- dailyReturn(JPM_clean$JPM.Adjusted)

# Remove outliers using z-score (assuming you have 'zoo' package installed)
library(zoo)
z_scores <- scale(JPM_clean$Returns)
JPM_clean <- JPM_clean[abs(z_scores) < 3, ]

# Check summary statistics of the cleaned data
summary(JPM_clean$Returns)

# Calculate 50-day and 200-day moving averages
JPM_clean$MA_50 <- SMA(JPM_clean$JPM.Adjusted, n = 50)
JPM_clean$MA_200 <- SMA(JPM_clean$JPM.Adjusted, n = 200)

# Calculate historical volatility
JPM_clean$Volatility <- runSD(JPM_clean$Returns, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data = JPM_clean, aes(x = Index)) +
  geom_line(aes(y = JPM.Adjusted), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "JPM Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

# Plot the closing price of JPM
chartSeries(JPM$JPM.Close, type = "line", name = "JPM Closing Price")

# Calculate 20-day, 50-day and 200-day moving averages
JPM$SMA_20 <- SMA(JPM$JPM.Close, n = 20)
JPM$SMA_50 <- SMA(JPM$JPM.Close, n = 50)
JPM$SMA_100 <- SMA(JPM$JPM.Close, n = 100)

# Plot the moving averages
chart_Series(JPM, name = "JPM Moving Averages")
add_TA(JPM$SMA_20, on = 1, col = "blue", lty = 1)
add_TA(JPM$SMA_50, on = 1, col = "red", lty = 1)
add_TA(JPM$SMA_100, on = 1, col = "green", lty = 1)

# Repeat for MS
getSymbols("MS")

# Clean the data by removing missing values
MS_clean <- na.omit(MS)

# Calculate daily returns
MS_clean$Returns <- dailyReturn(MS_clean$MS.Adjusted)

# Remove outliers using z-score (assuming you have 'zoo' package installed)
library(zoo)
z_scores <- scale(MS_clean$Returns)
MS_clean <- MS_clean[abs(z_scores) < 3, ]

# Check summary statistics of the cleaned data
summary(MS_clean$Returns)

# Calculate 50-day and 200-day moving averages
MS_clean$MA_50 <- SMA(MS_clean$MS.Adjusted, n = 50)
MS_clean$MA_200 <- SMA(MS_clean$MS.Adjusted, n = 200)

# Calculate historical volatility
MS_clean$Volatility <- runSD(MS_clean$Returns, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data = MS_clean, aes(x = Index)) +
  geom_line(aes(y = MS.Adjusted), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "MS Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

# Plot the closing price of MS
chartSeries(MS$MS.Close, type = "line", name = "MS Closing Price")

# Calculate 20-day, 50-day and 200-day moving averages
MS$SMA_20 <- SMA(MS$MS.Close, n = 20)
MS$SMA_50 <- SMA(MS$MS.Close, n = 50)
MS$SMA_100 <- SMA(MS$MS.Close, n = 100)

# Plot the moving averages
chart_Series(MS, name = "MS Moving Averages")
add_TA(MS$SMA_20, on = 1, col = "blue", lty = 1)
add_TA(MS$SMA_50, on = 1, col = "red", lty = 1)
add_TA(MS$SMA_100, on = 1, col = "green", lty = 1)

# Repeat for WFC
getSymbols("WFC")

# Clean the data by removing missing values
WFC_clean <- na.omit(WFC)

# Calculate daily returns
WFC_clean$Returns <- dailyReturn(WFC_clean$WFC.Adjusted)

# Remove outliers using z-score (assuming you have 'zoo' package installed)
library(zoo)
z_scores <- scale(WFC_clean$Returns)
WFC_clean <- WFC_clean[abs(z_scores) < 3, ]

# Check summary statistics of the cleaned data
summary(WFC_clean$Returns)

# Calculate 50-day and 200-day moving averages
WFC_clean$MA_50 <- SMA(WFC_clean$WFC.Adjusted, n = 50)
WFC_clean$MA_200 <- SMA(WFC_clean$WFC.Adjusted, n = 200)

# Calculate historical volatility
WFC_clean$Volatility <- runSD(WFC_clean$Returns, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data = WFC_clean, aes(x = Index)) +
  geom_line(aes(y = WFC.Adjusted), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "WFC Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

# Plot the closing price of WFC
chartSeries(WFC$WFC.Close, type = "line", name = "WFC Closing Price")

# Calculate 20-day, 50-day and 200-day moving averages
WFC$SMA_20 <- SMA(WFC$WFC.Close, n = 20)
WFC$SMA_50 <- SMA(WFC$WFC.Close, n = 50)
WFC$SMA_100 <- SMA(WFC$WFC.Close, n = 100)

# Plot the moving averages
chart_Series(WFC, name = "WFC Moving Averages")
add_TA(WFC$SMA_20, on = 1, col = "blue", lty = 1)
add_TA(WFC$SMA_50, on = 1, col = "red", lty = 1)
add_TA(WFC$SMA_100, on = 1, col = "green", lty = 1)
