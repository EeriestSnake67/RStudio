# Load the necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(plotly)

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

# Create the candlestick chart BAC
fig_BAC <- plot_ly(x = ~BAC$Date, type="candlestick",
               open = ~BAC$Open, close = ~BAC$Close,
               high = ~BAC$High, low = ~BAC$Low) %>%
  layout(title = "Bank of America Stock Price",
         yaxis = list(title = "BAC Stock"),
         shapes = list(
           list(type = "line", x0 = '2009-01-20', x1 = '2009-01-20', y0 = 0, y1 = 1, yref = "paper", line = list(color = "black")),
           list(type = "line", x0 = '2007-12-01', x1 = '2007-12-01', y0 = 0, y1 = 1, yref = "paper", line = list(color = "black"))
         ),
         annotations = list(
           list(x = '2009-01-25', y = 0.9, text = 'President Obama Took Office', showarrow = FALSE, xref='x', yref='paper', xanchor = 'left'),
           list(x = '2007-11-25', y = 0.05, text = 'Subprime Mortgage Crisis', showarrow = FALSE, xref='x', yref='paper', xanchor = 'right')
         ))

# Display the plot
fig_BAC


# Set the column names
colnames(bank_stocks) <- paste(rep(tickers, each = ncol(C)), colnames(C), sep = "_")

# Create the candlestick chart BAC
fig_C <- plot_ly(x = ~C$Date, type="candlestick",
                   open = ~C$Open, close = ~C$Close,
                   high = ~C$High, low = ~C$Low) %>%
  layout(title = "CitiGroup Stock Price",
         yaxis = list(title = "CitiGroup Stock"),
         shapes = list(
           list(type = "line", x0 = '2009-01-20', x1 = '2009-01-20', y0 = 0, y1 = 1, yref = "paper", line = list(color = "black")),
           list(type = "line", x0 = '2007-12-01', x1 = '2007-12-01', y0 = 0, y1 = 1, yref = "paper", line = list(color = "black"))
         ),
         annotations = list(
           list(x = '2009-01-25', y = 0.9, text = 'President Obama Took Office', showarrow = FALSE, xref='x', yref='paper', xanchor = 'left'),
           list(x = '2007-11-25', y = 0.05, text = 'Subprime Mortgage Crisis', showarrow = FALSE, xref='x', yref='paper', xanchor = 'right')
         ))

# Display the plot
fig_C