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

# Function to create an OHLC chart for a given bank
create_ohlc_chart <- function(bank_data, ticker) {
  # Create the OHLC chart
  fig <- plot_ly(x = ~bank_data$Date, type="ohlc",
                 open = ~bank_data$Open, close = ~bank_data$Close,
                 high = ~bank_data$High, low = ~bank_data$Low) %>%
    layout(title = paste(ticker, "Stock Price"),
           yaxis = list(title = paste(ticker, "Stock")),
           shapes = list(
             list(type = "line", x0 = '2009-01-20', x1 = '2009-01-20', y0 = 0, y1 = 1, yref = "paper", line = list(color = "black")),
             list(type = "line", x0 = '2007-12-01', x1 = '2007-12-01', y0 = 0, y1 = 1, yref = "paper", line = list(color = "black"))
           ),
           annotations = list(
             list(x = '2009-01-20', y = 0.95, text = 'President Obama Took Office', showarrow = FALSE, xref='x', yref='paper', xanchor = 'left'),
             list(x = '2007-12-01', y = 0.1, text = 'Subprime Mortgage Crisis', showarrow = FALSE, xref='x', yref='paper', xanchor = 'right')
           ))
  
  # Save the plot as a jpeg
  export(fig, file = paste0("C:/Users/jmjkr/Desktop/MATES-Work/Data Science/Final/Photos/OHLC/", ticker, ".jpeg"))
  
  # Display the plot
  return(fig)
}
# Call the function for BAC
fig_BAC <- create_ohlc_chart(BAC, "Bank of America")

fig_BAC

# Call the function for C
fig_C <- create_ohlc_chart(C, "CitiGroup")

fig_C

# Call the function for GS
fig_GS <- create_ohlc_chart(GS, "Goldman Sachs")

fig_GS

# Call the function for JPM
fig_JPM <- create_ohlc_chart(JPM, "JPMorgan Chase")

fig_JPM

# Call the function for MS
fig_MS <- create_ohlc_chart(MS, "Morgan Stanley")

fig_MS

# Call the function for WFC
fig_WFC <- create_ohlc_chart(WFC, "Wells Fargo")

fig_WFC