library(tidyverse)
library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)
library(GGally)
library(readr)
library(tidyr)
library(lubridate)

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

# Filter the columns
close_columns <- bank_stocks[, grepl("_Close", colnames(bank_stocks))]

# Extract the Date column from one of the original data frames (e.g., BAC)
date_column <- BAC$Date

# Add the Date column to the close_columns data frame
close_columns$Date <- as.Date(date_column, format = "%Y-%m-%d")

# Reshape the data for ggplot
close_columns_long <- close_columns %>% 
  pivot_longer(cols = -Date, names_to = "Bank", values_to = "Close") %>%
  mutate(Bank = gsub("_Close", "", Bank))

# Calculate the average closing price for each month
close_columns_long <- close_columns_long %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Bank, Month) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE)) %>%
  ungroup()

# Create the plot
graph2 = close_columns_long %>%
  ggplot(aes(x = reorder(Bank, Avg_Close), y = Avg_Close, fill = Bank)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average Closing Price by Month",
       x = "Bank",
       y = "Average Closing Price ($)",
       caption = "Data covers a 15 year period") +
  transition_time(Month) +
  scale_y_continuous(limits = c(0, max(close_columns_long$Avg_Close) * 1.1)) +
  ease_aes('cubic-in-out') +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "#EEEEEE"),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_color_brewer(palette = "Dark2") +
  geom_text(aes(label = format(Month, "%b %Y"), y = Inf, x = Inf), hjust = 1, vjust = 2, size = 5, colour = "white", check_overlap = TRUE)

# Create the animation
animation <- animate(graph2, fps = 10, duration = 15 * 12 / 2, end_pause = 60, width = 1800, height = 1000, renderer = gifski_renderer())

# Save the animation
gganimate::anim_save("C:/Users/jmjkr/Desktop/MATES-Work/Data Science/Final/Photos/closing_price_anim.gif", animation)