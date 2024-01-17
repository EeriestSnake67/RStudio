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
open_columns <- bank_stocks[, grepl("_Open", colnames(bank_stocks))]

# Extract the Date column from one of the original data frames (e.g., BAC)
date_column <- BAC$Date

# Add the Date column to the open_columns data frame
open_columns$Date <- as.Date(date_column, format = "%Y-%m-%d")

# Reshape the data for ggplot
open_columns_long <- open_columns %>% 
  pivot_longer(cols = -Date, names_to = "Bank", values_to = "Open") %>%
  mutate(Bank = gsub("_Open", "", Bank))

# Create the plot
graph2 = open_columns_long %>%
  ggplot(aes(x=Date, y=Open, color=Bank)) +
  geom_line(size = 2, alpha = 0.75) +
  theme_solarized_2(light = FALSE) +
  labs(title = "Bank Stock Prices",
       x = "Timeline",  # Add this line
       y = "Opening Price ($)") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Create the animation
graph2.animation = graph2 +
  transition_reveal(Date) + 
  view_follow(fixed_y = TRUE)

animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100, renderer = gifski_renderer())