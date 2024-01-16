install.packages("tidyverse")
library(tidyverse)
install.packages(
  c("arrow", "babynames", "curl", "duckdb", "gapminder", 
    "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
    "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
    "repurrrsive", "tidymodels", "writexl")
)
library(ggrepel)
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
mean <- mean(bank_stocks$y)
sd <- sd(bank_stocks$y)
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2)