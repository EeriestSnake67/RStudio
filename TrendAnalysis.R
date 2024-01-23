library(ggplot2)
library(tidyverse)
library(reshape2)
library(dplyr)
# Read the CSV file
BAC <- read_csv("Data/BAC.csv")
C <- read_csv('Data/C.csv')
GS <- read_csv('Data/GS.csv')
JPM <- read_csv('Data/JPM.csv')
MS <- read_csv('Data/MS.csv')
WFC <- read_csv('Data/WFC.csv')

#Creates an advanced stock analysis chart 
getSymbols("BAC")

chartSeries(BAC,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addSMI()
addTRIX()
addCMF()
addCCI()
addADX(n=40)

#Repeat for C
getSymbols("C")

chartSeries(C,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addSMI()
addTRIX()
addCMF()
addCCI()
addADX(n=40)

#Repeat for GS
getSymbols("GS")

chartSeries(GS,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addSMI()
addTRIX()
addCMF()
addCCI()
addADX(n=40)

#Repeat for JPM
getSymbols("JPM")

chartSeries(JPM,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addSMI()
addTRIX()
addCMF()
addCCI()
addADX(n=40)

#Repeat for MS
getSymbols("MS")

chartSeries(MS,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addSMI()
addTRIX()
addCMF()
addCCI()
addADX(n=40)

#Repeat for WFC
getSymbols("WFC")

chartSeries(WFC,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addSMI()
addTRIX()
addCMF()
addCCI()
addADX(n=40)