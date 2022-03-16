# Krystian Gombosi
# Written for the Mystic River Watershed Association (MyWRA)
# August, 2021
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(lubridate)
library(pander)
library(xtable)
library(gridExtra)
library(ggthemes)
library(ggpubr)
library(googlesheets4)
library(readr)

rm(list = ls())

# ---------------------------------- Functions ------------------------------ #

# Plotter function 
grapher <- function(X,Y) {
  
  
  g <- ggplot(X, aes(x = Date, y = Phycocyanin, color = "grey"),
              na.omit = TRUE) + 
    geom_point() + 
    theme_linedraw() + 
    geom_line(data = Y, aes(color = "black"))
  
  g <- g + facet_grid(rows = vars(Type),cols = vars(Waterbody),scales = "free_y")
  g <- g +
    labs(title = "Phycocyanin Concentration by Date",
         subtitle = "Mystic River Watershed, 2021",
         x = "Date", y = "Phycocyanin Concentration (ug/L)") +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_color_manual(name = "Legend",values = c("grey" = "grey", 
                                                   "black" = "black"),
                                                   labels = c("Triplicate Data", "Moving Average"))
  
  return(g)
}

# ------------------------------ Script -------------------------------------- #

spreadsheet <-"https://docs.google.com/spreadsheets/d/1FSjgMYN5-GDqfu3OjithLuKcal-Cp4DK4tj7dFQqGt0/edit#gid=729913531"
allData <- read_sheet(spreadsheet, sheet = "MyRWA Algae Data 2021")
                   
# Saves and labels the columns of pertinent data for visualization. 
# Note: if this script is used in the future, make sure to either copy the 2021 
# data sheet on Google Sheets, or update the column selection1s to your new data 
# set. 
allData <- allData[,c(1,3,8,44,46,47)]
colnames(allData) <- (c("Date", "Waterbody", "Type", "Genus", 
                  "Phycocyanin", "Phycoethyrene"))

allData$Waterbody <- as.character(allData$Waterbody)
allData$Type <- as.character(allData$Type)
allData$Genus <- as.character(allData$Genus)
allData$Date <- as.Date(allData$Date)

# Sample names were input incorrectly in the 2021 data sheet. Lines 55-57 
# correct this mistake by correctly naming each sample. 

allData$Type <- gsub("NET", "BFC", allData$Type)
allData$Type <- gsub("50UM", "Pico", allData$Type)
allData$Type <- gsub("IT", "WLW", allData$Type)

# Upcoming section was added because we had errors in our sampling procedure 
# that caused us to question the validity of our data. Should probably be 
# commented out if this script is to be used in the future for data 
# visualization.

allPC <- allData %>%
  select(Date, Waterbody,Type,Genus,Phycocyanin,Phycoethyrene) %>%
  filter(Date >= as.Date("2021-07-26"))

averagePC <- allPC %>%
  select(Date,Waterbody,Type,Genus,Phycocyanin,Phycoethyrene) %>%
  group_by(Date,Waterbody,Type,Genus) %>%
  summarise(Phycocyanin = mean(Phycocyanin, n = n()))

plot <- grapher(allPC,averagePC)
plot

# ---------------------- Microcystin Systems ----------------------------------#

# Takes in pre-averaged datasets
MC_dominant <- subset(averagePC, Genus == "Microcystis")
D_dominant <- anti_join(averagePC,MC_dominant)

m <- 1.148 # Uses newest regression coefficients from Nancy Leland's research
b <- 1.341

m1 <- 0.536
b1 <- 0.49

MC_dominant$Estimated_Microcystin <- m*log10(MC_dominant$Phycocyanin) + b
MC_dominant$Estimated_Microcystin <- (10^(MC_dominant$Estimated_Microcystin))/1000 
D_dominant$Estimated_Microcystin <- m1*log10(D_dominant$Phycocyanin) + b1
D_dominant$Estimated_Microcystin <- (10^(D_dominant$Estimated_Microcystin))/1000 
# Calculates estimated MC concentration in ug/L

MC_total <- rbind(MC_dominant,D_dominant)

total_WLW <- subset(MC_total, Type == "WLW")

MC_plot <- ggplot(total_WLW, aes(Date, Estimated_Microcystin,colour = Genus)) + 
  geom_point() + 
  geom_line() +
  facet_grid(rows = vars(Waterbody), scales = "free_y") + 
  theme_linedraw() +
  scale_colour_calc() +
  labs(x = "Date", y = "Estimated Microsystin Toxin Concentration (ug/L)", 
       title = "Microcystin Concentration by Date, Whole Lake Water", 
       subtitle = "Recreational Advisory = 14 ug/L") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 
          
MC_plot


