#############################################################################
###### Load Day Ahead Market Data                                       ####
###########################################################################

# Day Ahead Market Prices for the NYISO 2010-2016
# Source: NYISO DAM, http://www.nyiso.com/public/markets_operations/market_data/tcc/index.jsp
# Data from this study posted: 


####################################
###  Setup Enviornment    #########
##################################

### Load Libraries
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scales)
library(ggthemes)

### Create Functions
# Function to add title to the last column
read_csv_filename <- function(filename){
        ret <- read.csv(filename, 
                        skip = 3,
                        col.names = c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "avg_hourly_cost_of_losses", "avg_hourly_cost_of_congestion"),
                        stringsAsFactors = FALSE)
        ret$filename <- filename # Adds title of filename to column
        ret
} ### Custom function to read DAM csv's based on NYISO's format

convertAccounting <- function(accounting) {
        bbracket <- sub(")", "", as.character(accounting), fixed = TRUE)
        dollar_sign <- sub("$", "", as.character(bbracket), fixed = TRUE)
        heart_commas <- sub(',', '', as.character(dollar_sign), fixed = TRUE)
        account_result <- sub("(", "-", as.character(heart_commas), fixed = TRUE)
        account_numeric <- as.numeric(account_result)
        account_numeric
} # convert accounting brackets to negative



####################################
###  Load DAM Price Data  #########
##################################

filenames <- list.files(path = "./DAM", 
                        pattern = "*.csv") #get a list of all day ahead market prices

setwd("./DAM") #change working director to DAM

import_list <- lapply(filenames, read_csv_filename) # convert files in the list to data frames
dam <- do.call("rbind", import_list) # combined the data frames into one
setwd("..") # return to parent direcotry
remove(filenames, import_list) # remove unnecessary files


## Format data
dam$Cost_of_Losses <- convertAccounting(dam$Cost_of_Losses) 
dam$Cost_of_Congestion <- convertAccounting(dam$Cost_of_Congestion)
dam$avg_hourly_cost_of_losses <- convertAccounting(dam$avg_hourly_cost_of_losses)
dam$avg_hourly_cost_of_congestion <- convertAccounting(dam$avg_hourly_cost_of_congestion)

dam$PTID <- as.character(dam$PTID)

## Breakout Month and Year from filename with Regex
dam$month <- sapply(strsplit(dam$filename, split = "_"), "[", 1) #Breakout Month
dam$year <- sapply(strsplit(dam$filename, split = "_"), "[", 2) #Breakout Year
dam$date <- ymd(paste0(dam$year, dam$month, "16"))
dam$month <- month(dam$date) #convert to month, date format
dam$year <- year(dam$date) #convert to year, date format

# summary(dam) # Review
print(summary(dam)) # print summary data
print("Day Ahead Market Data Loaded") # print informing user data is loaded