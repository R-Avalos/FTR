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
                        pattern = "*.csv") #get a list of all day ahead market price files

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

# Duplicate data frames and change names, one for Point of Injection (monthly_dam_poi) and one for Point of Withdrawal (monthly_dam_pow)
# These will be used to join to TCC dataframe
myvars <- c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "month", "year", "date")
monthly_DAM <- dam[myvars]
monthly_DAM_POI <- rename(monthly_DAM, `POI ID` = PTID,
                          PTID_Name_POI = PTID_Name,
                          Cost_of_Losses_POI = Cost_of_Losses,
                          Cost_of_Congestion_POI = Cost_of_Congestion,
                          Total_Hours_POI = Total_Hours,
                          Congested_Hours_POI = Congested_Hours)
monthly_DAM_POW <- rename(monthly_DAM, `POW ID` = PTID,
                          PTID_Name_POW = PTID_Name,
                          Cost_of_Losses_POW = Cost_of_Losses,
                          Cost_of_Congestion_POW = Cost_of_Congestion,
                          Total_Hours_POW = Total_Hours,
                          Congested_Hours_POW = Congested_Hours)

remove(dam)
remove(monthly_DAM)
remove(myvars)

#print(summary(monthly_DAM)) # print summary data
print("Day Ahead Market Data Loaded") # print informing user data is loaded

