#############################################################################
###### Load Day Ahead Market Data                                       ####
###########################################################################







####################################
###  Load DAM Price Data  #########
##################################

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


filenames <- list.files(path = "./DAM", 
                        pattern = "*.csv") #get a list of all day ahead market prices

setwd("./DAM") #change working director to DAM

import_list <- lapply(filenames, read_csv_filename) # convert files in the list to data frames
combined_dam <- do.call("rbind", import_list) # combined the data frames into one
setwd("..") # return to parent direcotry
remove(filenames, import_list) # remove unnecessary files
summary(combined_dam)

## Format data
combined_dam$Cost_of_Losses <- convertAccounting(combined_dam$Cost_of_Losses) 
combined_dam$Cost_of_Congestion <- convertAccounting(combined_dam$Cost_of_Congestion)
combined_dam$avg_hourly_cost_of_losses <- convertAccounting(combined_dam$avg_hourly_cost_of_losses)
combined_dam$avg_hourly_cost_of_congestion <- convertAccounting(combined_dam$avg_hourly_cost_of_congestion)

combined_dam$PTID <- as.character(combined_dam$PTID)

## Breakout Month and Year from filename with Regex
combined_dam$month <- sapply(strsplit(combined_dam$filename, split = "_"), "[", 1) #Breakout Month
combined_dam$year <- sapply(strsplit(combined_dam$filename, split = "_"), "[", 2) #Breakout Year
combined_dam$date <- ymd(paste0(combined_dam$year, combined_dam$month, "16"))
combined_dam$month <- month(combined_dam$date) #convert to month, date format
combined_dam$year <- year(combined_dam$date) #convert to year, date format

# summary(combined_dam) # Review

