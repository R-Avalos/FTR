# Exploratory -- Data Sources ###

library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

# setClass("acntngFmt")
# setAs("character", "acntngFmt",
#       function(from) as.numeric(sub("\\)", 
#                                     "", 
#                                     sub("\\(", 
#                                         "-", 
#                                         from, 
#                                         sub(",", 
#                                             "", 
#                                             from, 
#                                             fixed = TRUE), 
#                                         fixed = TRUE),
#                                     fixed = TRUE)
#                                 ))

# Functions
convertCurrency <- function(currency) {
        currency1 <- sub('$', '', as.character(currency), fixed = TRUE)
        currency2 <- as.numeric(gsub('\\,', '', as.character(currency1))) 
        currency2
}

# convertAccounting <- function(accounting) {
#         accounting1 <- sub(")", "", as.character(accounting), fixed = TRUE)
#         accounting2 <- sub("(", "-", as.numeric(accounting1), fixed = TRUE)
#         accounting2
# }


# NYISO
# http://tcc.nyiso.com/tcc/public/view_summary_of_transmission_contracts.do

## Contract ID = TCC (FTR) contract, should be character or factor
## POI = Point of Injection, with associated name and zone, change POI to factor
## POW = Point of Withdrawal
# Start and end date... er... Contract length?
# Ref Cont ID ???

list.files()

### Load Contract Data
Jan2016 <- read_csv("TransmissionContracts_NYISO_example_Jan2016.csv")
Jan2016$`End Date` <- dmy(Jan2016$`End Date`)
Jan2016$`Start Date` <- dmy(Jan2016$`Start Date`)
Jan2016$`Primary Holder` <- as.factor(Jan2016$`Primary Holder`)

Jan2016$`Market Clr Price` <- gsub("N/A", "", Jan2016$`Market Clr Price`)
Jan2016$`Market Clr Price` <- as.numeric(Jan2016$`Market Clr Price`)


Jan2016$days <- Jan2016$`End Date` - Jan2016$`Start Date` + 1 ### Contract Length in days
Jan2016$days_numeric <- as.numeric(Jan2016$days)

Jan2016 #review
summary(Jan2016$`Market Clr Price`)
summary(Jan2016)
sd(Jan2016$`Market Clr Price`)


### Subset Contract Data to Monthly FTRs
Jan2016_monthly_contracts <- Jan2016 %>%
        filter(days <= 31)
        


### Load DAM Price Data

# Function to add title to the last column
read_csv_filename <- function(filename){
        ret <- read.csv(filename, 
                        skip = 3,
                        col.names = c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "avg_hourly_cost_of_losses", "avg_hourly_cost_of_congestion"),
                        stringsAsFactors = FALSE)
        ret$filename <- filename # Adds title of filename to column
        ret
}

filenames <- list.files(path = "./DAM", 
                        pattern = "*.csv") #get a list of all day ahead market prices

setwd("./DAM") #change working director to DAM

import_list <- lapply(filenames, read_csv_filename) # convert files in the list to data frames
combined <- do.call("rbind", import_list) # combine the data frames into one
str(import_list)
setwd("..") # return to parent direcotry

# Jan2016_DAM <- read.csv("January_2016_DAM_Posting.csv", 
#                         skip = 3,
#                         col.names = c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "avg_hourly_cost_of_losses", "avg_hourly_cost_of_congestion"),
#                         stringsAsFactors = FALSE)
# summary(Jan2016_DAM)
# str(Jan2016_DAM)

# Convert Currency to numeric
Jan2016_DAM$Cost_of_Losses <- convertCurrency(Jan2016_DAM$Cost_of_Losses)
Jan2016_DAM$Cost_of_Congestion <- convertCurrency(Jan2016_DAM$Cost_of_Congestion)
Jan2016_DAM$avg_hourly_cost_of_losses <- convertCurrency(Jan2016_DAM$avg_hourly_cost_of_losses)
Jan2016_DAM$avg_hourly_cost_of_congestion <- convertCurrency(Jan2016_DAM$avg_hourly_cost_of_congestion)


## Get Contract payoffs
# POI - POW * MWh
# Join POI monthly totals



## Plots
plot_holderprice <- ggplot(data=Jan2016, 
                           aes(x =`Market Clr Price`,
                               y =`Primary Holder`)
                           ) +
        geom_point(alpha = 0.2)
plot_holderprice

plot_duration <- ggplot(data=Jan2016, 
                           aes(y =`Market Clr Price`,
                               x = days_numeric)
) +
        geom_point(alpha = 0.2) +
        scale_x_continuous(limits = c(0, 1000))
plot_duration


plot(y = Jan2016$`Market Clr Price`, x = Jan2016$`Primary Holder`)
plot(Jan2016$`Market Clr Price`)
