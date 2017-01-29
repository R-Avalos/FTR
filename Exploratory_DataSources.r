# Exploratory -- Data Sources ###

library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

rm(list = ls()) #Clear Workspace

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
        
### Add Summer and winter Ttrue Fales
# Summer vs winter periods: http://www.nyiso.com/public/markets_operations-services-customer_support-faq-index.jsp
# SUmmer May 1st - Oct 31st
# Winter Nov 1st - April 30th

Jan2016_monthly_contracts$winter_month <- ifelse(test = (month(Jan2016_monthly_contracts$`Start Date`) > month(mdy("10/31/2016")) & Jan2016_monthly_contracts$days_numeric <= 31) | (month(Jan2016_monthly_contracts$`End Date`)< month(mdy("5/1/2016")) & Jan2016_monthly_contracts$days_numeric <= 31), 
                                           yes = TRUE, 
                                           no = FALSE) # If monthly contract, and winter, set to TRUE

summary(Jan2016_monthly_contracts$winter_month) # quick check
summary(Jan2016_monthly_contracts)

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
combined <- do.call("rbind", import_list) # combined the data frames into one
str(import_list)
setwd("..") # return to parent direcotry

Jan2016_DAM <- read.csv("./DAM/January_2016_DAM_Posting.csv",
                        skip = 3,
                        col.names = c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "avg_hourly_cost_of_losses", "avg_hourly_cost_of_congestion"),
                        stringsAsFactors = FALSE)
# summary(Jan2016_DAM)
# str(Jan2016_DAM)

## Convert Currency to numeric
combined$Cost_of_Losses <- convertCurrency(combined$Cost_of_Losses)
combined$Cost_of_Congestion <- convertCurrency(combined$Cost_of_Congestion)
combined$avg_hourly_cost_of_losses <- convertCurrency(combined$avg_hourly_cost_of_losses)
combined$avg_hourly_cost_of_congestion <- convertCurrency(combined$avg_hourly_cost_of_congestion)

## Breakout Month and Year from filename with Regex
combined$month <- sapply(strsplit(combined$filename, split = "_"), "[", 1) #Breakout Month
combined$year <- sapply(strsplit(combined$filename, split = "_"), "[", 2) #Breakout Year


## Get Contract payoffs
# POI - POW * MWh
# Join POI monthly totals
myvars <- c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "month", "year")
x <- combined[myvars]
x <- rename(x, `POI ID` = PTID, 
            PTID_Name_POI = PTID_Name, 
            Cost_of_Losses_POI = Cost_of_Losses,
            Cost_of_Congestion_POI = Cost_of_Congestion,
            Total_Hours_POI = Total_Hours,
            Congested_Hours_POI = Congested_Hours,
            month_POI = month,
            year_POI = year)
x2 <- combined[myvars]
x2 <- rename(x2, `POW ID` = PTID, 
            PTID_Name_POW = PTID_Name, 
            Cost_of_Losses_POW = Cost_of_Losses,
            Cost_of_Congestion_POW = Cost_of_Congestion,
            Total_Hours_POW = Total_Hours,
            Congested_Hours_POW = Congested_Hours,
            month_POW = month,
            year_POW = year)

Jan2016_monthly_contracts$PTID <- Jan2016_monthly_contracts$`POI ID`


### Merge... need to add DAM for Each Month and merge according to month
###

First_merge <- inner_join(x = Jan2016_monthly_contracts, y = x, by = "POI ID")
Second_merge <- inner_join(x = First_merge, y = x2, by = "POW ID")

####  Revenue
Second_merge$revenue <- ifelse(test = Second_merge$winter_month==T, yes = Second_merge$`MW Winter`, no = Second_merge$`MW Summer`) * (-1*Second_merge$Cost_of_Congestion_POW)-(-1*Second_merge$Cost_of_Congestion_POI) # If winter, use winter MWh, else use Summer MWh, then multiply by congestion between points to get rent

Second_merge$profit <- Second_merge$revenue - Second_merge$`Market Clr Price`

summary(Second_merge$revenue)
plot(Second_merge$revenue)
summary(Second_merge$profit)
plot(Second_merge$`End Date`, Second_merge$profit)

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
