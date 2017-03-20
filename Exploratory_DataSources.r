# Exploratory -- Data Sources ###

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scales)
library(ggthemes)

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

###############################
### Load Contract Data  ######
#############################

# Source all awards: http://tcc.nyiso.com/tcc/public/view_awards_summary.do
# Source: http://tcc.nyiso.com/tcc/public/view_summary_of_transmission_contracts.do
# Manually removed footers from csv

contract_filenames <- list.files( pattern = "*.csv") #get a list of all contract files
contract_import_list <- lapply(contract_filenames, read_csv) # convert files in the list to data frames
combined_TCC <- do.call("rbind", contract_import_list) # combined the data frames into one
remove(contract_filenames, contract_import_list) #remove unncessary files

# Quick Review
combined_TCC
summary(combined_TCC)

# Transform
combined_TCC$`End Date` <- dmy(combined_TCC$`End Date`)
combined_TCC$`Start Date` <- dmy(combined_TCC$`Start Date`)
combined_TCC$`Primary Holder` <- as.factor(combined_TCC$`Primary Holder`)

combined_TCC$`Market Clr Price` <- gsub("N/A", "", combined_TCC$`Market Clr Price`)
combined_TCC$`Market Clr Price` <- as.numeric(combined_TCC$`Market Clr Price`)

combined_TCC$days <- combined_TCC$`End Date` - combined_TCC$`Start Date` + 1 ### Contract Length in days
combined_TCC$days_numeric <- as.numeric(combined_TCC$days)

combined_TCC #review
summary(combined_TCC$`Market Clr Price`)
summary(combined_TCC)


### Subset Contract Data to Monthly FTRs
combined_TCC_monthly_contracts <- combined_TCC %>%
        filter(days <= 31)
combined_TCC_monthly_contracts$month <- month(combined_TCC_monthly_contracts$`Start Date`)
combined_TCC_monthly_contracts$year <- year(combined_TCC_monthly_contracts$`Start Date`)
combined_TCC_monthly_contracts$`POI ID` <- as.numeric(combined_TCC_monthly_contracts$`POI ID`)
combined_TCC_monthly_contracts$`POW ID` <- as.numeric(combined_TCC_monthly_contracts$`POW ID`)


### Add Summer and winter Ttrue Fales
# Summer vs winter periods: http://www.nyiso.com/public/markets_operations-services-customer_support-faq-index.jsp
# SUmmer May 1st - Oct 31st
# Winter Nov 1st - April 30th

combined_TCC_monthly_contracts$winter_month <- ifelse(test = (month(combined_TCC_monthly_contracts$`Start Date`) > month(mdy("10/31/2016")) & combined_TCC_monthly_contracts$days_numeric <= 31) | (month(combined_TCC_monthly_contracts$`End Date`)< month(mdy("5/1/2016")) & combined_TCC_monthly_contracts$days_numeric <= 31), 
                                           yes = TRUE, 
                                           no = FALSE) # If monthly contract, and winter, set to TRUE

summary(combined_TCC_monthly_contracts$winter_month) # quick check
summary(combined_TCC_monthly_contracts)


x <- ggplot(combined_TCC_monthly_contracts, aes(x = `Market Clr Price`, y = `Primary Holder`)) +
        geom_point(alpha = 0.1)
x


####################################
###  Load DAM Price Data  #########
##################################

# 

# Function to add title to the last column
read_csv_filename <- function(filename){
        ret <- read.csv(filename, 
                        skip = 3,
                        col.names = c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "avg_hourly_cost_of_losses", "avg_hourly_cost_of_congestion"),
                        stringsAsFactors = FALSE)
        ret$filename <- filename # Adds title of filename to column
        ret
} ### Custom function to read DAM csv's based on NYISO's format

filenames <- list.files(path = "./DAM", 
                        pattern = "*.csv") #get a list of all day ahead market prices

setwd("./DAM") #change working director to DAM

import_list <- lapply(filenames, read_csv_filename) # convert files in the list to data frames
combined_dam <- do.call("rbind", import_list) # combined the data frames into one
setwd("..") # return to parent direcotry
remove(filenames, import_list) # remove unnecessary files
summary(combined_dam)

## Format data
combined_dam$Cost_of_Losses <- convertCurrency(combined_dam$Cost_of_Losses)
combined_dam$Cost_of_Congestion <- convertCurrency(combined_dam$Cost_of_Congestion)
combined_dam$avg_hourly_cost_of_losses <- convertCurrency(combined_dam$avg_hourly_cost_of_losses)
combined_dam$avg_hourly_cost_of_congestion <- convertCurrency(combined_dam$avg_hourly_cost_of_congestion)

## Breakout Month and Year from filename with Regex
combined_dam$month <- sapply(strsplit(combined_dam$filename, split = "_"), "[", 1) #Breakout Month
combined_dam$year <- sapply(strsplit(combined_dam$filename, split = "_"), "[", 2) #Breakout Year
combined_dam$date <- ymd(paste0(combined_dam$year, combined_dam$month, "16"))
combined_dam$month <- month(combined_dam$date) #convert to month, date format
combined_dam$year <- year(combined_dam$date) #convert to year, date format

summary(combined_dam) # Review
plot(combined_dam$date, combined_dam$avg_hourly_cost_of_congestion)

## Get Contract payoffs
# POI - POW * MWh
# Join POI monthly totals
myvars <- c("PTID", "PTID_Name", "Cost_of_Losses", "Cost_of_Congestion", "Total_Hours", "Congested_Hours", "month", "year", "date")
monthly_DAM <- combined_dam[myvars]
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


### Merge... need to add DAM for Each Month and merge according to month
test <- inner_join(x = combined_TCC_monthly_contracts, y = monthly_DAM_POI, by = c("POI ID", "year", "month"))
summary(test)

test2 <- inner_join(x = test, y = monthly_DAM_POW, by = c("POW ID", "year", "month")) 


####  Revenue
test2$revenue <- ifelse(test = test2$winter_month==T, yes = test2$`MW Winter`, no = test2$`MW Summer`) * (-1*test2$Cost_of_Congestion_POW)-(-1*test2$Cost_of_Congestion_POI) # If winter, use winter MWh, else use Summer MWh, then multiply by congestion between points to get rent

test2$profit <- test2$revenue - test2$`Market Clr Price`

summary(test2$revenue)
plot(test2$revenue)
summary(test2$profit)
plot(test2$`End Date`, test2$profit)


#### Percent Return
# Return(i) = [Profit(i) / absolute|MarketClearingPrice(i)|] * 100%

test2$abs_mktprice <- abs(test2$`Market Clr Price`)
# Need to save account for $0.00 market clearing price
# Change 0.00 to 0.001
test2$abs_mktprice[test2$abs_mktprice == 0.00] <- 0.01 
summary(test2$abs_mktprice)

test2$return <- (test2$profit/test2$abs_mktprice)*100
summary(test2$return)
plot(test2$`End Date`, test2$return) # quick view

## Plots
plot_profit <- ggplot(test2, 
                      aes(x = date.x, y = profit, color = winter_month)
                      ) + 
        geom_jitter(alpha = 0.2, width = 1) +
        geom_rangeframe(sides = "l", alpha = 0.2) +
        geom_hline(yintercept = 0, alpha = 0.2) +
        scale_y_continuous(labels = comma) +
        scale_color_manual(values = c("black", "dodger blue")) +
        ggtitle("NYISO Monthly TCC Profits \n") +
        theme_tufte()
ggMarginal(plot_profit, type = "density", margins = "y", color = "light grey") # Add histogram on for profit

plot_holderprice <- ggplot(data=Jan2016, 
                           aes(x =`Market Clr Price`,
                               y =`Primary Holder`)
                           ) +
        geom_point(alpha = 0.2)
plot_holderprice

plot_holderreturn <- ggplot(data = test2, 
                           aes(x = return,
                               y =`Primary Holder`)
) +
        geom_point(alpha = 0.2)
plot_holderreturn



plot_duration <- ggplot(data=Jan2016, 
                           aes(y =`Market Clr Price`,
                               x = days_numeric)
) +
        geom_point(alpha = 0.2) +
        scale_x_continuous(limits = c(0, 1000))
plot_holderprice


plot(y = Jan2016$`Market Clr Price`, x = Jan2016$`Primary Holder`)
plot(Jan2016$`Market Clr Price`)
