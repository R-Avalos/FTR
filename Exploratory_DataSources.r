# Exploratory -- Data Sources ###

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scales)
library(ggthemes)

rm(list = ls()) #Clear Workspace


# Functions
convertCurrency <- function(currency) {
        currency1 <- sub('$', '', as.character(currency), fixed = TRUE)
        currency2 <- as.numeric(gsub('\\,', '', as.character(currency1))) 
        currency2
}

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

# contract_filenames <- list.files( pattern = "*.csv") #get a list of all contract files
# contract_import_list <- lapply(contract_filenames, read_csv) # convert files in the list to data frames
# TCC <- do.call("rbind", contract_import_list) # combined the data frames into one
# remove(contract_filenames, contract_import_list) #remove unncessary files

TCC <- read_csv("TransmissionContracts_2010_2016.csv")

### Quick Review
#TCC
#summary(TCC)

# Transform
TCC$`End Date` <- dmy(TCC$`End Date`)
TCC$`Start Date` <- dmy(TCC$`Start Date`)
TCC$`Primary Holder` <- as.factor(TCC$`Primary Holder`)

TCC$allocated <- 0
#sum(TCC$allocated) #check
TCC$allocated[TCC$`Market Clr Price` == "N/A"] <- 1 # Note the mkt clear prices that were listed as N/A by contract 

TCC$`Market Clr Price` <- gsub("N/A", "", TCC$`Market Clr Price`) # Run after noting N/A in allocated column
TCC$`Market Clr Price` <- as.numeric(TCC$`Market Clr Price`)

TCC$days <- TCC$`End Date` - TCC$`Start Date` + 1 ### Contract Length in days
TCC$days_numeric <- as.numeric(TCC$days)

#TCC #review
#summary(TCC$`Market Clr Price`)
#summary(TCC)



### Subset Contract Data to Monthly FTRs ###################
###########################################################

TCC_monthly_contracts <- TCC %>%
        filter(days <= 31)
TCC_monthly_contracts$month <- month(TCC_monthly_contracts$`Start Date`)
TCC_monthly_contracts$year <- year(TCC_monthly_contracts$`Start Date`)
TCC_monthly_contracts$`POI ID` <- as.numeric(TCC_monthly_contracts$`POI ID`)
TCC_monthly_contracts$`POW ID` <- as.numeric(TCC_monthly_contracts$`POW ID`)

# summary(TCC_monthly_contracts)

### Add Summer and winter True Fales
# Summer vs winter periods: http://www.nyiso.com/public/markets_operations-services-customer_support-faq-index.jsp
# Summer May 1st - Oct 31st
# Winter Nov 1st - April 30th


TCC_monthly_contracts$winter_month <- ifelse(test = (month(TCC_monthly_contracts$`Start Date`) > month(mdy("10/31/2016")) | (month(TCC_monthly_contracts$`End Date`)< month(mdy("5/1/2016")) )), 
                                                      yes = TRUE, 
                                                      no = FALSE) # If monthly contract, and winter, set to TRUE



#summary(TCC_monthly_contracts$winter_month) # quick check
#summary(TCC_monthly_contracts)




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
combined_dam$Cost_of_Losses <- convertAccounting(combined_dam$Cost_of_Losses) # this does not work
combined_dam$Cost_of_Congestion <- convertAccounting(combined_dam$Cost_of_Congestion)
combined_dam$avg_hourly_cost_of_losses <- convertAccounting(combined_dam$avg_hourly_cost_of_losses)
combined_dam$avg_hourly_cost_of_congestion <- convertAccounting(combined_dam$avg_hourly_cost_of_congestion)

## Breakout Month and Year from filename with Regex
combined_dam$month <- sapply(strsplit(combined_dam$filename, split = "_"), "[", 1) #Breakout Month
combined_dam$year <- sapply(strsplit(combined_dam$filename, split = "_"), "[", 2) #Breakout Year
combined_dam$date <- ymd(paste0(combined_dam$year, combined_dam$month, "16"))
combined_dam$month <- month(combined_dam$date) #convert to month, date format
combined_dam$year <- year(combined_dam$date) #convert to year, date format

# summary(combined_dam) # Review






#################### JOIN DATA ##########
#######################################


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
#POI_join <- inner_join(x = TCC_monthly_contracts, y = monthly_DAM_POI, by = c("POI ID", "year", "month"))
#summary(POI_join)

# subset TCCS to 2010 through 2016
TCC_2010_2016 <- subset(TCC_monthly_contracts, `Start Date` >= ymd("2010-1-1") & `End Date` <= ymd("2016-12-31"))
#summary(TCC_2010_2016)
TCC_2010_2016$ID <- seq.int(nrow(TCC_2010_2016)) #Add TCC ID by row


POI_join <- left_join(x = TCC_2010_2016, y = monthly_DAM_POI, by = c("POI ID", "year", "month"))
#summary(POI_join)

test2 <- left_join(x = POI_join, y = monthly_DAM_POW, by = c("POW ID", "year", "month")) 
summary(test2)



####  Revenue
#test2$revenue <- ifelse(test = test2$winter_month==T, yes = test2$`MW Winter`, no = test2$`MW Summer`) * ((-1*test2$Cost_of_Congestion_POW)-(-1*test2$Cost_of_Congestion_POI)) # If winter, use winter MWh, else use Summer MWh, then multiply by congestion between points to get rent
test2$`Market Clr Price`[is.na(test2$`Market Clr Price`)] <- 0 #assign to allocated

test2$mwh_calc <- ifelse(test = test2$winter_month==T, yes = test2$`MW Winter`, no = test2$`MW Summer`)
test2$congest_diff <- (-1*test2$Cost_of_Congestion_POW) - (-1*test2$Cost_of_Congestion_POI)
test2$revenue <- test2$mwh_calc*test2$congest_diff
test2$cost <- test2$mwh_calc*test2$`Market Clr Price`
test2$profit <- test2$revenue - test2$cost



summary(test2)
summary(test2$revenue)
plot(test2$revenue)
summary(test2$profit)
plot(test2$`End Date`, test2$profit)


#### Percent Return
# Return(i) = [Profit(i) / absolute|MarketClearingPrice(i)|] * 100%

#test2$abs_mktprice <- abs(test2$`Market Clr Price`)
# Need to save account for $0.00 market clearing price
# Change 0.00 to 0.001
#test2$abs_mktprice[test2$abs_mktprice == 0.00] <- 0.01 
#summary(test2$abs_mktprice)


#### FIND TCC that is paid that does not fit... where they are paid to get a tcc (-auction price) and they are also paid to + revenue (twice paid, no investment)


#test2$return <- (test2$profit/test2$abs_mktprice*MWH)*100
#summary(test2$return)
#plot(test2$`End Date`, test2$return) # quick view
allocated_TCC <- subset(test2, allocated >0)
market_TCC <- subset(test2, allocated <1)
sum(allocated_TCC$profit, na.rm = TRUE)
sum(market_TCC$profit, na.rm = TRUE)


## Plots
plot_profit <- ggplot(market_TCC, 
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


rplot <- ggMarginal(plot_profit, type = "density", margins = "y", color = "light grey") # Add 
ggsave("NYISO.png", plot = rplot, width = 6, height = 4, dpi = 300)


## histogram profits
hist_profit <- ggplot(market_TCC, 
                      aes(profit, bindwidth = 0.1)
) + 
        geom_histogram()
hist_profit

summary(market_TCC$profit)

sum(test2$profit, na.rm = TRUE)
summary(test2$`Start Date`)
plot_holderprice <- ggplot(data=Jan2016, 
                           aes(x =`Market Clr Price`,
                               y =`Primary Holder`)
                           ) +
        geom_point(alpha = 0.2)
plot_holderprice


######
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

#### Scratch
x <- subset(test2, `Primary Holder` == "DC Energy LLC")
summary(test$`Primary Holder`)
#DC Energy New York, LLC
#DC Energy New England, LLC

x <- subset(test2, `Primary Holder` == "DC Energy New York, LLC")
x <- subset(test2, `Primary Holder` == "DC Energy New York, LLC" | `Primary Holder` == "DC Energy New England, LLC" | `Primary Holder` =="DC Energy LLC")
summary(x$`Primary Holder`)


sum(x$profit, na.rm = TRUE)
str(x)
summary(x$`Start Date`)

sum(test2$profit, na.rm = TRUE)
sum(test3$profit, na.rm = TRUE)


DCenergy <- subset(test2, `Primary Holder` == "DC Energy New York, LLC")
DCenergy2014 <- subset(DCenergy, `Start Date`>= ymd("2014-1-1") & `Start Date` <= ymd("2014-12-31"))
summary(DCenergy2014$`Start Date`)

sum(DCenergy2014$profit, na.rm = TRUE)

###### Sum profits by year
test2$profit
profit_by_year <- test2 %>% 
        group_by(year) %>%
        summarise(Profit = sum(profit), Revenue = sum(revenue), Cost = sum(cost))
profit_by_year

### profit by holder
profit_by_holder <- test2 %>% 
        group_by(year, `Primary Holder`) %>%
        summarise(Profit = sum(profit), Revenue = sum(revenue), Cost = sum(cost), Count_TCC = n()) %>% 
        arrange(year, desc(Profit), `Primary Holder`)

profit_by_holder
write.csv(x = profit_by_holder, file = "1_profit_holder.csv")

###
profit_by_poi <- test2 %>% 
        group_by(year, `POI ID`) %>%
        summarise(Profit = sum(profit), Revenue = sum(revenue), Cost = sum(cost), Count_TCC = n()) %>% 
        arrange(year, desc(Profit), `POI ID`)
profit_by_poi$`POI ID` <- as.factor(profit_by_poi$`POI ID`)

plot_poi <- ggplot(data = profit_by_poi, aes(x = year, y = Profit, color = `POI ID`)) + 
        geom_line() +
        scale_y_continuous(labels =comma) +
        theme(legend.position = "none")
plot_poi

write.csv(x = profit_by_poi, file = "2_profit_poi.csv")

###
profit_by_pow <- test2 %>% 
        group_by(year, `POW ID`) %>%
        summarise(Profit = sum(profit), Revenue = sum(revenue), Cost = sum(cost), Count_TCC = n()) %>% 
        arrange(year, desc(Profit), `POW ID`)
profit_by_pow

profit_by_pow$`POW ID` <- as.factor(profit_by_pow$`POW ID`)

plot_pow <- ggplot(data = profit_by_pow, aes(x = year, y = Profit, color = `POW ID`)) + 
        geom_line() +
        scale_y_continuous(labels =comma) +
        theme(legend.position = "none")
plot_pow

### POW Name
profit_by_pow <- test2 %>% 
        group_by(year, `POW Name`) %>%
        summarise(Profit = sum(profit), Revenue = sum(revenue), Cost = sum(cost), Count_TCC = n()) %>% 
        arrange(year, desc(Profit), `POW Name`)
profit_by_pow

profit_by_pow$`POW Name` <- as.factor(profit_by_pow$`POW Name`)

plot_pow <- ggplot(data = profit_by_pow, aes(x = year, y = Profit, color = `POW Name`)) + 
        geom_line() +
        scale_y_continuous(labels =comma) +
        geom_text(aes(label = `POW Name`)) +        
        theme(legend.position = "none") +
        ggtitle("Point of Withdrawl, Total Profit by Year")
plot_pow


### subset to LONGIL
longil <- subset(test2, `POW Name`=="LONGIL")
