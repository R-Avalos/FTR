#############################################################################
###### Load Transmission Congestion Revenue Rights Data                 ####
###########################################################################


library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scales)
library(ggthemes)

#rm(list = ls()) #Clear Workspace


# Functions
convertCurrency <- function(currency) {
        currency1 <- sub('$', '', as.character(currency), fixed = TRUE)
        currency2 <- as.numeric(gsub('\\,', '', as.character(currency1))) 
        currency2
}  # Convert standard currency to numeric


###############################
### Load Contract Data  ######
#############################

# NYISO Data source
# http://tcc.nyiso.com/tcc/public/view_summary_of_transmission_contracts.do
# Manually removed footers from csv


list.files()
TCC <- read_csv("TransmissionContracts_2010_2016.csv")

### Quick Review
#TCC
#summary(TCC)

# Transform
TCC$`End Date` <- dmy(TCC$`End Date`)
TCC$`Start Date` <- dmy(TCC$`Start Date`)
TCC$`Primary Holder` <- as.factor(TCC$`Primary Holder`)
TCC$`POI ID` <- as.character(TCC$`POI ID`)

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



###############################
### Subset TCC to Monthly  ###
#############################

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


###############################
### Subset TCC to 6 month  ###
#############################

TCC_6mo <- TCC %>%
        filter(days > 31 & days < 187)


###############################
### Subset TCC to 1 year   ###
#############################

TCC_1yr <- TCC %>%
        filter(days > 187 & days < 366)
