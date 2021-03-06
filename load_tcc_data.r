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


# list.files()
TCC <- read_csv("TransmissionContracts_2008_2016.csv")
#TCC #quick review
#summary(TCC)

# Transform types
TCC$`End Date` <- dmy(TCC$`End Date`)
TCC$`Start Date` <- dmy(TCC$`Start Date`)
TCC$`Primary Holder` <- as.factor(TCC$`Primary Holder`)
TCC$`POI ID` <- as.character(TCC$`POI ID`)
TCC$`POW ID` <- as.character(TCC$`POW ID`)
TCC$`Contract ID` <- as.character(TCC$`Contract ID`)
TCC$`Att L Ref` <- as.character(TCC$`Att L Ref`)
TCC$`Ref Cont ID` <- as.character(TCC$`Ref Cont ID`)

TCC$allocated <- 0 
#sum(TCC$allocated) #check
TCC$allocated[TCC$`Market Clr Price` == "N/A"] <- 1 # Note the mkt clear prices that were listed as N/A by contract 
#allocated <- TCC[TCC$allocated==1,]

TCC$`Market Clr Price` <- gsub("N/A", "", TCC$`Market Clr Price`) # Run after noting N/A in allocated column
TCC$`Market Clr Price` <- as.numeric(TCC$`Market Clr Price`)
TCC$days <- TCC$`End Date` - TCC$`Start Date` + 1 ### Contract Length in days
TCC$days_numeric <- as.numeric(TCC$days)


### Data Checks  #############

#### Check for N/As
#summary(TCC)
#summary(TCC$`Primary Holder`) # All contracts have holder
#summary(TCC$`MW Summer`)
#summary(TCC$`MW Winter`)
#summary(TCC$`Market Clr Price`) # Market clearing price has N/As for allocated
#sum(is.na(TCC$`Market Clr Price`))
# test <- TCC %>% 
#         filter(TCC$`Market Clr Price` == 5555 | 
#                        TCC$`Market Clr Price`== 9999 |
#                        TCC$`Market Clr Price`== 55555 |
#                        TCC$`Market Clr Price`== 99999 ) # checking for oddities
# summary(TCC$`Market Clr Price`)

# Remove duplicated data within dataset
#duplicate_rows <- TCC[duplicated(TCC),] # duplicated rows in TCCs
TCC <- TCC %>%
        distinct(.keep_all = TRUE) # subset to unique TCCs
#unique_monthly_TCC <- unique_TCC %>%
#        filter(days_numeric <= 31) # subset to monthly TCCs
#count(unique(TCC))


### Subset TCC to Monthly  ###
TCC_monthly_contracts <- TCC %>%
        filter(days <= 31)
TCC_monthly_contracts$month <- month(TCC_monthly_contracts$`Start Date`)
TCC_monthly_contracts$year <- year(TCC_monthly_contracts$`Start Date`)
TCC_monthly_contracts$ID <- seq.int(nrow(TCC_monthly_contracts)) #Add TCC ID by row for future 
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
### Subset TCC to 1 year   ###
#############################

TCC_1yr <- TCC %>%
        filter(days > 187 & days < 366)



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



###############################
### Confirm results        ###
#############################

print("Tranmission Congestion Contract (TCC) data loaded")
