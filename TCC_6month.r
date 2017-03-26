#############################################################################
###### 6 Month TCC                                                      ####
###########################################################################

# Functions
# Sum Congestion by POI and POW
func_POI <- function(TCC_data){
        combined_dam %>%
                filter(PTID == TCC_data$`POI ID`,
                       date >= TCC_data$`Start Date` &
                               date <= TCC_data$`End Date`) %>%
                summarize(POI_Congestion_Cost = sum(Cost_of_Congestion))
} # Look at monthly DAM data, search for each POI ID matching the supplid TCC contract that falls within the contract range. Using that subset of data, sum the costs of congestoin and return that result. 
func_POW <- function(TCC_data){
        combined_dam %>%
                filter(PTID == TCC_data$`POW ID`,
                       date >= TCC_data$`Start Date` &
                               date <= TCC_data$`End Date`) %>%
                summarize(POW_Congestion_Cost = sum(Cost_of_Congestion))
}



##### Add DAM for each TCC
# Point of Injection TCC
TCC_6mo$Cost_of_Congestion_POI <- by(TCC_6mo, 1:nrow(TCC_6mo), func_POI)
TCC_6mo$Cost_of_Congestion_POI <- sapply(TCC_6mo$Cost_of_Congestion_POI, paste0, collapse="") #unlist
TCC_6mo$Cost_of_Congestion_POI <- as.numeric(TCC_6mo$Cost_of_Congestion_POI)

TCC_6mo$Cost_of_Congestion_POW <- by(TCC_6mo, 1:nrow(TCC_6mo), func_POW)
TCC_6mo$Cost_of_Congestion_POW <- sapply(TCC_6mo$Cost_of_Congestion_POW, paste0, collapse="") #unlist
TCC_6mo$Cost_of_Congestion_POW <- as.numeric(TCC_6mo$Cost_of_Congestion_POW)

test6mo$Cost_of_COngestion_POW[[1]] - test6mo$Cost_of_Congestion_POI[[1]]
test6mo$Cost_of_Congestion_POW <- sapply(test6mo$Cost_of_Congestion_POW, paste0, collapse="") #unlist

# date... year-m-16

test6mo <- head(TCC_6mo)

test6mo$`POI ID`[1]
test6mo$`POI ID`[1]
str(test6mo$`Start Date`[1])
test6mo$`End Date`[1]



x <- combined_dam %>%
        filter(PTID == test6mo$`POW ID`[1], 
               date >= test6mo$`Start Date`[1] & 
                       date <= test6mo$`End Date`[1]) %>%
        summarize(POI_Congestion_Cost = sum(Cost_of_Congestion))
x
test6mo$Cost_of_Congestion_POW <- combined_dam
