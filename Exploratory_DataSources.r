# Exploratory -- Data Sources ###

library(readr)
library(lubridate)
library(ggplot2)

# NYISO
# http://tcc.nyiso.com/tcc/public/view_summary_of_transmission_contracts.do
list.files()
Jan2016 <- read_csv("TransmissionContracts_NYISO_example_Jan2016.csv")
Jan2016$`End Date` <- dmy(Jan2016$`End Date`)
Jan2016$`Start Date` <- dmy(Jan2016$`Start Date`)
Jan2016$`Primary Holder` <- as.factor(Jan2016$`Primary Holder`)

Jan2016$`Market Clr Price` <- gsub("N/A", "", Jan2016$`Market Clr Price`)
Jan2016$`Market Clr Price` <- as.numeric(Jan2016$`Market Clr Price`)

Jan2016 #review
summary(Jan2016$`Market Clr Price`)
sd(Jan2016$`Market Clr Price`)

plot_holderprice <- ggplot(data=Jan2016, 
                           aes(y ='Market Clr Price',
                               x ='Start Date')
                           ) +
        geom_point()
plot_holderprice

plot(y = Jan2016$`Market Clr Price`, x = Jan2016$`Primary Holder`)
plot(Jan2016$`Market Clr Price`)
