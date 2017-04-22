#############################################################################
###### Load DAILY Day Ahead Market Data                                 ####
###########################################################################

rm(list = ls()) #Clear Workspace
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scales)
library(ggthemes)
library(pool)
library(RPostgreSQL)


dbSafeNames = function(names) {
        names = gsub('[^a-z0-9]+','_',tolower(names))
        names = make.names(names, unique=TRUE, allow_=TRUE)
        names = gsub('.','_',names, fixed=TRUE)
        names
}

# Data source = http://mis.nyiso.com/public/P-2Blist.htm
# Files are uncompressed into the same folder, then multiple csv loads are used to combine into single data frame

# Downloads complete
# 2010, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# 2011, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# 2012, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# 2013, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# 2014, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# 2015, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# 2016, 1, 


#### Create a connection to postgresql database ####
pw <- scan(".pgpass", what="")  # read password from local file
#database_name <- scan(".pgdb", what="") 
database_name <- "NYISO"
host_name <- scan(".pghn", what="") 
port_name <- as.numeric(scan(".pgpn", what="")) #read local file. Yep, even for a port name
user_name <- scan(".pgun", what="")  #postgres
db_driver <- dbDriver("PostgreSQL") # loads PostgreSQL driver

con <- dbConnect(drv = db_driver,
                 dbname = database_name,
                 host = host_name,
                 port = port_name,
                 user = user_name,
                 password = pw) #creates a connection to database to call upon
remove(pw, database_name, host_name, port_name, user_name) # remove variable, retain connect call





getwd()
setwd("./DAM_Daily") #change working director to DAM
setwd("..")
getwd()

#### 2010 DAM Daily Data ####
filenames_2010 <- list.files(path = "./DAM_Daily/2010", 
                        pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_Daily/2010")
import_list_2010 <- lapply(filenames_2010, read.csv) # read in files
dam_daily_2010 <- do.call("rbind", import_list_2010) # combine CSV files into dataframe
colnames(dam_daily_2010) <- dbSafeNames(colnames(dam_daily_2010)) #database friendly names
dam_daily_2010$time_stamp <- mdy_hm(as.character(dam_daily_2010$time_stamp))
dam_daily_2010$name <- as.character(dam_daily_2010$name)
dam_daily_2010$ptid <- as.character(dam_daily_2010$ptid)
remove(filenames_2010) #cleanup
setwd("..")
setwd("..")
# getwd()

dbWriteTable(con,'dam_daily',dam_daily_2010, row.names=FALSE) #create table and load 2010
remove(dam_daily_2010, import_list_2010)


#### 2011 DAM Daily Data ####
filenames_2011 <- list.files(path = "./DAM_Daily/2011", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2011")
import_list_2011 <- lapply(filenames_2011, read.csv) # read in files
dam_daily_2011 <- do.call("rbind", import_list_2011) # combine CSV files into dataframe

colnames(dam_daily_2011) <- dbSafeNames(colnames(dam_daily_2011)) #database friendly names
dam_daily_2011$time_stamp <- mdy_hm(as.character(dam_daily_2011$time_stamp)) #convert to date
dam_daily_2011$name <- as.character(dam_daily_2011$name) #convert to character
dam_daily_2011$ptid <- as.character(dam_daily_2011$ptid) # id is a character
remove(filenames_2011) #cleanup
setwd("..")
setwd("..")
# getwd()

dbWriteTable(con, "dam_daily", dam_daily_2011, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2011, import_list_2011)


#### 2012 DAM Daily Data ####
filenames_2012 <- list.files(path = "./DAM_Daily/2012", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2012")
import_list_2012 <- lapply(filenames_2012, read.csv) # read in files
dam_daily_2012 <- do.call("rbind", import_list_2012) # combine CSV files into dataframe

colnames(dam_daily_2012) <-  dbSafeNames(colnames(dam_daily_2012)) #database friendly names
dam_daily_2012$time_stamp <- mdy_hm(as.character(dam_daily_2012$time_stamp)) #convert to date
dam_daily_2012$name <- as.character(dam_daily_2012$name) #convert to character
dam_daily_2012$ptid <- as.character(dam_daily_2012$ptid) # id is a character
remove(filenames_2012) #cleanup
setwd("..")
setwd("..")
# getwd()

dbWriteTable(con, "dam_daily", dam_daily_2012, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2012, import_list_2012)


#### 2013 DAM Daily Data ####
filenames_2013 <- list.files(path = "./DAM_Daily/2013", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2013")
import_list_2013 <- lapply(filenames_2013, read.csv) # read in files
dam_daily_2013 <- do.call("rbind", import_list_2013) # combine CSV files into dataframe

colnames(dam_daily_2013) <-  dbSafeNames(colnames(dam_daily_2013)) #database friendly names
dam_daily_2013$time_stamp <- mdy_hm(as.character(dam_daily_2013$time_stamp)) #convert to date
dam_daily_2013$name <- as.character(dam_daily_2013$name) #convert to character
dam_daily_2013$ptid <- as.character(dam_daily_2013$ptid) # id is a character
remove(filenames_2013) #cleanup
setwd("..")
setwd("..")
# getwd()

dbWriteTable(con, "dam_daily", dam_daily_2013, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2013, import_list_2013)


#### 2014 DAM Daily Data ####
filenames_2014 <- list.files(path = "./DAM_Daily/2014", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2014")
import_list_2014 <- lapply(filenames_2014, read.csv) # read in files
dam_daily_2014 <- do.call("rbind", import_list_2014) # combine CSV files into dataframe

colnames(dam_daily_2014) <-  dbSafeNames(colnames(dam_daily_2014)) #database friendly names
dam_daily_2014$time_stamp <- mdy_hm(as.character(dam_daily_2014$time_stamp)) #convert to date
dam_daily_2014$name <- as.character(dam_daily_2014$name) #convert to character
dam_daily_2014$ptid <- as.character(dam_daily_2014$ptid) # id is a character
remove(filenames_2014) #cleanup
setwd("..")
setwd("..")
# getwd()

dbWriteTable(con, "dam_daily", dam_daily_2014, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2014, import_list_2014)


#### 2015 DAM Daily Data ####
filenames_2015 <- list.files(path = "./DAM_Daily/2015", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2015")
import_list_2015 <- lapply(filenames_2015, read.csv) # read in files
dam_daily_2015 <- do.call("rbind", import_list_2015) # combine CSV files into dataframe

colnames(dam_daily_2015) <-  dbSafeNames(colnames(dam_daily_2015)) #database friendly names
dam_daily_2015$time_stamp <- mdy_hm(as.character(dam_daily_2015$time_stamp)) #convert to date
dam_daily_2015$name <- as.character(dam_daily_2015$name) #convert to character
dam_daily_2015$ptid <- as.character(dam_daily_2015$ptid) # id is a character
remove(filenames_2015) #cleanup
setwd("..")
setwd("..")
# getwd()

dbWriteTable(con, "dam_daily", dam_daily_2015, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2015, import_list_2015)


# 2016, 1, 2, 3, 4, 5, 6
#### 2016 DAM Daily Data, through 2016-06-22 ####
filenames_2016 <- list.files(path = "./DAM_Daily/2016", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2016")
import_list_2016 <- lapply(filenames_2016, read.csv) # read in files
dam_daily_2016 <- do.call("rbind", import_list_2016) # combine CSV files into dataframe

colnames(dam_daily_2016) <-  dbSafeNames(colnames(dam_daily_2016)) #database friendly names
saved_colnames <- colnames(dam_daily_2016) #
dam_daily_2016$time_stamp <- mdy_hm(as.character(dam_daily_2016$time_stamp)) #convert to date
dam_daily_2016$name <- as.character(dam_daily_2016$name) #convert to character
dam_daily_2016$ptid <- as.character(dam_daily_2016$ptid) # id is a character
remove(filenames_2016) #cleanup
setwd("..")
setwd("..")
getwd()
summary(dam_daily_2016$time_stamp)

dbWriteTable(con, "dam_daily", dam_daily_2016, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2016, import_list_2016)


# 2016, 6, 7, 8, 9, 10, 11, 12
#### 2016 DAM Daily Data, from 2016-06-22 ####
## someone at NYISO corrected the column names for data past the 22nd
filenames_2016_2 <- list.files(path = "./DAM_Daily/2016_2ndHalf", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2016_2ndHalf")
import_list_2016_2 <- lapply(filenames_2016_2, read.csv) # read in files
dam_daily_2016_2 <- do.call("rbind", import_list_2016_2) # combine CSV files into dataframe

colnames(dam_daily_2016_2) <- saved_colnames #use previous data column names
dam_daily_2016_2$time_stamp <- mdy_hm(as.character(dam_daily_2016_2$time_stamp)) #convert to date
dam_daily_2016_2$name <- as.character(dam_daily_2016_2$name) #convert to character
dam_daily_2016_2$ptid <- as.character(dam_daily_2016_2$ptid) # id is a character
remove(filenames_2016_2) #cleanup
setwd("..")
setwd("..")
getwd()
summary(dam_daily_2016_2$time_stamp)

#colnames(dam_daily_2016_2) <- 

dbWriteTable(con, "dam_daily", dam_daily_2016_2, row.names=FALSE, append=TRUE) #append
remove(dam_daily_2016_2, import_list_2016_2)


# Disconnect ####
dbDisconnect(con) #disconnect from database
