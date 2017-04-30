#############################################################################
###### Load DAILY Day Ahead Market Data                                 ####
###########################################################################


### Load hourly DAM data from http://mis.nyiso.com/public/ website, P-2A and P-2B
# Process
# Download csv's from website and store into folders
# Folders: DAM > year
#          DAM_Zonal > year
# Read in a year's worth of CSVs from P-2B files
# Aggregate hourly into daily totals
# Save to data frame
# Repeat for P-2A files
# Repeat for each year


rm(list = ls()) #Clear Workspace
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(scales)
library(ggthemes)
library(RPostgreSQL)


dbSafeNames = function(names) {
        names = gsub('[^a-z0-9]+','_',tolower(names))
        names = make.names(names, unique=TRUE, allow_=TRUE)
        names = gsub('.','_',names, fixed=TRUE)
        names
}

# Data source = http://mis.nyiso.com/public/P-2Blist.htm
# Files are uncompressed into the same folder, then multiple csv loads are used to combine into single data frame

#### Create a connection to postgresql database ####
pw <- scan(".pgpass", what="")  # read password from local file
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

db_list_tables(con) #list tables
dbListFields(con, "paths")
#dbSendQuery(con, "drop table monthly_path_returns")


#### 2010 DAM Daily Data ####
############################
filenames_2010 <- list.files(path = "./DAM_Daily/2010", 
                        pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_Daily/2010")
import_list_2010 <- lapply(filenames_2010, read.csv) # read in files
dam_daily_2010 <- do.call("rbind", import_list_2010) # combine CSV files into dataframe
colnames(dam_daily_2010) <- dbSafeNames(colnames(dam_daily_2010)) #database friendly names

saved_colnames <- colnames(dam_daily_2010)

dam_daily_2010$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2010$time_stamp))) #removes hours and converts to date

dam_daily_2010$name <- as.character(dam_daily_2010$name)
dam_daily_2010$ptid <- as.character(dam_daily_2010$ptid)
remove(filenames_2010) #cleanup
setwd("..")
setwd("..")
# getwd()
# summary(dam_daily_2010)

### Sum totals by day
dam_daily_2010_sum <- dam_daily_2010 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2010_sum <- as.data.frame(dam_daily_2010_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2010_sum, row.names=FALSE) #create table and load 2010
remove(dam_daily_2010, import_list_2010, dam_daily_2010_sum)


#### 2010 DAM Zonal Daily Data ####
filenames_2010_zonal <- list.files(path = "./DAM_zonal/2010", 
                             pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2010")
import_list_2010_zonal <- lapply(filenames_2010_zonal, read.csv) # read in files
dam_daily_2010_zonal <- do.call("rbind", import_list_2010_zonal) # combine CSV files into dataframe

colnames(dam_daily_2010_zonal) <- saved_colnames #use specific column names

dam_daily_2010_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2010_zonal$time_stamp)))
dam_daily_2010_zonal$name <- as.character(dam_daily_2010_zonal$name)
dam_daily_2010_zonal$ptid <- as.character(dam_daily_2010_zonal$ptid)
remove(filenames_2010_zonal) #cleanup
setwd("..")
setwd("..")
# getwd()
# summary(dam_daily_2010_zonal)

### Sum Zonal totals by day
dam_daily_2010_zonal_sum <- dam_daily_2010_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2010_zonal_sum <- as.data.frame(dam_daily_2010_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2010_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2010_zonal, import_list_2010_zonal, dam_daily_2010_zonal_sum)



#### 2011 DAM Daily Data ####
filenames_2011 <- list.files(path = "./DAM_Daily/2011", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2011")
import_list_2011 <- lapply(filenames_2011, read.csv) # read in files
dam_daily_2011 <- do.call("rbind", import_list_2011) # combine CSV files into dataframe

colnames(dam_daily_2011) <- dbSafeNames(colnames(dam_daily_2011)) #database friendly names

dam_daily_2011$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2011$time_stamp)))

dam_daily_2011$name <- as.character(dam_daily_2011$name) #convert to character
dam_daily_2011$ptid <- as.character(dam_daily_2011$ptid) # id is a character
remove(filenames_2011) #cleanup
setwd("..")
setwd("..")
# getwd()

### Sum totals by day
dam_daily_2011_sum <- dam_daily_2011 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2011_sum <- as.data.frame(dam_daily_2011_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2011_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2011, import_list_2011, dam_daily_2011_sum)

#### 2011 DAM Zonal Daily Data ####
filenames_2011_zonal <- list.files(path = "./DAM_zonal/2011", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2011")
import_list_2011_zonal <- lapply(filenames_2011_zonal, read.csv) # read in files
dam_daily_2011_zonal <- do.call("rbind", import_list_2011_zonal) # combine CSV files into dataframe

colnames(dam_daily_2011_zonal) <- saved_colnames #use specific column names

dam_daily_2011_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2011_zonal$time_stamp)))

dam_daily_2011_zonal$name <- as.character(dam_daily_2011_zonal$name)
dam_daily_2011_zonal$ptid <- as.character(dam_daily_2011_zonal$ptid)
remove(filenames_2011_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
# summary(dam_daily_2011_zonal)

### Sum Zonal totals by day
dam_daily_2011_zonal_sum <- dam_daily_2011_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2011_zonal_sum <- as.data.frame(dam_daily_2011_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2011_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2011_zonal, import_list_2011_zonal, dam_daily_2011_zonal_sum)



#### 2012 DAM Daily Data ######
###############################
filenames_2012 <- list.files(path = "./DAM_Daily/2012", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2012")
import_list_2012 <- lapply(filenames_2012, read.csv) # read in files
dam_daily_2012 <- do.call("rbind", import_list_2012) # combine CSV files into dataframe

colnames(dam_daily_2012) <-  dbSafeNames(colnames(dam_daily_2012)) #database friendly names

dam_daily_2012$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2012$time_stamp))) #removes hours and converts to date

dam_daily_2012$name <- as.character(dam_daily_2012$name) #convert to character
dam_daily_2012$ptid <- as.character(dam_daily_2012$ptid) # id is a character
remove(filenames_2012) #cleanup
setwd("..")
setwd("..")
# getwd()

### Sum totals by day
dam_daily_2012_sum <- dam_daily_2012 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2012_sum <- as.data.frame(dam_daily_2012_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2012_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2012, import_list_2012, dam_daily_2012_sum)

#### 2012 DAM Zonal Daily Data ####
filenames_2012_zonal <- list.files(path = "./DAM_zonal/2012", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2012")
import_list_2012_zonal <- lapply(filenames_2012_zonal, read.csv) # read in files
dam_daily_2012_zonal <- do.call("rbind", import_list_2012_zonal) # combine CSV files into dataframe

colnames(dam_daily_2012_zonal) <- saved_colnames #use specific column names

dam_daily_2012_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2012_zonal$time_stamp)))

dam_daily_2012_zonal$name <- as.character(dam_daily_2012_zonal$name)
dam_daily_2012_zonal$ptid <- as.character(dam_daily_2012_zonal$ptid)
remove(filenames_2012_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
# summary(dam_daily_2012_zonal)

### Sum Zonal totals by day
dam_daily_2012_zonal_sum <- dam_daily_2012_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2012_zonal_sum <- as.data.frame(dam_daily_2012_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2012_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2012_zonal, import_list_2012_zonal, dam_daily_2012_zonal_sum)





#### 2013 DAM Daily Data ####
############################
filenames_2013 <- list.files(path = "./DAM_Daily/2013", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2013")
import_list_2013 <- lapply(filenames_2013, read.csv) # read in files
dam_daily_2013 <- do.call("rbind", import_list_2013) # combine CSV files into dataframe

colnames(dam_daily_2013) <-  dbSafeNames(colnames(dam_daily_2013)) #database friendly names

dam_daily_2013$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2013$time_stamp))) #removes hours and converts to date

dam_daily_2013$name <- as.character(dam_daily_2013$name) #convert to character
dam_daily_2013$ptid <- as.character(dam_daily_2013$ptid) # id is a character
remove(filenames_2013) #cleanup
setwd("..")
setwd("..")
# getwd()

### Sum totals by day
dam_daily_2013_sum <- dam_daily_2013 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2013_sum <- as.data.frame(dam_daily_2013_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2013_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2013, import_list_2013, dam_daily_2013_sum)

#### 2013 DAM Zonal Daily Data ####
filenames_2013_zonal <- list.files(path = "./DAM_zonal/2013", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2013")
import_list_2013_zonal <- lapply(filenames_2013_zonal, read.csv) # read in files
dam_daily_2013_zonal <- do.call("rbind", import_list_2013_zonal) # combine CSV files into dataframe

colnames(dam_daily_2013_zonal) <- saved_colnames #use specific column names

dam_daily_2013_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2013_zonal$time_stamp)))

dam_daily_2013_zonal$name <- as.character(dam_daily_2013_zonal$name)
dam_daily_2013_zonal$ptid <- as.character(dam_daily_2013_zonal$ptid)
remove(filenames_2013_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
# summary(dam_daily_2013_zonal)

### Sum Zonal totals by day
dam_daily_2013_zonal_sum <- dam_daily_2013_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2013_zonal_sum <- as.data.frame(dam_daily_2013_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2013_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2013_zonal, import_list_2013_zonal, dam_daily_2013_zonal_sum)






#### 2014 DAM Daily Data ####
############################
filenames_2014 <- list.files(path = "./DAM_Daily/2014", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2014")
import_list_2014 <- lapply(filenames_2014, read.csv) # read in files
dam_daily_2014 <- do.call("rbind", import_list_2014) # combine CSV files into dataframe

colnames(dam_daily_2014) <-  dbSafeNames(colnames(dam_daily_2014)) #database friendly names

dam_daily_2014$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2014$time_stamp))) #removes hours and converts to date

dam_daily_2014$name <- as.character(dam_daily_2014$name) #convert to character
dam_daily_2014$ptid <- as.character(dam_daily_2014$ptid) # id is a character
remove(filenames_2014) #cleanup
setwd("..")
setwd("..")
# getwd()

### Sum totals by day
dam_daily_2014_sum <- dam_daily_2014 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2014_sum <- as.data.frame(dam_daily_2014_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2014_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2014, import_list_2014, dam_daily_2014_sum)

#### 2014 DAM Zonal Daily Data ####
filenames_2014_zonal <- list.files(path = "./DAM_zonal/2014", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2014")
import_list_2014_zonal <- lapply(filenames_2014_zonal, read.csv) # read in files
dam_daily_2014_zonal <- do.call("rbind", import_list_2014_zonal) # combine CSV files into dataframe

colnames(dam_daily_2014_zonal) <- saved_colnames #use specific column names

dam_daily_2014_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2014_zonal$time_stamp)))

dam_daily_2014_zonal$name <- as.character(dam_daily_2014_zonal$name)
dam_daily_2014_zonal$ptid <- as.character(dam_daily_2014_zonal$ptid)
remove(filenames_2014_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
# summary(dam_daily_2014_zonal)

### Sum Zonal totals by day
dam_daily_2014_zonal_sum <- dam_daily_2014_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2014_zonal_sum <- as.data.frame(dam_daily_2014_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2014_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2014_zonal, import_list_2014_zonal, dam_daily_2014_zonal_sum)



#### 2015 DAM Daily Data ####
############################
filenames_2015 <- list.files(path = "./DAM_Daily/2015", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2015")
import_list_2015 <- lapply(filenames_2015, read.csv) # read in files
dam_daily_2015 <- do.call("rbind", import_list_2015) # combine CSV files into dataframe

colnames(dam_daily_2015) <-  dbSafeNames(colnames(dam_daily_2015)) #database friendly names

dam_daily_2015$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2015$time_stamp))) #removes hours and converts to date

dam_daily_2015$name <- as.character(dam_daily_2015$name) #convert to character
dam_daily_2015$ptid <- as.character(dam_daily_2015$ptid) # id is a character
remove(filenames_2015) #cleanup
setwd("..")
setwd("..")
# getwd()

### Sum totals by day
dam_daily_2015_sum <- dam_daily_2015 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2015_sum <- as.data.frame(dam_daily_2015_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2015_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2015, import_list_2015, dam_daily_2015_sum)

#### 2015 DAM Zonal Daily Data ####
filenames_2015_zonal <- list.files(path = "./DAM_zonal/2015", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2015")
import_list_2015_zonal <- lapply(filenames_2015_zonal, read.csv) # read in files
dam_daily_2015_zonal <- do.call("rbind", import_list_2015_zonal) # combine CSV files into dataframe

colnames(dam_daily_2015_zonal) <- saved_colnames #use specific column names

dam_daily_2015_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2015_zonal$time_stamp)))

dam_daily_2015_zonal$name <- as.character(dam_daily_2015_zonal$name)
dam_daily_2015_zonal$ptid <- as.character(dam_daily_2015_zonal$ptid)
remove(filenames_2015_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
# summary(dam_daily_2015_zonal)

### Sum Zonal totals by day
dam_daily_2015_zonal_sum <- dam_daily_2015_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2015_zonal_sum <- as.data.frame(dam_daily_2015_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2015_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2015_zonal, import_list_2015_zonal, dam_daily_2015_zonal_sum)





# 2016, 1, 2, 3, 4, 5, 6
#### 2016 DAM Daily Data, through 2016-06-22 ####
filenames_2016 <- list.files(path = "./DAM_Daily/2016", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2016")
import_list_2016 <- lapply(filenames_2016, read.csv) # read in files
dam_daily_2016 <- do.call("rbind", import_list_2016) # combine CSV files into dataframe

colnames(dam_daily_2016) <-  dbSafeNames(colnames(dam_daily_2016)) #database friendly names
saved_colnames <- colnames(dam_daily_2016) #

dam_daily_2016$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2016$time_stamp))) #removes hours and converts to date

dam_daily_2016$name <- as.character(dam_daily_2016$name) #convert to character
dam_daily_2016$ptid <- as.character(dam_daily_2016$ptid) # id is a character
remove(filenames_2016) #cleanup
setwd("..")
setwd("..")
getwd()
#summary(dam_daily_2016$time_stamp)

### Sum totals by day
dam_daily_2016_sum <- dam_daily_2016 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2016_sum <- as.data.frame(dam_daily_2016_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2016_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2016, import_list_2016, dam_daily_2016_sum)


# 2016, 6, 7, 8, 9, 10, 11, 12
#### 2016 DAM Daily Data, from 2016-06-22 ####
## someone at NYISO corrected the column names for data past the 22nd
filenames_2016_2 <- list.files(path = "./DAM_Daily/2016_2ndHalf", 
                             pattern = "*.csv") #get a list of all 2011 dam price files
setwd("./DAM_Daily/2016_2ndHalf")
import_list_2016_2 <- lapply(filenames_2016_2, read.csv) # read in files
dam_daily_2016_2 <- do.call("rbind", import_list_2016_2) # combine CSV files into dataframe

colnames(dam_daily_2016_2) <- saved_colnames #use previous data column names

dam_daily_2016_2$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2016_2$time_stamp))) #removes hours and converts to date

dam_daily_2016_2$name <- as.character(dam_daily_2016_2$name) #convert to character
dam_daily_2016_2$ptid <- as.character(dam_daily_2016_2$ptid) # id is a character
remove(filenames_2016_2) #cleanup
setwd("..")
setwd("..")
getwd()
#summary(dam_daily_2016_2$time_stamp)

### Sum totals by day
dam_daily_2016_2_sum <- dam_daily_2016_2 %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2016_2_sum <- as.data.frame(dam_daily_2016_2_sum)

dbWriteTable(con,'dam_daily_totals',dam_daily_2016_2_sum, row.names=FALSE, append = TRUE) #create table and load 2011
remove(dam_daily_2016_2, import_list_2016_2, dam_daily_2016_2_sum)


#### 2016 DAM Zonal Daily Data ####
filenames_2016_zonal <- list.files(path = "./DAM_zonal/2016", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2016")
import_list_2016_zonal <- lapply(filenames_2016_zonal, read.csv) # read in files
dam_daily_2016_zonal <- do.call("rbind", import_list_2016_zonal) # combine CSV files into dataframe

colnames(dam_daily_2016_zonal) <- saved_colnames #use specific column names

dam_daily_2016_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2016_zonal$time_stamp)))

dam_daily_2016_zonal$name <- as.character(dam_daily_2016_zonal$name)
dam_daily_2016_zonal$ptid <- as.character(dam_daily_2016_zonal$ptid)
remove(filenames_2016_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
# summary(dam_daily_2016_zonal)

### Sum Zonal totals by day
dam_daily_2016_zonal_sum <- dam_daily_2016_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2016_zonal_sum <- as.data.frame(dam_daily_2016_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2016_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2016_zonal, import_list_2016_zonal, dam_daily_2016_zonal_sum)


#### 2016 DAM Zonal 2nd Half Daily Data ####
filenames_2016_zonal <- list.files(path = "./DAM_zonal/2016_2ndHalf", 
                                   pattern = "*.csv") #get a list of all day ahead market price files
setwd("./DAM_zonal/2016_2ndHalf")
import_list_2016_zonal <- lapply(filenames_2016_zonal, read.csv) # read in files
dam_daily_2016_zonal <- do.call("rbind", import_list_2016_zonal) # combine CSV files into dataframe

colnames(dam_daily_2016_zonal) <- saved_colnames #use specific column names

dam_daily_2016_zonal$time_stamp <- as.Date(mdy_hm(as.character(dam_daily_2016_zonal$time_stamp)))

dam_daily_2016_zonal$name <- as.character(dam_daily_2016_zonal$name)
dam_daily_2016_zonal$ptid <- as.character(dam_daily_2016_zonal$ptid)
remove(filenames_2016_zonal) #cleanup
setwd("..")
setwd("..")
getwd()
summary(dam_daily_2016_zonal)

### Sum Zonal totals by day
dam_daily_2016_zonal_sum <- dam_daily_2016_zonal %>%
        group_by(ptid, time_stamp, name) %>%
        summarise(congestion_cost = sum(marginal_cost_congestion_mwh),
                  lbmp_mwhr = sum(lbmp_mwhr_),
                  marginal_cost_losses = sum(marginal_cost_losses_mwhr_)) 
dam_daily_2016_zonal_sum <- as.data.frame(dam_daily_2016_zonal_sum)
dbWriteTable(con,'dam_daily_totals', dam_daily_2016_zonal_sum, row.names = FALSE, append = TRUE) #Append zonal data
remove(dam_daily_2016_zonal, import_list_2016_zonal, dam_daily_2016_zonal_sum)



# Disconnect ####
dbDisconnect(con) #disconnect from database
