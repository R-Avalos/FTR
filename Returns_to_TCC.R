#############################################################################
###### TCC Analysis                                                     ####
###########################################################################

# Reviewing returns to TCCs within NYISO 2010-2016
# Breakdown of 1 month, 6 month, 1 year, >1 year, all
# Breakdown by marketp participants
rm(list = ls()) #Clear Workspace

####################################
###  Setup Enviornment    #########
##################################


####################################
###  Load Data            #########
##################################

source(file = "load_dam_data.r", encoding = "UTF-8") # Load Day Ahead Market Data
source(file = "load_tcc_data.r", encoding = "UTF-8") # Load Transmission  Congestion Contract Data

####################################
###  Join Data            #########
##################################
duplicate_rows <- TCC[duplicated(TCC),]
unique_rows <- TCC[unique(TCC),]
count(unique(TCC))
# Join