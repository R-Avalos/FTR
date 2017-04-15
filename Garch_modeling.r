# GARCH Modeling of Data


# GARCH helps model volatility clustering. As the power grid is inteconnected, a Locational marginal price spike will effect nearby lmp prices. There are periods of relative calm and periods of high volatility (see winter storm knocking out power lines) 


# Long tail returns
# Ljung Box text to see if there is auto correlation (we don't want that)

install.packages("rugarch")
library(rugarch)

# data
TCC_Path_Returns #From Returns_to_TCC.R

# E(R_i) = R_free + B_i[E(R_mkt) - R_free]  ... CAPM 
# R_it - R_freet = A_i + B_i[R_mktt - R_freet] + U_it  ... time series
# TCC_DAM_Monthly$excess return =  A_i + B_i[R_mktt - R_freet] + U_it 


# For each path, garch model
x <- TCC_Path_Returns
x$path <- paste(x$`POI ID`, x$`POW ID`, sep = "_")
x2 <- x %>%
        filter(path == "23512_24120")

x2$date <- ymd(paste(as.character(x2$year), "-", as.character(x2$month), "-1"))

x2 <- x2 %>%
        order(x2$date)
####
specification_rugarch <- ugarchspec(mean.model=list(
        armaOrder=c(1,1)), distribution="std")
garch_fit <- ugarchfit(spec = specification_rugarch, data = x$Path_excess_return)




#### Test GARCH Model with Daily rate for Monthly T-Bills ######
###  needs at least 100 data points ###########################

Data_test <- read.csv(file = "DGS1MO.csv", stringsAsFactors = FALSE) #daily returns 1 month t-bill
Data_test$DGS1MO <- as.numeric(Data_test$DGS1MO) #convert to numeric
Data_test$DATE <- ymd(Data_test$DATE) #convert to date format
Data_test <- Data_test[complete.cases(Data_test),] #remove na rows
plot(Data_test$DATE, Data_test$DGS1MO) # quick review
acf(Data_test$DGS1MO) #plot auto correlation
pacf(Data_test$DGS1MO) #plot partial auto correlation


show(specification_rugarch)
garch_fit_test <- ugarchfit(spec = specification_rugarch, data = Data_test$DGS1MO) # run the model
coef(garch_fit_test) #coeficient summary
garch_fit_test # full summary
plot(sqrt(252)*garch_fit_test@fit$sigma, type = "l")
# Run Subsets