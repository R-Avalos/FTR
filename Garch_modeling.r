# GARCH Modeling of Data


# GARCH helps model volatility clustering. As the power grid is inteconnected, a Locational marginal price spike will effect nearby lmp prices. There are periods of relative calm and periods of high volatility (see winter storm knocking out power lines) 


# Long tail returns
# Ljung Box text to see if there is auto correlation (we don't want that)
#install.packages("tidyverse")
#install.packages("rugarch")
library(rugarch)
library(tidyverse)
library(reshape2)


## Test #2
data <- cbind(rnorm(1000), rnorm(1000))
data <- as.data.frame(data)
# Yt = a + bXt + e
# column 1 = Yt  (excess returns path)
# column 2 = Xt  (excess returns market)
plot(data[,1], data[,2])
spec_test <- ugarchspec(variance.model = list(model = "sGARCH",
                                              garchOrder = c(1, 1),
                                              submodel = NULL,
                                              external.regressors = NULL,
                                              variance.targeting = FALSE),
                        mean.model = list(armaOrder = c(0,0),
                                           external.regressors = matrix(data[,2])
                                           )
)

garch <- ugarchfit(spec = spec_test,
                   data = data[,1],
                   solver.control = list(trace = 0)
                   )
garch@fit$matcoef

# data
TCC_Path_Returns #From Returns_to_TCC.R

# E(R_i) = R_free + B_i[E(R_mkt) - R_free]  ... CAPM 
# R_it - R_freet = A_i + B_i[R_mktt - R_freet] + U_it  ... time series
# TCC_DAM_Monthly$excess return =  A_i + B_i[R_mktt - R_freet] + U_it 


# For each path, garch model

## Reshape Data

Test <- dcast(TCC_Path_Returns, year + month ~ Path_Name, value.var = "Path_excess_return") # works well... need more data
Data_test2 <- Test$`23512-->24120`[complete.cases(Test$`23512-->24120`)]

summary(Test$`23512-->24117`)
garch_fit_test2 <- ugarchfit(spec = specification_rugarch, data = Data_test2) # run the model
coef(garch_fit_test2) #coeficient summary




### Calculate Market Portfolio Returns ###
# Account for all paths not included
# R_m = Sum (Return_i *  ( TotalMW_i / TotalMW_m))
# TotalMW_m = all megawatts held for a month across every path


TotalMW_monthly <- market_TCC %>%
        group_by()


### Calculate Individual Contract Returns ###


### Dependenint Variable is the excess returns (Asset Return - Risk Free Return) for indidvidual asset regressions
# TCC_Path_Returns$Path_excess_return




#### Test GARCH Model with Daily rate for Monthly T-Bills ######
###  needs at least 100 data points ###########################
specification_rugarch <- ugarchspec(mean.model=list(
        armaOrder=c(1,1)), distribution="std")
# garch_fit <- ugarchfit(spec = specification_rugarch, data = x$Path_excess_return)


#Data_test <- read.csv(file = "DGS1MO.csv", stringsAsFactors = FALSE) #daily returns 1 month t-bill

Data_test <- T_Bill

#Data_test$DGS1MO <- as.numeric(Data_test$DGS1MO) #convert to numeric
#Data_test$DATE <- ymd(Data_test$DATE) #convert to date format
#Data_test <- Data_test[complete.cases(Data_test),] #remove na rows
plot(Data_test$DATE, Data_test$GS1M) # quick review
acf(Data_test$GS1M) #plot auto correlation
pacf(Data_test$GS1M) #plot partial auto correlation


show(specification_rugarch)
garch_fit_test <- ugarchfit(spec = specification_rugarch, data = Data_test$GS1M) # run the model
coef(garch_fit_test) #coeficient summary
garch_fit_test # full summary
plot(sqrt(108)*garch_fit_test@fit$sigma, type = "l")
plot(garch_fit_test@fit$sigma, type = "l")
# Run Subsets