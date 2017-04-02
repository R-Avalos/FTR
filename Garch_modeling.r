# GARCH Modeling of Data


# GARCH helps model volatility clustering. As the power grid is inteconnected, a Locational marginal price spike will effect nearby lmp prices. There are periods of relative calm and periods of high volatility (see winter storm knocking out power lines) 


# Long tail returns
# Ljung Box text to see if there is auto correlation (we don't want that)

install.packages("rugarch")


# data


# E(R_i) = R_free + B_i[E(R_mkt) - R_free]  ... CAPM 
# R_it - R_freet = A_i + B_i[R_mktt - R_freet] + U_it  ... time series
# TCC_DAM_Monthly$excess return =  A_i + B_i[R_mktt - R_freet] + U_it 




# Run Subsets