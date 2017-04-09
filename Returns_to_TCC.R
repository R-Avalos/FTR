###### TCC Analysis #########################################################
# Reviewing returns to TCCs within NYISO 2010-2016
# Breakdown of 1 month, 6 month, 1 year, >1 year, all
# Breakdown by marketp participants
rm(list = ls()) #Clear Workspace



###  Setup Enviornment ############
options(scipen=999) #remove scientific notation
library(stargazer) # package for pretty table outputs
library(ggplot2)
library(ggthemes) #tufte theme
library(scales)


###  Load Data  ###################
source(file = "load_dam_data.r", encoding = "UTF-8") # Load Day Ahead Market Data
source(file = "load_tcc_data.r", encoding = "UTF-8") # Load Transmission  Congestion 
T_Bill <- read.csv("GS1M.csv", stringsAsFactors = FALSE) #1 month treasury bill return
# Source = https://fred.stlouisfed.org/series/GS1M


###  Transform Data  ##############
T_Bill$DATE <- ymd(T_Bill$DATE) #convert to date
T_Bill$month <- month(T_Bill$DATE)
T_Bill$year <- year(T_Bill$DATE)


###  Join Data  ###################

## Join DAM data for Point of Injection to TCCs
POI_join <- left_join(x = TCC_monthly_contracts, y = monthly_DAM_POI, by = c("POI ID", "year", "month"))
#summary(POI_join)

## Join DAM data for Point of Withdrawal to TCCs
TCC_DAM_Monthly <- left_join(x = POI_join, y = monthly_DAM_POW, by = c("POW ID", "year", "month")) 
#summary(TCC_DAM_Monthly)
remove(POI_join) #remove

TCC_DAM_Monthly <- TCC_DAM_Monthly %>%
        filter (`Start Date` >= ymd("2010-1-1") & `End Date` <= ymd("2016-12-31")) # subset to 2010 through 2016

## Join T_Bill
TCC_DAM_Monthly <- left_join(x = TCC_DAM_Monthly, y = T_Bill, by = c("year", "month"))


###  Revenue  #####################
# If winter, use winter MWh, else use Summer MWh, then multiply by congestion between points to get rent
TCC_DAM_Monthly$`Market Clr Price`[is.na(TCC_DAM_Monthly$`Market Clr Price`)] <- 0 #assign to allocated, as no cost
TCC_DAM_Monthly$mwh_calc <- ifelse(test = TCC_DAM_Monthly$winter_month==T, yes = TCC_DAM_Monthly$`MW Winter`, no = TCC_DAM_Monthly$`MW Summer`) #determine which mwh total to use based on winter summer value
TCC_DAM_Monthly$congest_diff <- (-1*TCC_DAM_Monthly$Cost_of_Congestion_POW) - (-1*TCC_DAM_Monthly$Cost_of_Congestion_POI)
TCC_DAM_Monthly$revenue <- TCC_DAM_Monthly$mwh_calc*TCC_DAM_Monthly$congest_diff
TCC_DAM_Monthly$cost <- TCC_DAM_Monthly$mwh_calc*TCC_DAM_Monthly$`Market Clr Price`
TCC_DAM_Monthly$profit <- TCC_DAM_Monthly$revenue - TCC_DAM_Monthly$cost



###  Returns  #####################
# Return(i) = [Profit(i) / absolute|MarketClearingPrice(i)|] * 100%

## Return assuming that -contracts required payment up front
TCC_DAM_Monthly$abs_mktprice <- abs(TCC_DAM_Monthly$cost)
# Need to save account for $0.00 market clearing price
# Change 0.00 to 0.001
TCC_DAM_Monthly$abs_mktprice[TCC_DAM_Monthly$abs_mktprice == 0.00] <- 0.01 
#summary(TCC_DAM_Monthly$abs_mktprice)
TCC_DAM_Monthly$return <- (TCC_DAM_Monthly$profit/TCC_DAM_Monthly$abs_mktprice)*100
#summary(TCC_DAM_Monthly$return)

## TCC Return minus T_Bill
TCC_DAM_Monthly$excess_return <- TCC_DAM_Monthly$return-TCC_DAM_Monthly$GS1M #Return minus risk free rate of return

# summary(TCC_DAM_Monthly$excess_return) #this is a percent!
# prettyNum(mean(TCC_DAM_Monthly$excess_return), big.mark = ",") #this is a percent!
# prettyNum(mean(TCC_DAM_Monthly$return), big.mark = ",") #seems high
# prettyNum(sum(TCC_DAM_Monthly$GS1M), big.mark = ",")
# prettyNum(sum(TCC_DAM_Monthly$profit), big.mark = ",")
# prettyNum(sum(TCC_DAM_Monthly$cost), big.mark = ",")
# 
# plot(TCC_DAM_Monthly$excess_return, ylim = c(-1000, 1000))
# hist(TCC_DAM_Monthly$excess_return, breaks = 10, ylim = c(-10, 1000))

####
#sum(TCC_DAM_Monthly$profit)/sum(TCC_DAM_Monthly$cost) # Quick check, around 48% market return...
allocated_TCC <- subset(TCC_DAM_Monthly, allocated >0)
market_TCC <- subset(TCC_DAM_Monthly, allocated <1)
summary(allocated_TCC$profit, na.rm = TRUE)
summary(market_TCC$profit, na.rm = TRUE)



###  Returns by Path  #############

TCC_Path_Returns <- TCC_DAM_Monthly %>%
        group_by(year, month, `POI ID`, `POW ID`) %>%
        summarize(Path_Revenue = sum(revenue),
                  Path_Cost = sum(cost),
                  Path_MWh_calc = sum(mwh_calc),
                  Path_Profits = sum(profit),
                  Path_abs_mktprice = sum(abs_mktprice),
                  Path_Returns = sum(return),
                  Path_excess_return = sum(excess_return),
                  List_Holders = list(`Primary Holder`)
                  )

#TCC_Path_Returns$List_Holders[2] #Check names in list
# summary(TCC_Path_Returns$Path_excess_return)
# summary(TCC_Path_Returns$Path_Profits)
# hist(TCC_Path_Returns$Path_excess_return, breaks = 100)
# hist(TCC_Path_Returns$Path_Profits, breaks = 500)


# prettyNum(median(TCC_Path_Returns$Path_excess_return), big.mark = ",") #percent
# prettyNum(sum(TCC_Path_Returns$Path_Profits), big.mark = ",")
# 
# 
# prettyNum(mean(x$Path_excess_return), big.mark = ",")
# prettyNum(sum(x$Path_Revenue), big.mark = ",")
# prettyNum(sum(x$Path_Cost), big.mark = ",")
# prettyNum(sum(x$Path_Profits), big.mark = ",")


holder_return <- TCC_DAM_Monthly %>%
        group_by(`Primary Holder`, year) %>%
        summarise(Total_profit = sum(profit), 
                  Mean_excess_return = mean(excess_return),
                  Median_excess_return = median(excess_return))


### Exporatory Plots #####
plot_holder <- ggplot(holder_return,
                      aes(x = year, y = Total_profit, color = `Primary Holder`)) +
        geom_line(alpha = 0.3) +
        geom_hline(yintercept = 0, color = "black") +
        scale_y_continuous(labels = comma) +
        labs(
                title = "Profits by Market Participant, Monthly TCC from 2010-2016",
                subtitle = "Each line represents a single market participants returns over time.",
                y = "Total Profit",
                x = ""
        ) +
        theme_tufte() +
        theme(legend.position = "none")
plot_holder
#ggMarginal(plot_holder, type = "density", margins = "y", color = "light grey") # Add density plot on for profit
plot_density_holder <- ggplot(holder_return, aes(Total_profit)) +
        geom_density(fill = "black", alpha = 0.1) +
        geom_vline(xintercept = 0, color = "dodger blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        labs(
                title = "Density Plot, Monthly Profits for Market Participants",
                subtitle = "Non-Truncated Density Plot",
                x = "Monthly Holder Returns"
        ) +
        theme_tufte() 
plot_density_holder




### Density plot TCC
plot_density <- ggplot(TCC_DAM_Monthly, aes(profit)) +
        geom_density(fill = "black", alpha = 0.1) +
        geom_vline(xintercept = 0, color = "dodger blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        labs(
                title = "Density Plot, Individual TCC Profit",
                subtitle = "Non-Truncated Density Plot",
                x = "TCC Profit"
        ) +
        theme_tufte() 
plot_density

# Save plot
svg(filename="plot_density.svg",
    width=8,
    height=2,
    pointsize=12) # SVG setting for antialiasing
plot_density #call plot to save as svg
dev.off() #turn off sVG settings


plot_density_cut <- ggplot(TCC_DAM_Monthly, aes(profit)) +
        geom_density(fill = "black", alpha = 0.1) +
        geom_vline(xintercept = 0, color = "dodger blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        coord_cartesian(xlim = c(-1000000, 1000000)) +
        labs(
                title = "Density Plot, Individual TCC Profit",
                subtitle = "X Axis Truncated to -$1mil and $1mil. Profits Exist Greater than $1mil.",
                x = "TCC Profit"
        ) +
        theme_tufte() 
plot_density_cut 

# Save plot        
svg(filename="plot_density_truncate.svg",
    width=6,
    height=4,
    pointsize=12)
plot_density_cut
dev.off()


plot_profit_truncated <- ggplot(TCC_DAM_Monthly, 
                      aes(x = date.x, y = profit, color = winter_month)
) + 
        geom_jitter(alpha = 0.1, width = 10) +
        geom_rangeframe(sides = "l", alpha = 0.2) +
        geom_hline(yintercept = 0, alpha = 0.2) +
        scale_y_continuous(labels = comma) +
        scale_color_manual(values = c("black", "blue")) +
        labs(
                title = "NYISO Monthly TCC Profits",
                subtitle = "Truncated to +/- $1,000,000 with points jittered horizontally",
                x = "",
                y = "$ Profit") +
        coord_cartesian(ylim = c(-1000000, 1000000)) +
        theme_tufte() 
plot_profit_truncated
# ggMarginal(plot_profit, type = "density", margins = "y", color = "light grey") # Add density plot on for profit







#### Stargazer results
TCC_DAM_Monthly_table <- as.data.frame(TCC_DAM_Monthly) #convert to data frame to play nicely with stargazer
stargazer(TCC_DAM_Monthly_table, type = "html", median = TRUE, digits = 2, out = "TCC_DAM_table.html") 
stargazer(T_Bill, type = "html", out = "t_bill_table.html")
stargazer(holder_return)
str(TCC_DAM_Monthly_table)
str(T_Bill)

summary(TCC_monthly_contracts)
summary(TCC_DAM_Monthly)
x2 <- TCC %>% 
        filter(`Start Date` >= ymd("2010-1-1") & `End Date` <= ymd("2016-12-31"))
