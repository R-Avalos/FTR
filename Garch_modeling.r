######  GARCH Modeling of Data  #############################################
# GARCH helps model volatility clustering. As the power grid is inteconnected, a Locational marginal price spike will effect nearby lmp prices. There are periods of relative calm and periods of high volatility (see winter storm knocking out power lines) 
# Long tail returns
# Ljung Box text to see if there is auto correlation (we don't want that)

rm(list = ls()) #Clear Workspace

library(rugarch)
library(tidyverse)
library(reshape2)
library(RPostgreSQL)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)

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


### Load Data ####
# dbListTables(con)
TCC_Path <- dbGetQuery(con, "SELECT * FROM paths") # 10,532 unique paths
summary(TCC_Path)

x <- dbGetQuery(con, "SELECT * FROM monthly_path_returns")
PretyNum(sum(x$path_profits_mo), comma)
prettyNum(sum(x$path_profits_mo), big.mark = ",")
remove(x)

# TCC_Path_6mo <- TCC_Path %>%
#         filter(Months_Active >= 6) # 2,428 with observations >= 6 months
# remove(TCC_Path) # clean up
#remove(TCC_Path_6mo)

#### Select data with over 100 days of observations
TCC_Path_4mo <- TCC_Path %>%
        filter(Months_Active >= 4) # 3630 with observations greater than 4 months
remove(TCC_Path) # clean up


target_paths <- TCC_Path_4mo$Path_Name
target_paths_sample <- sample(target_paths, 150) # random sample for test runs
remove(TCC_Path_4mo, TCC_Path_6mo)

# dbListFields(con, "transformed_data")
TCC_Path_Returns <- dbGetQuery(con, "SELECT * FROM transformed_data") #load from local db

## Subset to targeted paths
Test_df_all2 <- TCC_Path_Returns %>%
        filter(path_name %in% target_paths_sample) 

# test_one_group <- Test_df_all %>%
#         filter(path_name == "23512_to_24155")

#str(Test_df_2016)
summary(Test_df_all)
remove(TCC_Path_Returns) #clean house


### Transform Data ####

#pad_diff_func <- function(x, n = 1){ c(rep(0L, n), diff(x, lag = n))}
#TCC_Path_Returns_2$date_diff <- pad_diff_func(TCC_Path_Returns_2$date)

### Model Data ####
Garch_model_function <- function(dataframe){
        spec_test <- ugarchspec(variance.model = list(model = "sGARCH",
                                                      garchOrder = c(1, 1),
                                                      submodel = NULL,
                                                      external.regressors = NULL,
                                                      variance.targeting = FALSE),
                                mean.model = list(
                                        armaOrder= c(0,0),
                                        include.mean = TRUE,
                                        external.regressors = matrix(dataframe[,4]),
                                                  start.pars = list(),
                                                  fixed.pars = list())
                                )
        garch_fit_test <- ugarchfit(spec = spec_test,
                                              data = dataframe[,3],
                                              solver = 'hybrid')
        garch_fit_test
} # create model to run over list of groups

################# Run Model on all selected data ####

test_model1 <- Test_df_all1 %>%
        group_by(path_name) %>%
        do(garhc_fit = tryCatch(Garch_model_function(as.data.frame(.)), error=function(err) NA)) # run model for each path



# test_model2 <- Test_df_all2 %>%
#         group_by(path_name) %>%
#         do(garhc_fit = tryCatch(Garch_model_function(as.data.frame(.)), error=function(err) NA)) # run model for each path
# 
# x <- Test_df_all2 %>%
#         filter(path_name == "23514_to_23652")

# test_model2$garhc_fit[[1]]
# test_model2$garhc_fit[[1]]@fit$matcoef
# x <- as.data.frame(test_model$garhc_fit[[5]]@fit$matcoef)
# showClass("GARCHtests")
# nyblom(test_model$garhc_fit[[1]])
# signbias(test_model$garhc_fit[[1]])
# infocriteria(test_model$garhc_fit[[1]])
# plot(test_model$garhc_fit[[1]])
sum(is.na(test_model$garhc_fit))
# is.na(test_model$garhc_fit)
test_model$na <- is.na(test_model$garhc_fit) # find models that did not sovle
summary(test_model$na)

test_model <- test_model %>%
        filter(na == FALSE) #removes models that did not solve


### Create function to store results in data frame ####
# Pull out results for models and save to data frame with associated path
model_result_function <- function(x) {
        model_list <- list() # create a list to store files within
        
        for(i in 1:length(x$garhc_fit)){
                model_1 <- as.data.frame(x$garhc_fit[[i]]@fit$matcoef)
                model_1$path <- x$path_name[i]
                model_1$value <- rownames(model_1)
                model_list[[i]] <- model_1
                remove(model_1)
                
        }
        model_results <- do.call(rbind, model_list) #rowbind the dataframes in the list
        print(paste0("All ", length(x$garhc_fit), 
                     " models results combined into data frame"))
        return(model_results)
}

### Use function to place results into data frame ####
test_models_results <- model_result_function(test_model)
# summary(test_models_results)
rownames(test_models_results) <- NULL #removes unncessary rownames


#### Subset to statistically significant results #### 
# library(reshape2)
# x <- melt(test_models_results, id.vars = c("path"), variable.name = "value")

model_results <- test_models_results %>%
        group_by(path) %>%
        summarize(mu = ` Estimate`[value == "mu"],
                  mu_p = `Pr(>|t|)`[value == "mu"],
                  alpha1 = ` Estimate`[value == "alpha1"],
                  alpha1_p = `Pr(>|t|)`[value == "alpha1"],
                  beta1 = ` Estimate`[value == "beta1"],
                  beta1_p = `Pr(>|t|)`[value == "beta1"],
                  ar1 = ` Estimate`[value == "ar1"],
                  ar1_p = `Pr(>|t|)`[value == "ar1"],
                  ma1 = ` Estimate`[value == "ma1"],
                  ma1_p = `Pr(>|t|)`[value == "ma1"],
                  mxreg1 = ` Estimate`[value == "mxreg1"],
                  mxreg1_p = `Pr(>|t|)`[value == "mxreg1"],
                  omega = ` Estimate`[value == "omega"],
                  omega_p = `Pr(>|t|)`[value == "omega"]
                                    )

summary(model_results)

#### Jensen's Alpha #############
# Jensen_alpha = avg_path_return - beta_reg*average_market
# j_alpha = mu - mxreg1(avg_return_mkt)

model_results_filter <- model_results %>%
        group_by(path) %>%
        filter(mxreg1_p <= 0.05 & mu_p <= 0.05)



avg_market_returns <- TCC_Path_Returns %>%
        group_by(path_name) %>%
        summarize(mkt_mean = mean(mkt_excess_percent_return))

avg_path_returns <- TCC_Path_Returns %>%
        group_by(path_name) %>%
        summarize(path_mean = mean(tcc_excess_percent_return))

model_results_ja <- left_join(model_results_filter, avg_market_returns, by = c("path" = "path_name"))

model_results_ja <- left_join(model_results_ja, avg_path_returns, by = c("path" = "path_name"))


model_results_ja$mktB <- model_results_ja$beta1*model_results_ja$mkt_mean

model_results_ja$j_alpha <- model_results_ja$mu - model_results_ja$mktB

hist(model_results_ja$j_alpha)
hist(model_results_ja$mxreg1)

summary(model_results_ja)
summary(avg_path_returns)

model_results_ja$Alpha <- NA
model_results_ja$Alpha[model_results_ja$j_alpha>0] <- ">0"
model_results_ja$Alpha[model_results_ja$j_alpha<0] <- "<0"
model_results_ja$Alpha[model_results_ja$j_alpha==0] <- "=0"

mean(TCC_Path_Returns$tcc_excess_percent_return)
mean(model_results_ja$path_mean)

#### Plots #####
systemic_plot <- ggplot(model_results_ja, aes(x = mxreg1, y = path_mean)) +
        geom_point(aes(color = Alpha), alpha = 0.5) +
        geom_hline(yintercept = 0, alpha = 0.5) +
        geom_vline(xintercept = 0, alpha = 0.5) +
        xlab("Systemic Risk, Beta") +
        ylab("Average Daily % Excess Return") +
        geom_smooth(method = lm) +
        scale_fill_manual(values = c("red", "dodger blue")) +
        theme_tufte() +
        ggtitle(label = "TCC Return by Systemic Risk, 2010-2016")
systemic_plot


systemic_plot_trunc <- ggplot(model_results_ja, aes(x = mxreg1, y = mu, color = Alpha)) +
        geom_point(alpha = 0.5) +
        geom_hline(yintercept = 0, alpha = 0.5) +
        geom_vline(xintercept = 0, alpha = 0.5) +
        xlab("Systemic Risk, Beta") +
        ylab("Average Daily % Excess Return") +
        geom_smooth(method = lm) +
        scale_fill_manual(values = c("red", "dodger blue")) +
        theme_tufte() +
        coord_cartesian(ylim = c(-1000, 1000),
                        xlim = c(-10, 10)) +
        ggtitle(label = "TCC Return by Systemic Risk, 2010-2016", 
                subtitle = "Truncated to +/- 10,000% return and +/- 10 Beta")
systemic_plot_trunc

summary(model_results_ja$j_alpha)
### Desnity Plot Betas
plot_density_beta <- ggplot(model_results_ja, aes(mxreg1)) +
        geom_density(fill = "black", alpha = 0.1) +
        geom_vline(xintercept = 0, color = "dodger blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        labs(
                title = "Density Plot, TCC Beta",
                subtitle = "Systemic Risk, Non-Truncated Density Plot",
                x = "Beta"
        ) +
        theme_tufte() 
plot_density_beta

plot_density_beta_trunc <- ggplot(model_results_ja, aes(mxreg1)) +
        geom_density(fill = "black", alpha = 0.1) +
        geom_vline(xintercept = 0, color = "dodger blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        labs(
                title = "Density Plot, TCC Beta",
                subtitle = "Systemic Risk, Truncated Density Plot",
                x = "Beta"
        ) +
        coord_cartesian(xlim = c(-5, 5)) +
        theme_tufte() 
plot_density_beta_trunc



plot_density_excessTCC_trunc <- ggplot(TCC_Path_Returns, aes(tcc_excess_percent_return)) +
        geom_density(fill = "black", alpha = 0.1) +
        geom_vline(xintercept = 0, color = "dodger blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        labs(
                title = "Density Plot, TCC Beta",
                subtitle = "Systemic Risk, Truncated Density Plot",
                x = "Beta"
        ) +
        coord_cartesian(xlim = c(-50000, 5000000000)) +
        theme_tufte() 
plot_density_excessTCC_trunc


plot_density_alpha_trunc <- ggplot(model_results_ja, aes(j_alpha)) +
        geom_density(fill = "dark green", alpha = 0.5) +
        geom_vline(xintercept = 0, color = "blue", alpha = 0.75) +
        scale_x_continuous(labels = comma) +
        labs(
                title = "Density Plot, TCC Alpha",
                subtitle = "Jensen's Alpha = 0, Truncated Density Plot",
                x = "TCC Alpha"
        ) +
        coord_cartesian(xlim = c(-5000, 5000)) +
        theme_tufte() 
plot_density_alpha_trunc


hist(model_results_ja$path_mean)
summary(TCC_Path_Returns)

summary(model_results_ja$mxreg1)
summary(model_results_ja$mkt_mean)
# scale_fill_manual(values = c("green", "black", "red"))

####
library(stargazer)

stargazer(daily_treasury, median = TRUE, type = "html", out = "t_bill_table.html")

write.csv(x = model_results, file = "model_results.csv")
write.csv(x = model_results_ja, file = "model_results_j.csv")
