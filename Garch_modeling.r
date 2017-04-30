# GARCH Modeling of Data


# GARCH helps model volatility clustering. As the power grid is inteconnected, a Locational marginal price spike will effect nearby lmp prices. There are periods of relative calm and periods of high volatility (see winter storm knocking out power lines) 

# Long tail returns
# Ljung Box text to see if there is auto correlation (we don't want that)
#install.packages("tidyverse")
#install.packages("rugarch")
rm(list = ls()) #Clear Workspace

library(rugarch)
library(tidyverse)
library(reshape2)
library(RPostgreSQL)
library(ggplot2)
library(lubridate)

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

TCC_Path_6mo <- TCC_Path %>%
        filter(Months_Active >= 6) # 2,428 with observations >= 6 months
remove(TCC_Path) # clean up
#remove(TCC_Path_6mo)

TCC_Path_4mo <- TCC_Path %>%
        filter(Months_Active >= 4) # 3630 with observations greater than 4 months
remove(TCC_Path) # clean up


target_paths <- TCC_Path_4mo$Path_Name
target_paths_sample <- sample(target_paths, 150) # random sample for test runs
remove(TCC_Path_4mo, TCC_Path_6mo)

# dbListFields(con, "transformed_data")
TCC_Path_Returns <- dbGetQuery(con, "SELECT * FROM transformed_data") #load from local db

## Subset to targeted paths
Test_df_all <- TCC_Path_Returns %>%
        filter(path_name %in% target_paths_sample) # sampele 

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
                                mean.model = list(external.regressors = matrix(dataframe[,4]),
                                                  start.pars = list(),
                                                  fixed.pars = list())
                                )
        garch_fit_test <- ugarchfit(spec = spec_test,
                                              data = dataframe[,3],
                                              solver = 'hybrid')
        garch_fit_test
} # create model to run over list of groups

################# Run Model on all selected data ####

test_model <- Test_df_all %>%
        group_by(path_name) %>%
        do(garhc_fit = tryCatch(Garch_model_function(as.data.frame(.)), error=function(err) NA)) # run model for each path


x <- as.data.frame(test_model$garhc_fit[[5]]@fit$matcoef)
x
# x$path <- test_model$path_name
# x
#showClass("GARCHtests")
#nyblom(test_model$garhc_fit[[1]])
#signbias(test_model$garhc_fit[[1]])
#infocriteria(test_model$garhc_fit[[1]])
#plot(test_model$garhc_fit[[1]])
sum(is.na(test_model$garhc_fit))
is.na(test_model$garhc_fit)
test_model$na <- is.na(test_model$garhc_fit)
summary(test_model$na)

test_model <- test_model %>%
        filter(na == FALSE) #removes NAs


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

model_results_filter <- model_results %>%
        group_by(path) %>%
        filter(alpha1_p <= 0.05 & mxreg1_p <= 0.05 )


systemic_plot <- ggplot(model_results_filter, aes(x = mxreg1, y = mu, color = ma1)) +
        geom_point() +
        xlab("Systemic Risk, Beta") +
        ylab("Average Daily Return, $") +
        geom_smooth(method = lm)

systemic_plot



####

