### import packages ###

library("readr") # install.packages("readr")
library("dplyr") # install.packages("dplyr")
library("weights") # install.packages("weights")
source("functions.R") 

### Load data ###

table = read_csv(file = "data_final_2019.csv") 

### List of parameters ###

param = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1")
param_rf = c("return_rf", "beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1")
n_param = length(param)

### Summary statistics ###

Infos = MatrixInfos(param_rf)
print(Infos)

### Summary correlations ###

Corr = MatrixCorr(param_rf)
print(Corr$p_values)
print(Corr$corr)

### Regresion with Fama-MacBeth ###

cat("### Regresion with Fama-MacBeth ###")
result1 = regression_table_FMB(param, n_param)
print(result1$gammas)
print(result1$p_values)

optim1 = which.min(result1$p_values[,"AIM_t1"])
result1$p_values[optim1,]

### Regression with L-R ###

result2 = regression_table_LR(param, n_param)
print(result2$gammas)
print(result2$p_values)

optim2 = which.min(result2$p_values[,"AIM_t1"])
result2$p_values[optim2,]





