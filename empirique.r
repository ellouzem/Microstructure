## import packages
library("readr")
library("dplyr")
library("weights")
source("functions.R")

## Load data
table = read_csv(file = "data_final_2019.csv") 

param = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1")
param_rf = c("return_rf", "beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1")
n_param = length(param)

### Summary statistics

Infos = MatrixInfos(param_rf)
print(Infos)
#print(xtable(Infos, type = "latex"), file = "stat.tex")


### Summary correlations

L = MatrixCorr(param_rf)
print(L$p_values)
print(L$corr)
#print(xtable(L$corr, type = "latex"), file = "corr.tex")

### Regresion avec MAcBeth

  # Initialisation

p_values = matrix(nrow=1, ncol=n_param)
gammas = matrix(nrow=1, ncol=n_param)
#lignames = c()
colnames(p_values) = param
colnames(gammas) = param

  # Regression avec seulement beta

gamma = regression(c("beta_t1"))
result = MacBeth(gamma, param)
p_values[1, ] = result[1, ]
gammas[1, ] = result[2, ]
#lignames = c(lignames, "beta_t1")

  # Regression avec seulement AIM

gamma = regression(c("AIM_t1"))
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])
#lignames = c(lignames, "beta_t1")

# Regression avec seulement PIN

gamma = regression(c("pin_t1"))
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])
#lignames = c(lignames, "beta_t1")

# Regression avec seulement SIZE

gamma = regression(c("log_market_cap_t1"))
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])
#lignames = c(lignames, "beta_t1")


  # Regression avec beta + AIM

modele = c("beta_t1", "AIM_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1")

  # Regression avec beta + AIM + PIN

modele = c("beta_t1", "AIM_t1", "pin_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1 + pint_t1")

  # Regression avec beta + AIM + SIZE

modele = c("beta_t1", "AIM_t1", "log_market_cap_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])


  # Regression avec beta + AIM + SIZE + PIN

modele = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1 + log_market_cap_t1 + pint_t1")


  # Regression en rajoutant le reste

for (i in 5:n_param)
{
  modele = param[1:i]
  gamma = regression(modele)
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  #lignames = c(lignames, paste(model, collapse=" + "))
}

#rownames(result) = lignames
print(gammas)
print(p_values)


optim = which.min(p_values[,"AIM_t1"])
p_values[optim,]



### Regression avec L-R

  # Initialisation

p_values2 = matrix(nrow=1, ncol=n_param)
gammas2 = matrix(nrow=1, ncol=n_param)
#lignames = c()
colnames(p_values2) = param
colnames(gammas2) = param

  # Regression avec seulement beta

result = regression_LR(c("beta_t1"))
gamma = result$gamma
errors = result$errors
result2 = LR(gamma, errors, param)

p_values2[1, ] = result2[1, ]
gammas2[1, ] = result2[2, ]


  # Regression avec seulement beta

result = regression_LR(c("AIM_t1"))
gamma = result$gamma
errors = result$errors
result2 = LR(gamma, errors, param)

p_values2 = rbind(p_values2, result2[1, ])
gammas2 = rbind(gammas2, result2[2, ])



  # Regression avec beta + AIM

modele = c("beta_t1", "AIM_t1")
result = regression_LR(modele)
gamma = result$gamma
errors = result$errors
result2 = LR(gamma, errors, param)

p_values2 = rbind(p_values2, result2[1, ])
gammas2 = rbind(gammas2, result2[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1")

  # Regression avec beta + AIM + PIN

modele = c("beta_t1", "AIM_t1", "pin_t1")
result = regression_LR(modele)
gamma = result$gamma
errors = result$errors
result2 = LR(gamma, errors, param)

p_values2 = rbind(p_values2, result2[1, ])
gammas2 = rbind(gammas2, result2[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1 + pint_t1")

  # Regression avec beta + AIM + SIZE

modele = c("beta_t1", "AIM_t1", "log_market_cap_t1")
result = regression_LR(modele)
gamma = result$gamma
errors = result$errors
result2 = LR(gamma, errors, param)

p_values2 = rbind(p_values2, result2[1, ])
gammas2 = rbind(gammas2, result2[2, ])


  # Regression avec beta + AIM + SIZE + PIN

modele = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1")
result = regression_LR(modele)
gamma = result$gamma
errors = result$errors
result2 = LR(gamma, errors, param)

p_values2 = rbind(p_values2, result2[1, ])
gammas2 = rbind(gammas2, result2[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1 + log_market_cap_t1 + pint_t1")


  # Regression en rajoutant le reste

for (i in 5:n_param)
{
  modele = param[1:i]
  result = regression_LR(modele)
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  #lignames = c(lignames, paste(model, collapse=" + "))
}

#rownames(result) = lignames

print(gammas2)
print(p_values2)

optim2 = which.min(p_values[,"AIM_t1"])
p_values[optim2,]





