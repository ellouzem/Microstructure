## import packages
library("readr")
library("dplyr")

## Load data
table = read_csv(file = "data_final_2019.csv") 

param = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1", "Marketretrun", "RiskFreeReturn", "ExcessReturn")
n_param = length(param)

### Calcul des corrélations



### Regression

regression <- function(param)
{
  n_param = length(param)
  gamma = c()
  years = sort(unique(table$year))
  for (y in years)
  {
    months = sort(unique((table$month[table$year == y])))
    n_months = length(months)
    for (m in months[2:n_months])
    {
      model = as.formula(paste("return_rf ~ ",  paste(param, collapse="+"))) 
      if (!("pin_t1" %in% param))
      {
        subtable = subset(table, table$year == y & table$month == m)
        fit = lm(model, data=subtable)
      }
      else
      {
        subtable = subset(table, table$year == y & table$month == m & !is.na(table$pin_t1))
        if (length(subtable$return_rf) > 0)
        {
          fit = lm(model, data=subtable)
        }
        else
        {
          next
        }
      }
      gamma <- rbind(gamma, fit$coefficients)
    }
  }
  return(gamma)
}

  # Test de Fama et MacBeth

MacBeth <- function(gamma, param)
{
  n_param = ncol(gamma)
  result = matrix(nrow = 2, ncol=length(param))
  rownames(result) = c("p-value", "gamma")
  colnames(result) = param
  #cat("   -- p-values --\n")
  for (i in 2:n_param)
  {
    t_test = t.test(na.omit(gamma[,i]))
    #cat(paste(colnames(gamma)[i], ": "), t_test$p.value, "\n")
    var = colnames(gamma)[i]
    result[1, var] = t_test$p.value
    result[2, var] = t_test$estimate
  }
  return(result)
}



### Initialisation

p_values = matrix(nrow=1, ncol=n_param)
gammas = matrix(nrow=1, ncol=n_param)
#lignames = c()
colnames(p_values) = param
colnames(gammas) = param

### Regression avec seulement beta

gamma = regression(c("beta_t1"))
result = MacBeth(gamma, param)
p_values[1, ] = result[1, ]
gammas[1, ] = result[2, ]
#lignames = c(lignames, "beta_t1")


### Regression avec beta + AIM

modele = c("beta_t1", "AIM_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1")

### Regression avec beta + AIM + PIN
modele = c("beta_t1", "AIM_t1", "pin_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1 + pint_t1")

### Regression avec beta + AIM + SIZE

modele = c("beta_t1", "AIM_t1", "log_market_cap_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])


### Regression avec beta + AIM + SIZE + PIN

modele = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1")
gamma = regression(modele)
result = MacBeth(gamma, param)
p_values = rbind(p_values, result[1, ])
gammas = rbind(gammas, result[2, ])

#lignames = c(lignames, "beta_t1 + AIM_t1 + log_market_cap_t1 + pint_t1")


### Regression en rajoutant le reste

for (i in 5:(n_param-3))
{
  modele = param[1:i]
  print(modele)
  gamma = regression(modele)
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  #lignames = c(lignames, paste(model, collapse=" + "))
}

#rownames(result) = lignames

optim = which.min(p_values[,"AIM_t1"])
p_values[optim,]
