## import packages
library(readr)
library(dplyr)
library(plm)

## Load data
table = read_csv(file = "data_final_2019.csv") 

param = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1", "Marketreturn", "RiskFreeReturn", "ExcessReturn")


### Calcul des corrélations



### Regression avec seulement beta

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
        fit = lm(model, data=table, subset=(table$year == y & table$month == m))
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

p_values <- function(gamma)
{
  n_param = ncol(gamma)
  cat("   -- p-values --\n")
  for (i in 2:n_param)
  {
    t_test = t.test(gamma[,i])
    cat(paste(colnames(gamma)[i], ": "), t_test$p.value, "\n")
  }
}

gamma = regression(c("beta_t1"))
p_values(gamma)

### Regression avec beta + AIM

gamma = regression(c("beta_t1", "AIM_t1"))
p_values(gamma)

### Regression avec beta + AIM + PIN

gamma = regression(c("beta_t1", "AIM_t1", "pin_t1"))
p_values(gamma)

### Regression avec beta + AIM + SIZE

gamma = regression(c("beta_t1", "AIM_t1", "log_market_cap_t1"))
p_values(gamma)

### Regression avec beta + AIM + SIZE + PIN

gamma = regression(c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1"))
p_values(gamma)



