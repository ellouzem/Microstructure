## import packages
library("readr")
library("dplyr")

## Load data
table = read_csv(file = "data_final_2019.csv") 

param = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1")
param_rf = c("return_rf", "beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1", "log_btm_t1", "fsrv_t1", "turnover_t1", "illiq_amihud_t1")
n_param = length(param)

### Summary statistics

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
  colnames(gamma) <- c("alpha", param)
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
    g = na.omit(gamma[,i])
    if (length(g) > 0)
    {
      t_test = t.test()
      #cat(paste(colnames(gamma)[i], ": "), t_test$p.value, "\n")
      var = colnames(gamma)[i]
      result[1, var] = t_test$p.value
      result[2, var] = t_test$estimate
    }
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

for (i in 5:n_param)
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





######################################################
######Matrice de Correlation 2 a 2 ###################

CorrCouple <- function(param1, param2)
{

  result = matrix(nrow = 2, ncol=1)
  rownames(result) = c("p-value", "correlation")
  colnames(result) = paste( c(param1,param2) , collapse=" % ")

  v1 = unlist( subset(table[param1], table[param1]!="NA" & table[param2]!="NA") )
  v2 = unlist( subset(table[param2], table[param1]!="NA" & table[param2]!="NA") )
  test = cor.test( v1 , v2, method=c("pearson", "kendall", "spearman"))
  
  result[1,1] = unlist(test[3])
  result[2,1] = unlist(test[4])
  
  hello = c(unlist(test[3]), unlist(test[4]))

  return(hello)
}

CorrCouple("return_rf", "beta_t1")

MatrixCorr <- function(param){
  n = length(param)
  
  p_val_matrix = matrix(nrow = n, ncol=n)
  corr_matrix = matrix(nrow = n, ncol=n)
  rownames(p_val_matrix) = param
  rownames(corr_matrix) = param
  colnames(p_val_matrix) = param
  colnames(corr_matrix) = param
  
  for (i in 1:n)
  {
    for (j in i:n){
      res = CorrCouple(param[i],param[j])
      p_val_matrix[i,j] = res[1]
      corr_matrix[i,j] = res[2]
      cat("i= ",i," j= ",j, "\n")
    }
  }
  L = list("p_values" = p_val_matrix, "corr"= corr_matrix)

  return(L)
}
L = MatrixCorr(param)

#execute L$p_values and L$corr


######################################################
###### Matrice d'infos  générales ###################

MatrixInfos <- function(param){
  n = length(param)
  result = matrix(nrow=n, ncol=4)
  rownames(result) = param
  colnames(result) = c("min", "max", "mean", "median")
  
  for(i in 1:n){
    vect = unlist( subset(table[param[i]], table[param[i]]!="NA" ) )
    result[i,1] = min(vect)
    result[i,2] = max(vect)
    result[i,3] = mean(vect)
    result[i,4] = median(vect)
  }
  return(result)
}

Infos = MatrixInfos(param)

