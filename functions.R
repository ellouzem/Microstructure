#####################
##### Functions #####
#####################

### Summary statistics

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


### Summary correlations 

##########################################
###### Matrice de Correlation 2 a 2 ######

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
  
  L = c(unlist(test[3]), unlist(test[4]))
  
  return(L)
}

MatrixCorr <- function(param){
  n = length(param)
  
  p_val_matrix = matrix(nrow = n, ncol=n)
  corr_matrix = matrix(nrow = n, ncol=n)
  rownames(p_val_matrix) = param
  rownames(corr_matrix) = param
  colnames(p_val_matrix) = param
  colnames(corr_matrix) = param
  pb = txtProgressBar(min=0, max=n*n/2, style = 3)
  for (i in 1:n)
  {
    for (j in i:n){
      res = CorrCouple(param[i],param[j])
      p_val_matrix[i,j] = res[1]
      corr_matrix[i,j] = res[2]
      setTxtProgressBar(pb, (i*n+j)/2)
    }
  }
  L = list("p_values" = p_val_matrix, "corr"= corr_matrix)
  
  return(L)
}


### Regression Fama MacBeth

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

# Test of Fama MacBeth

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
      t_test = t.test(g)
      #cat(paste(colnames(gamma)[i], ": "), t_test$p.value, "\n")
      var = colnames(gamma)[i]
      result[1, var] = t_test$p.value
      result[2, var] = t_test$estimate
    }
  }
  return(result)
}

# Return gammas and p-values of different regression models

regression_table_FMB <- function(param, n_param, param2)
{
  # Initialization
  p_values = matrix(nrow=1, ncol=n_param)
  gammas = matrix(nrow=1, ncol=n_param)
  #lignames = c()
  colnames(p_values) = param
  colnames(gammas) = param
  pb = txtProgressBar(min=0, max=16, style = 3)
  
  # Regression with only beta
  gamma = regression(c("beta_t1"))
  result = MacBeth(gamma, param)
  p_values[1, ] = result[1, ]
  gammas[1, ] = result[2, ]
  setTxtProgressBar(pb, 1)
  
  # Regression with only AIM
  gamma = regression(c("AIM_t1"))
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 2)
  
  # Regression with only PIN
  gamma = regression(c("pin_t1"))
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 3)
  
  # Regression with only SIZE
  gamma = regression(c("log_market_cap_t1"))
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 4)
  
  # Regression with beta + AIM
  modele = c("beta_t1", "AIM_t1")
  gamma = regression(modele)
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 5)
  
  # Regression with beta + AIM + PIN
  modele = c("beta_t1", "AIM_t1", "pin_t1")
  gamma = regression(modele)
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 6)
  
  # Regression avec beta + AIM + SIZE
  modele = c("beta_t1", "AIM_t1", "log_market_cap_t1")
  gamma = regression(modele)
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 7)
  
  # Regression avec beta + AIM + SIZE + PIN
  modele = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1")
  gamma = regression(modele)
  result = MacBeth(gamma, param)
  p_values = rbind(p_values, result[1, ])
  gammas = rbind(gammas, result[2, ])
  setTxtProgressBar(pb, 8)
  
  # Regression with the others
  k = 9
  for (i in 5:n_param)
  {
    modele = param[1:i]
    gamma = regression(modele)
    result = MacBeth(gamma, param)
    p_values = rbind(p_values, result[1, ])
    gammas = rbind(gammas, result[2, ])
    setTxtProgressBar(pb, k)
    k = k+1
  }
  
  for (i in 4:(n_param-1))
  {
    modele = param2[1:i]
    gamma = regression(modele)
    result = MacBeth(gamma, param)
    p_values = rbind(p_values, result[1, ])
    gammas = rbind(gammas, result[2, ])
    setTxtProgressBar(pb, k)
    k = k+1
  }
  
  L = list("p_values" = p_values, "gammas"= gammas)
  return(L)
}


### Regression L-R

regression_LR <- function(param)
{
  n_param = length(param)
  gamma = c()
  errors = c()
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
      errors = rbind(errors, summary(fit)$coef[, "Std. Error"])
      gamma <- rbind(gamma, fit$coefficients)
    }
  }
  #colnames(gamma_pond) <- c("alpha", param)
  L = list("gamma" = gamma, "errors"= errors)
  return(L)
}

  # Test of L-R

LR <- function(gamma, errors, param)
{
  n_param = ncol(gamma)
  result = matrix(nrow = 2, ncol=length(param))
  rownames(result) = c("p-value", "gamma")
  colnames(result) = param
  #cat("   -- p-values --\n")
  for (i in 2:n_param)
  {
    g = na.omit(gamma[,i])
    w = 1/errors[,i]
    if (length(g) > 0)
    {
      t_test = wtd.t.test(g, weight = w)
      #cat(paste(colnames(gamma)[i], ": "), t_test$p.value, "\n")
      var = colnames(gamma)[i]
      result[1, var] = t_test$coefficients[3]
      result[2, var] = t_test$additional[2]
    }
  }
  return(result)
}

regression_table_LR <- function(param, n_param, param2)
{
  # Initialization
  p_values2 = matrix(nrow=1, ncol=n_param)
  gammas2 = matrix(nrow=1, ncol=n_param)
  colnames(p_values2) = param
  colnames(gammas2) = param
  pb = txtProgressBar(min=0, max=16, style = 3)
  
  # Regression with only beta
  result = regression_LR(c("beta_t1"))
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2[1, ] = result2[1, ]
  gammas2[1, ] = result2[2, ]
  setTxtProgressBar(pb, 1)
  
  
  # Regression with only AIM
  result = regression_LR(c("AIM_t1"))
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 2)
  
  # Regression with only PIN
  result = regression_LR(c("pin_t1"))
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 3)
  
  # Regression with only SIZE
  result = regression_LR(c("log_market_cap_t1"))
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 4)
  
  # Regression with beta + AIM
  modele = c("beta_t1", "AIM_t1")
  result = regression_LR(modele)
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 5)

  # Regression with beta + AIM + PIN
  modele = c("beta_t1", "AIM_t1", "pin_t1")
  result = regression_LR(modele)
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 6)
  
  # Regression with beta + AIM + SIZE
  modele = c("beta_t1", "AIM_t1", "log_market_cap_t1")
  result = regression_LR(modele)
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 7)
  
  # Regression with beta + AIM + SIZE + PIN
  modele = c("beta_t1", "AIM_t1", "log_market_cap_t1", "pin_t1")
  result = regression_LR(modele)
  gamma = result$gamma
  errors = result$errors
  result2 = LR(gamma, errors, param)
  
  p_values2 = rbind(p_values2, result2[1, ])
  gammas2 = rbind(gammas2, result2[2, ])
  setTxtProgressBar(pb, 8)
  
  # Regression with the others
  k = 9
  for (i in 5:n_param)
  {
    modele = param[1:i]
    result = regression_LR(modele)
    gamma = result$gamma
    errors = result$errors
    result2 = LR(gamma, errors, param)
    
    p_values2 = rbind(p_values2, result2[1, ])
    gammas2 = rbind(gammas2, result2[2, ])
    setTxtProgressBar(pb, k)
    k = k+1
  }
  
  for (i in 4:(n_param-1))
  {
    modele = param2[1:i]
    result = regression_LR(modele)
    gamma = result$gamma
    errors = result$errors
    result2 = LR(gamma, errors, param)
    
    p_values2 = rbind(p_values2, result2[1, ])
    gammas2 = rbind(gammas2, result2[2, ])
    setTxtProgressBar(pb, k)
    k = k+1
  }
  
  L = list("p_values" = p_values2, "gammas"= gammas2)
  return(L)
}

