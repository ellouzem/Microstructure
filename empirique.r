library(readr)
library(dplyr)
library(plm)
table = read_csv(file = "data_final_2019.csv") 

# subyear = data$year[!is.na(data$pin)]

alpha <- c()
gamma <- c()
years = sort(unique(table$year))
param <- c(6, 8, 10, 12, 14, 16, 18, 20, 22, 27, 28, 29)
for (y in years)
{
  months = sort(unique((table$month[table$year == y])))
  n = length(months)
  for (m in months[2:n])
  {
    renta = table$return_rf[table$year == y & table$month == m]
    beta_t1 = table$beta_t1[table$year == y & table$month == m]
    fit = fpmg(renta ~ beta_t1)
    alpha <- c(alpha, fit$coefficients[1])
    gamma <- c(gamma, fit$coefficients[2])
  }
}


