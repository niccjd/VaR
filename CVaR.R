#Question 2
calculate_cvar <- function(data, alpha = 0.05) {
  var <- calculate_historical_var(data, alpha)
  
  exceedances <- data[data < var]
  
  
  cvar <- mean(exceedances)
  
  return(cvar)
}

data <- c(23, 32, 30, 38, 40, 56, 82, 80, 75, 90)


cvar_95 <- calculate_cvar(data, alpha = 0.05)
cat("CVaR at 95% confidence level (historical method):", cvar_95, "\n")
