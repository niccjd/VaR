#Question 1
calculate_historical_var <- function(data, alpha = 0.05) {
  
  sorted_data <- sort(data)
  
  
  n <- length(sorted_data)
  index <- ceiling(n * (1 - alpha))
  
  
  var <- sorted_data[index]
  
  return(var)
}
data <- c(23, 32, 30, 38, 40, 56, 82, 80, 75, 90)
var_95 <- calculate_historical_var(data, alpha = 0.05)
cat("VaR at 95% confidence level (historical method):", var_95, "\n")


