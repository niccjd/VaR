library(ggplot2)

getwd()
robo_data$date <- as.Date(robo_data$date, format = "%y/%m/%d")
robo_data <- read.csv("ROBO2.csv")


window_size <- 24 * 30  # Assuming an average of 30 days per month


rolling_var <- numeric(0)
rolling_cvar <- numeric(0)

#  rolling VaR and CVaR using the historical method
for (i in window_size:length(robo_data$hvt30d)) {
  # Extract data for the current rolling window
  window_data <- robo_data$hvt30d[(i - window_size + 1):i]
  
  
  confidence_level <- 0.95
  
  # VaR (5th percentile)
  var <- quantile(window_data, probs = 1 - confidence_level)
  
  # CVaR (average of losses beyond VaR)
  cvar <- mean(window_data[window_data <= var])
  
  
  rolling_var <- c(rolling_var, var)
  rolling_cvar <- c(rolling_cvar, cvar)
}


rolling_risk_data <- data.frame(
  Date = robo_data$dates[window_size:length(robo_data$hvt30d)],
  VaR = rolling_var,
  CVaR = rolling_cvar
)


print(rolling_risk_data)
