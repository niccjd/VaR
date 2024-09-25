#Question 3

# a 



library(ggplot2)

getwd()

robo_data <- read.csv("ROBO2.csv")
summary(robo_data)
missing_data <- colSums(is.na(robo_data))

ggplot(robo_data, aes(x = price)) +
  geom_histogram(fill = "blue", bins = 30) +
  labs(title = "Histogram of Price",
       x = "Price (USD)",
       y = "Frequency")


robo_data$date <- as.Date(robo_data$dates, format = "%m/%d/%Y")

ggplot(robo_data, aes(x = date , y = price)) +
  geom_line(color = "red") +
  labs(title = "Time Series Plot of Price",
       x = "dates",
       y = "Prices") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(robo_data, aes(x = yesterday, y = today)) +
  geom_point(color = "green") +
  labs(title = "Scatterplot of Today vs. Yesterday's Return",
       x = "Yesterday's Return (%)",
       y = "Today's Return (%)") 



calculate_historical_var <- function(data, alpha = 0.05) {
  
  sorted_data <- sort(data)
  
  
  n <- length(sorted_data)
  index <- ceiling(n * (1 - alpha))
  
  
  var <- sorted_data[index]
  
  return(var)
}
data <- robo_data$hvt30d
var_95 <- calculate_historical_var(data, alpha = 0.05)
cat("VaR at 95% confidence level (historical method):", var_95, "\n")


