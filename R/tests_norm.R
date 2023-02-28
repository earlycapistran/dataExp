# =============================================================================
# Test Normality
# earlyc@stanford.edu, February 2023
# =============================================================================

#' Performs Shapiro-Wilk test and makes normality plots (qq)
#' 
#' @param data a dataframe
#' @param variable variable name passed as a string
#' @return normality plot and Shapiro-Wilke test results
#' @export
#' @example 
#' test_normality(iris, "Sepal.Length")
#' 

tests_norm <- function(data, variable) {
  
  # Make normality plot ---
  par(mfrow = c(2, 2)) # set up grid
  qqnorm(data[[variable]])
  qqline(data[[variable]], col = "red")
  
  # Make histogram with normality curve ---
  data <- na.omit(data) # Remove missing values
  myhist <- hist(data[[variable]], main = "Histogram") # Make histogram
  # Define multiplier to convert density to counts
  multiplier <- myhist$counts / myhist$density 
  mydensity <- density(data[[variable]]) 
  mydensity$y <- mydensity$y * multiplier[1] 
  
  plot(myhist, main = "Histogram with density curve")
  lines(mydensity, col = 4)
  
  # Normal curve 
  myX <- seq(min(data[[variable]]), max(data[[variable]]), length.out= 100)
  mymean <- mean(data[[variable]])
  mysd <- sd(data[[variable]])
  
  normal <- dnorm(x = myX, mean = mymean, sd = mysd)
  
  plot(myhist, main = "Histogram with normal curve")
  lines(myX, normal * multiplier[1], col = "red", lwd = 1)
  
  # Run normality tests ----
  norm_test <- shapiro.test(data[[variable]])
  print(norm_test)
}

tests_norm(penguins, "body_mass_g")
