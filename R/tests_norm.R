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
  myhist <- hist(data[[variable]], main = paste("Histogram"),
                 xlab = paste(variable)) # Make histogram
  
  # Define multiplier to convert density to counts
  multiplier <- myhist$counts / myhist$density 
  mydensity <- density(data[[variable]]) 
  mydensity$y <- mydensity$y * multiplier[1] 
  
  # Plot histogram with density curve
  plot(myhist, main = "Histogram with density curve", xlab = paste(variable))
  lines(mydensity, col = 4)
  
  # Generate normal curve 
  myX <- seq(min(data[[variable]]), max(data[[variable]]), length.out= 100)
  mymean <- mean(data[[variable]])
  mysd <- sd(data[[variable]])
  
  normal <- dnorm(x = myX, mean = mymean, sd = mysd)
  
  # Plot histogram with normal curve
  plot(myhist, main = "Histogram with normal curve", xlab = paste(variable))
  lines(myX, normal * multiplier[1], col = "red")
  
  # Run normality tests ----
  norm_test <- shapiro.test(data[[variable]])
  print(norm_test)
}

tests_norm(penguins, "body_mass_g")
