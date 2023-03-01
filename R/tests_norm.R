# =============================================================================
# Test Normality
# earlyc@stanford.edu, February 2023
# =============================================================================

#' Performs Shapiro-Wilk test and makes normality plot (qq), density plot,
#' histogram, and histogram with normality curve.
#' 
#' @param data A dataframe
#' @param variable Variable name passed as a string
#' @return A plot grid with normality plot, histogram, density plot, and 
#' histogram with normality curve. Prints Shapiro-Wilke test results.
#' @usage 
#' test_norm(iris, "Sepal.Length")
#' @export

#' 

tests_norm <- function(data, variable) {

  # Make normality plot ---
  par(mfrow = c(2, 2)) # set up grid
  qqnorm(data[[variable]])
  qqline(data[[variable]], col = "red")
  
  # Make histogram with normality curve ---
  data <- na.omit(data) # Remove missing values
  myhist <- hist(data[[variable]], 
                 main = "Histogram with normal curve",
                 xlab = paste(variable)
                 ) # Make histogram
  
  # Define multiplier to convert density to counts
  multiplier <- myhist$counts / myhist$density
  
  # Set an approximate value of 1.25 * maximum count to try and make sure
  # curve fits in y-axis
  yMax <- (1.25*(max(myhist$counts)))
  
  # Plot with adjusted axes
  plot(myhist, ylim = c(min(myhist$counts), yMax))

  # Generate normal curve 
  myX <- seq(min(data[[variable]]), max(data[[variable]]), length.out= 100)
  mymean <- mean(data[[variable]])
  mysd <- sd(data[[variable]])
  
  normal <- dnorm(x = myX, mean = mymean, sd = mysd)

  # Plot histogram with normal curve
  #plot(myhist, main = "Histogram with normal curve", xlab = paste(variable))
  lines(myX, normal * multiplier[1], col = "red")
  
  # Make density plot
  mydensity <- density(data[[variable]])
  plot(mydensity, main = "Density plot", xlab = paste(variable))
  polygon(mydensity, col = "blue")
  
  # Run normality tests ----
  norm_test <- shapiro.test(data[[variable]])
  print(norm_test)
}

