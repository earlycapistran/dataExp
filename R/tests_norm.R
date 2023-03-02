# =============================================================================
# Test Normality
# earlyc@stanford.edu, February 2023
# =============================================================================

#' Performs Shapiro-Wilk test and makes normality plot (qq), density plot,
#' histogram, and histogram with normality curve.
#' 
#' @param df A dataframe
#' @param var1 Variable name passed as a string
#' @return A plot grid with normality plot, histogram, density plot, and 
#' histogram with normality curve. Prints Shapiro-Wilke test results.
#' @usage 
#' test_norm(iris, "Sepal.Length")
#' @export
#' 

tests_norm <- function(df, var1) {
  var1 <- deparse(substitute(var1))

  df <- na.omit(df) # Remove missing values
  # Make normality plot ---
  par(mfrow = c(2, 2)) # set up grid
  qqnorm(df[[var1]])
  qqline(df[[var1]], col = "red")
  
  # Make histogram with normality curve ---
  myhist <- hist(df[[var1]], 
                 main = "Histogram with normal curve",
                 xlab = paste(var1)
                 ) # Make histogram
  
  # Define multiplier to convert density to counts
  multiplier <- myhist$counts / myhist$density
  
  # Set an approximate value of 1.25 * maximum count to try and make sure
  # curve fits in y-axis
  yMax <- (1.25*(max(myhist$counts)))
  
  # Plot with adjusted axes
  plot(myhist, ylim = c(min(myhist$counts), yMax))

  # Generate normal curve 
  myX <- seq(min(df[[var1]]), max(df[[var1]]), length.out= 100)
  mymean <- mean(df[[var1]])
  mysd <- sd(df[[var1]])
  
  normal <- dnorm(x = myX, mean = mymean, sd = mysd)

  # Plot histogram with normal curve
  #plot(myhist, main = "Histogram with normal curve", xlab = paste(var1))
  lines(myX, normal * multiplier[1], col = "red")
  
  # Make density plot
  mydensity <- density(df[[var1]])
  plot(mydensity, main = "Density plot", xlab = paste(var1))
  polygon(mydensity, col = "blue")
  
  # Run normality tests ----
  norm_test <- shapiro.test(df[[var1]])
  print(norm_test)
}