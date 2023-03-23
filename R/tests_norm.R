# =============================================================================
# Tests Normality
# earlyc@stanford.edu, February 2023
# =============================================================================

#' Performs Shapiro-Wilk test and makes normality plot (qq), density plot,
#' histogram, and histogram with normality curve.
#' @title Tests normality
#' @name tests_norm
#' @param df A dataframe
#' @param x A dataframe column with numeric values
#' @value A plot grid with normality plot, histogram, density plot, and
#' histogram with normality curve. Prints Shapiro-Wilke test results.
#' @usage
#' tests_norm(iris, Sepal.Length)
#' @export
#' @importFrom stats shapiro.test
#' @importFrom stats sd
#' @importFrom stats qqnorm
#' @importFrom stats qqline

tests_norm <- function(df, x) {
  # Deparse variable name
  x <- deparse(substitute(x))

  stopifnot(
    is.data.frame(df),
    is.numeric(df[[x]]),
    !anyNA(df[[x]])
  )

  df <- na.omit(df) # Remove missing values
  # Make normality plot ---
  par(mfrow = c(2, 2)) # set up grid
  qqnorm(df[[x]])
  qqline(df[[x]], col = "red")

  # Make histogram with normality curve ---
  myhist <- hist(df[[x]], # Make base histogram
    main = "Histogram",
    xlab = paste(x)
  )
  # Define multiplier to convert density to counts
  multiplier <- myhist$counts / myhist$density
  # Set an approximate value of 1.25 * maximum count to try and make sure
  # curve fits in y-axis.
  y_max <- (1.25 * (max(myhist$counts)))
  # Plot with adjusted axes
  plot(myhist,
    main = "Histogram with normal curve",
    ylim = c(min(myhist$counts), y_max),
    xlab = paste(x)
  )
  # Generate normal curve
  my_x <- seq(min(df[[x]]), max(df[[x]]), length.out = 100)
  my_mean <- mean(df[[x]])
  my_sd <- sd(df[[x]])
  normal <- dnorm(x = my_x, mean = my_mean, sd = my_sd)
  # Plot histogram with normal curve
  lines(my_x, normal * multiplier[1], col = "red")
  # Make density plot
  my_density <- density(df[[x]])
  plot(my_density, main = "Density plot", xlab = paste(x))
  polygon(my_density, col = "blue")
  # Run normality tests ----
  norm_test <- shapiro.test(df[[x]])
  print(norm_test)
}
