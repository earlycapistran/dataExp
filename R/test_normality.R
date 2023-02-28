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
  par(mfrow = c(1, 1))
  qqnorm(data[[variable]])
  qqline(data[[variable]])
  norm_test <- shapiro.test(data[[variable]])
  print(norm_test)
}

tests_norm(penguins, body_mass_g)
