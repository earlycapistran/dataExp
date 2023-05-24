# =============================================================================
# Residual analysis for linear models
# earlyc@stanford.edu, March 2023
# =============================================================================
#' 
#' @title Residual analysis for linear models
#' @name analyse_lm_residuals
#' @param lm_object An object of class 'lm' 
#' @return Residual plots: normality, residuals vs. fitted data, 
#' autocorrelation (lagged residuals vs. residuals with fitted linear model). 
#' 
#' Prints residual tests: Mean zero, Shapiro-Wilk normality test, 
#' Levene Test for homogeneity of variance, and Run's test for randomness
#' 
#' @export
#' 
#' @usage
#' iris_lm <- lm(iris$Sepal.Length, iris$Sepal.Width)
#' analyse_lm_residuals(iris_lm)
#' 
#' @importFrom car leveneTest
#' @importFrom DescTools RunsTest
#' @importFrom stats lm
#' @importFrom stats shapiro.test
#' @importFrom stats sd
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom graphics abline
#' @importFrom graphics par
#' 
#' @rdname analyse_lm_residuals
#' @aliases analyze_lm_residuals
#' 

analyse_lm_residuals <- function(lm_object) {
  # Load libraries
  library(car)
  library(DescTools)
  
  # Check if the input is an lm object
  if (!inherits(lm_object, "lm")) {
    stop("Input model is not of class 'lm'.")
  }
  
# Load and prepare data --------
  resi <- lm_object$residuals # Store residuals
  fit <- lm_object$fitted.values  # Store fitted values
  resiLag <- c(resi[-1], NA) # Store lagged residuals
  # Subset residuals by sign
  resiSign <- as.factor(ifelse(resi < 0, "negative", "positive"))
  # Set up dataframe for Levene test
  leveneDf <- data.frame(resi, resiSign)
  
# Make plots --------
  par(mar = c(1, 1, 1, 1)) # Adjust margins
  graphics::par(mfrow = c(2, 2))
  stats::qqnorm(resi)
  stats::qqline(resi, col = "red")
  # Residual vs. fitted values
  resPlot <- graphics::plot(x = fit, y = resi,
                   xlab = "Fitted values", 
                   ylab = "Residuals",
                   main = "Residuals versus fitted values")
  graphics::abline(h=0, col = "red")
  # Lag plot with trend line (residuals vs. lagged residuals)
  graphics::plot(resi, c(resi[-1], +  NA),
       xlab = "Residuals", 
       ylab = "Lagged residuals",
       main = "Autocorrelation") 
  graphics::abline(lm(resi ~ resiLag), col = "red")
  graphics::par(mfrow = c(1, 1)) 
  
  # Run tests -----------------------------------------------------------------
  norm <- stats::shapiro.test(resi) #Shapiro-Wilke
  levene <- car::leveneTest(resi ~ resiSign, data = leveneDf) # Levene
  durbin <- car::durbinWatsonTest(lm_object) # Durbin-Watson
  result <- list(normality = norm, levene = levene, durbin = durbin)
  names(result) <- c("Residual Normality Test ---", 
                     "Levene's Test ---", 
                     "Durbin-Watson Test ---")
  print(result)
  
  # Run t-test for mean = 0 -------
  stDev <- stats::sd(resi)
  resiMean <- mean(resi)
  degF <- summary(lm_object)$df[2] # Get second value from DF printout
  tValue  <- abs(resiMean/stDev)
  pValue <- dt(tValue, df=degF)
  tResult <- cbind("Residual mean"=resiMean,
                   "t-value"=tValue,
                   "p-value"=pValue)
  rownames(tResult) <- c("")
  cat(paste("$ `t-Test for residual mean zero`", "\n"))
  print(tResult)
  cat(paste("Alternative hypothesis: true mean is not equal to 0", "\n"))
}