
analyse_lm_residuals <- function(myLm) {
# Load and prepare data --------
  resi <- myLm$residuals # Store residuals
  fit <- myLm$fitted.values  # Store fitted values
  resiLag <- c(resi[-1], NA) # Store lagged residuals
  # Subset residuals by sign
  resiSign <- as.factor(ifelse(resi < 0, "negative", "positive"))
  # Set up dataframe for Levene test
  leveneDf <- data.frame(resi, resiSign)
  
# Make plots --------
  par(mfrow = c(2, 2))
  stats::qqnorm(resi)
  stats::qqline(resi, col = "red")
  # Residual vs. fitted values
  resPlot <- plot(x = fit, y = resi,
                   xlab = "Fitted values", 
                   ylab = "Residuals",
                   main = "Residuals versus fitted values")
  abline(h=0, col = "red")
  # Lag plot with trend line (residuals vs. lagged residuals)
  plot(resi, c(resi[-1], +  NA),
       xlab = "Residuals", 
       ylab = "Lagged residuals",
       main = "Autocorrelation") 
  abline(lm(resi ~ resiLag), col = "red")
  par(mfrow = c(1, 1)) 
  
  # Run tests -----------------------------------------------------------------
  norm <- stats::shapiro.test(resi) #Shapiro-Wilke
  levene <- car::leveneTest(resi ~ resiSign, data = leveneDf) # Levene
  runs <- DescTools::RunsTest(resi) # Runs
  result <- list(normality = norm, levene = levene, runs = runs)
  names(result) <- c("Residual Normality Test", "Levene's Test", "Runs Test")
  print(result)
  
  # Run t-test for mean = 0 -------
  stDev <- stats::sd(resi)
  resiMean <- mean(resi)
  degF <- summary(lm)$df[2] # Get second value from DF printout
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