analyse_gls_residuals <- function(gls_object) {
  library(broom)
  library(nlme)
  
  # Load and prepare data -----------------------------------------------------
  resi <- resid(gls_object)
  resDf <- as.data.frame(resi)
  fit <- fitted(gls_object)
  
  # Subset residuals by sign
  resDf$sign <- as.factor(ifelse(resDf$resi < 0, "negative", "positive"))
  
  # Store lagged residuals
  resDf$resi_lag <- c(resDf$resi[-1], NA) 
  
  # Make plots ----------------------------------------------------------------
  par(mar = c(1, 1, 1, 1)) # Adjust margins
  
  # Normality
  graphics::par(mfrow = c(2, 2))
  stats::qqnorm(resDf$resi)
  stats::qqline(resDf$resi)
  
  # Residual vs. fitted values
  res.plot <- graphics::plot(x = fit, y = resi,
                             xlab = "Fitted values", 
                             ylab = "Residuals",
                             main = "Residuals versus fitted values")
  graphics::abline(h = 0)
  
  # Lag plot with trend line (residuals vs. lagged residuals)
  graphics::plot(resDf$resi, c(resDf$resi[-1], +  NA),
                 xlab = "Residuals", 
                 ylab = "Lagged residuals",
                 main = "Autocorrelation") 
  graphics::abline(lm(resDf$resi ~ resDf$resi_lag))
  graphics::par(mfrow = c(1, 1)) 
  
  # Run tests -----------------------------------------------------------------
  norm <- stats::shapiro.test(resDf$resi)
  levene <- car::leveneTest(resDf$resi ~ sign, data = resDf)
  durbin <- car::durbinWatsonTest(gls_object)
  
  result <- list(normality = norm, levene = levene, durbin = durbin)
  names(result) <- c("Residual Normality Test", 
                     "Levene's Test", 
                     "Durbin-Watson Test")
  print(result)
  
  # Run t-test for mean = 0 ---------------------------------------------------
  stDev <- stats::sd(resDf$resi)
  mean <- mean(resDf$resi)
  degF <- broom::glance(gls_object)$df.null
  t.value  <- abs(mean/stDev)
  p.value <- dt(t.value, df = degF)
  tResult <- cbind("Residual mean" = mean, "t-value" = t.value, "p-value" = p.value)
  rownames(tResult) <- c("")
  
  cat("$ `t-Test for residual mean zero`\n")
  print(tResult)
  cat("Alternative hypothesis: true mean is not equal to 0\n")
}
