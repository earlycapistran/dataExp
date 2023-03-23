# =============================================================================
# Get R2
# earlyc@stanford.edu, March 2023
# =============================================================================
get_r2 <- function(model_object) {
  # Check object type
  model_type <- class(model_object)[1]
  r_sq <- switch(model_type,
    lm = 1 - stats::var(stats::residuals(model_object)) /
      stats::var(model_object$model[[1]]),
    glm = 1 - model_object$deviance / model_object$null.deviance
  )
  return(r_sq)
  # Print
  cat("R²:", "\n", paste(r_sq))
}
