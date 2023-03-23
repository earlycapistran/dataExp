# =============================================================================
# Get R2
# earlyc@stanford.edu, March 2023
# =============================================================================

#' Gets R-squared value for LM and GLM objects
#' 
#' @title Get R2
#' @name get_r2
#' @param model_object An 'lm' or 'glm' model object
#' @return An R-squared value
#' @usage 
#' my_lm <- lm(iris$Sepal.Length ~ iris$Sepal.Length)
#' get_r2(my_lm)
#' @export
#' 
#' @importFrom stats var
#' @importFrom stats residuals


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
