test_normality <- function(data, variable) {
  par(mfrow = c(1, 1))
  qqnorm(data[[variable]])
  qqline(data[[variable]])
  norm_test <- shapiro.test(data[[variable]])
  print(norm_test)
}

test_normality(penguins, "body_mass_g")
