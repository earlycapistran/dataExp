# =============================================================================
# Tests Variance
# earlyc@stanford.edu, March 2023
# =============================================================================

#' Performs Levene tests and makes boxplots and density plot
#'
#' @title Tests Variance
#' @name tests_variance
#' @param df A dataframe
#' @param data_var A dataframe column with numeric values
#' @param group_var A dataframe column with a grouping variable (factor)
#' @return A plot grid with boxplot, boxplot with dots, and density plot.
#' Prints Levene test results.
#' @usage
#' tests_variance(Sepal.Length, Species)
#' @export
#'
#' @importFrom car leveneTest
#' @importFrom gridExtra grid.arrange
#' @importFrom stats na.omit
#' @importFrom graphics par

tests_variance <- function(df, data_var, group_var) {
  # Load library
  library("car")
  library("ggplot2")
  library("gridExtra")

  # Deparse variable names
  data_var <- deparse(substitute(data_var))
  group_var <- deparse(substitute(group_var))

  # Check data classes
  stopifnot(
    is.data.frame(df),
    is.numeric(df[[data_var]]),
    !anyNA(df[[data_var]]),
    is.factor(df[[group_var]])
  )

  # Run Levene Test
  my_levene <- car::leveneTest(
    y = df[[data_var]],
    group = df[[group_var]]
  )

  # Insert spaces between test print-outs for legibility
  cat("\n", "\n", "---------------------", "\n", "\n")
  print(my_levene)

  # Make simple plots ---
  par(mfrow = c(2, 2)) # set up grid

  # Boxplot with ggplot
  box_plot <- ggplot(data = df, aes(
    x = .data[[group_var]],
    y = .data[[data_var]],
    fill = .data[[group_var]]
  )) +
    geom_boxplot(outlier.size = 2) +
    theme_classic()

  # Make a boxplot with dots
  box_dots <- box_plot +
    geom_dotplot(
      binaxis = "y",
      stackdir = "center",
      dotsize = 1,
      fill = "grey",
      alpha = 0.5
    )

  # Density plot by group
  dens_plot <- ggplot(data = df, aes(
    x = .data[[data_var]],
    fill = .data[[group_var]],
  )) +
    geom_density(alpha = 0.5)

  # Arrange the plots on a grid
  gridExtra::grid.arrange(box_plot, box_dots, dens_plot, nrow = 2)
}
