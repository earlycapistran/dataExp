tests_variance <- function(df, data_var, group_var) {
  # Load library
  library("car")
  library("ggplot2")
  library("gridExtra")

  df <- na.omit(df) # Remove missing values
  
  # Deparse variable names
  data_var <- deparse(substitute(data_var))
  group_var <- deparse(substitute(group_var))

    # Run Levene Test
    myLevene <- car::leveneTest(y = df[[data_var]],
                                group = df[[group_var]])

    # Insert spaces between test print-outs for legibility
    cat("\n", "\n", "---------------------" , "\n", "\n")
    print(myLevene)

    # Make simple plots ---
    par(mfrow = c(2, 2)) # set up grid

    # Boxplot with ggplot
    boxPlot <-ggplot(data = df, aes(x = .data[[group_var]],
                        y = .data[[data_var]],
                        fill = .data[[group_var]])) +
      geom_boxplot(outlier.size=2) +
      theme_classic()

    # Make a boxplot with dots
    boxDots <- boxPlot +
      geom_dotplot(binaxis='y',
                   stackdir='center',
                   dotsize=1,
                   fill = "grey",
                   alpha = 0.5)

    # Density plot by group
    densPlot <- ggplot(data = df, aes(x = .data[[data_var]],
                   fill = .data[[group_var]],
                   )) +
      geom_density(alpha = 0.5)

    # Arrange the plots on a grid
    gridExtra::grid.arrange(boxPlot, boxDots, densPlot, nrow = 2)
  }
