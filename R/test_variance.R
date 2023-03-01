tests_variance <- function(df, data_var, group_var) {
  # Load library
  library("car")
  library("ggplot2")
  library("gridExtra")

    # Run tests ---
  
    # Run Bartlett test
    btest <- bartlett.test(df[[data_var]], df[[group_var]])
    # Print with string for variable names
    cat(paste("Bartlett test for homogeneity of variance", "\n",
              "Data:", data_var, "and", group_var, "\n",
              "Bartlett's K-squared:", btest$statistic, "\n",
              "df: ", btest$parameter, "\n",
              "p-value:", btest$p.value))
    
    # Run Levene Test
    myLevene <- car::leveneTest(y = df[[data_var]], 
                                group = df[[group_var]])
    
    # Insert spaces between test print-outs for legibility
    cat("\n", "\n", "---------------------" , "\n", "\n")
    print(myLevene)

    # Make simple plots ---
    par(mfrow = c(2, 2)) # set up grid
    
    # Boxplot with base R
    # boxplot(data[[data_var]] ~ data[[group_var]], 
    #         data = data,
    #         xlab = paste(group_var),
    #         ylab = paste(data_var),
    #         col = factor(group))
    
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
  
tests_variance(iris, "Sepal.Length", "Species")

# p<-ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
#   geom_boxplot() +
#   theme_classic()
# p

# densPlot <- ggplot(data = iris, aes(x = Sepal.Length,
#                                   fill = Species)) +
#   geom_density()
# 
# densPlot
