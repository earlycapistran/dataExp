  tests_variance <- function(data, data_var, group_var) {
  # Load library
  library("car")
    
    # Run tests ---
  
    # Run Bartlett test
    btest <- bartlett.test(data[[data_var]], data[[group_var]])
    # Print with string for variable names
    cat(paste("Bartlett test for homogeneity of variance", "\n",
              "Data:", data_var, "and", group_var, "\n",
              "Bartlett's K-squared:", btest$statistic, "\n",
              "df: ", btest$parameter, "\n",
              "p-value:", btest$p.value))
    
    # Run Levene Test
    myLevene <- car::leveneTest(y = data[[data_var]], 
                                group = data[[group_var]])
    
    # Insert spaces between test print-outs for legibility
    cat("\n", "\n", "---------------------" , "\n", "\n")
    print(myLevene)
  }
  
  tests_variance(iris, "Sepal.Length", "Species")
