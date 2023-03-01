  tests_variance <- function(data, data_var, group_var) {
    # Load library
  
   # Run Bartlett test
    btest <- bartlett.test(data[[data_var]], data[[group_var]])
    # Print with string for variable names
    cat(paste("Bartlett test for homogeneity of variance", "\n",
              "Data:", data_var, "and", group_var, "\n",
              "Bartlett's K-squared:", btest$statistic, "\n",
              "df: ", btest$parameter, "\n",
              "p-value:", btest$p.value))
    print(btest)

  }
  
  tests_variance(iris, "Sepal.Length", "Species")
