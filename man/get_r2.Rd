% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_r2.R
\name{get_r2}
\alias{get_r2}
\title{Get R2}
\usage{
my_lm <- lm(iris$Sepal.Length ~ iris$Sepal.Length)
get_r2(my_lm)
}
\arguments{
\item{model_object}{An 'lm' or 'glm' model object}
}
\value{
An R-squared value
}
\description{
Gets R-squared value for LM and GLM objects
}
