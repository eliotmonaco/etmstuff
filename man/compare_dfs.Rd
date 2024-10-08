% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_dfs.R
\name{compare_dfs}
\alias{compare_dfs}
\title{Compare dataframes with some flexibility}
\usage{
compare_dfs(df1, df2, ignore = NULL, output = "values")
}
\arguments{
\item{df1, df2}{Dataframes to compare (must have the same dimensions).}

\item{ignore}{A value or a list of values to ignore in the comparison.}

\item{output}{A string or function that determines the output when unequal values are compared.
\itemize{
\item \code{"values"} returns both input values in a string separated by "|".
\item \code{"diff"} returns the difference between the input values when both are numeric.
\item A function whose arguments are \code{x} and \code{y} can be provided.
}}
}
\value{
A dataframe with the same dimensions as the inputs.
}
\description{
This function allows for specified values to be ignored and for flexibility in output values when comparing two dataframes.
}
\details{
When the compared values from the input dataframes are equal, or when one is included in the \code{ignore} argument, \code{NA} is returned in the appropriate position in the output dataframe. When the compared values are unequal, the value returned is determined by the \code{output} argument.
}
\examples{
df1 <- data.frame(
  col1 = letters[1:9],
  col2 = TRUE,
  col3 = as.numeric(1:9)
)

df2 <- data.frame(
  col_1 = c(letters[1:3], LETTERS[4:6], letters[7:9]),
  col_2 = NA,
  col_3 = as.numeric(c(1:2, 30, 4:6, 70, 80, 9))
)

comp1 <- compare_dfs(df1, df2)
comp2 <- compare_dfs(df1, df2, ignore = NA)
comp3 <- compare_dfs(df1, df2, ignore = NA, output = "diff")
comp4 <- compare_dfs(df1, df2, ignore = NA, output = function(x, y) paste("x is", x, "but y is", y))

}
