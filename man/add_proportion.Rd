% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_proportions.R
\name{add_proportion}
\alias{add_proportion}
\title{Add proportions by group}
\usage{
add_proportion(
  x,
  y,
  ...,
  name = "p",
  percent_format = FALSE,
  percent_accuracy = 0.01
)
}
\arguments{
\item{x}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr).}

\item{y}{Variable to calculate proportions from.}

\item{...}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}> Variables to group by.}

\item{name}{The name of the new column in the output.

If omitted, it will default to \code{n}. If there's already a column called \code{n},
it will error, and require you to specify the name.}

\item{percent_format}{A logical. If \code{TRUE}, transform the numeric decimal into a readable percent of class character as a new column; \verb{\{name\}_format}. If this parameter is missing it will default to the last column of the dataset passed in.}

\item{percent_accuracy}{A numeric. If \code{percent_format} is \code{TRUE}, number to round the decimal to. Uses \code{scales::percent()} accuracy method for transformations.}
}
\value{
An object of the same type as \code{x}. The output has the same groups as the input.
}
\description{
Add proportions by group
}
\examples{
mtcars \%>\%
  dplyr::count(cyl) \%>\%
  add_proportion(n)
}
