% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_variable.R
\name{summarize_variable}
\alias{summarize_variable}
\title{Simple summary of a numeric type variable}
\usage{
summarize_variable(.data, .col, round = TRUE)
}
\arguments{
\item{.data}{A tbl.}

\item{.col}{A numeric column that will be summarized}

\item{round}{A logical. If \code{TRUE}, the default, it will round all decimals to the hundredths position.}
}
\value{
A summarized tbl
}
\description{
If the column is numeric or a date this function will return the min, Q1, median (Q2), Q3, max, mean, sd (only for numeric and interger), and count of missing data. If data type is of character or factor, a count of the character will be returned alonged with p (percent of column, or group if \code{dplyr::group_by()} is passed) and a formatted p for a clean presentation in plots and/or tables.
}
