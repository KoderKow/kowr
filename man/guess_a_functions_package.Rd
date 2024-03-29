% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess_a_functions_package.R
\name{guess_a_functions_package}
\alias{guess_a_functions_package}
\title{Where is your home?}
\usage{
guess_a_functions_package()
}
\description{
The primary use for this function is for its functionality via addin. The addin can be added as a shortcut; I set it as CMD+; for Mac and CTRL+; for Windows. This will search for possible packages a function belongs to, similar to \verb{?function}. If there are multiple packages that have the same function, this function will favor any of tidyverse packages. This is due to myself generally using the tidyverse functions. If there are multiple tidyverse matchse, it picks the first package based on its alphabetic positioning. If there are no tidyverse packages and other packages, it will still pick the first package that shows up alphabetically.
}
