% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/Vowpal_extraction7.R
\name{change_NA}
\alias{change_NA}
\title{Cultivates NA's handling.}
\usage{
change_NA(dataset)
}
\arguments{
\item{dataset}{Name of the dataset.}
}
\description{
Function that creates an is.na() vector for each column that has at least one NA
and it then replaces all NA's with 0. The function is primarily used via a call
from csv2vw.
}

