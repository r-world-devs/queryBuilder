% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{in_string}
\alias{in_string}
\title{Check if character value matches the provided pattern}
\usage{
in_string(x, pattern, ...)
}
\arguments{
\item{x}{String value.}

\item{pattern}{Pattern that should be matched to \code{x}.}

\item{...}{Extra arguments passed to \link{grepl}.}
}
\value{
A logical vector indicating which elements of \code{x} are matching the provided \code{pattern}.
}
\description{
Check if character value matches the provided pattern
}
