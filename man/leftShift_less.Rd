% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/localMin.R
\name{leftShift_less}
\alias{leftShift_less}
\title{Check X-left side neighbors are less}
\usage{
leftShift_less(X, vec, vecLength)
}
\arguments{
\item{X}{a integer vector of sequence c(1:neighborhood)}

\item{vec}{the vector of values to be evaluated}

\item{vecLength}{the length of vec}
}
\value{
logical vector padded with `FALSE` values on the front
}
\description{
Check X-left side neighbors are less
}
