% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitmask.R
\name{as_arch_bitmask}
\alias{as_arch_bitmask}
\alias{as_arch_bitmask.default}
\alias{as_arch_bitmask.arch_bitmask}
\alias{as_arch_bitmask.logical}
\alias{new_arrow_bitmask}
\alias{as.logical.arch_bitmask}
\title{Create bitmask buffers}
\usage{
as_arch_bitmask(x, ...)

\method{as_arch_bitmask}{default}(x, ...)

\method{as_arch_bitmask}{arch_bitmask}(x, ...)

\method{as_arch_bitmask}{logical}(x, ...)

new_arrow_bitmask(buffer = raw())

\method{as.logical}{arch_bitmask}(x, ..., start = 1, end = NULL)
}
\arguments{
\item{x}{An object to convert to a bitmask}

\item{...}{Passed to S3 methods}

\item{buffer}{A \code{\link[=raw]{raw()}} vector containing the bitmask data}

\item{start, end}{Start and end indices (inclusive) to limit
when converting to \code{\link[=logical]{logical()}}.}
}
\value{
An object with class 'arch_bitmask'
}
\description{
Create bitmask buffers
}
\examples{
as_arch_bitmask(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
as.logical(new_arrow_bitmask(0x01))

}
