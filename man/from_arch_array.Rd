% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/from-array.R
\name{from_arch_array}
\alias{from_arch_array}
\alias{from_arch_array.default}
\alias{from_arch_array.NULL}
\alias{from_arch_array.logical}
\alias{from_arch_array.integer}
\alias{from_arch_array.double}
\alias{from_arch_array.raw}
\alias{from_arch_array.character}
\alias{from_arch_array.factor}
\alias{from_arch_array.data.frame}
\title{Convert Arrow vectors to R objects}
\usage{
from_arch_array(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{default}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{`NULL`}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{logical}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{integer}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{double}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{raw}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{character}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{factor}(x, ptype = arch_default_ptype(x$schema), ...)

\method{from_arch_array}{data.frame}(x, ptype = arch_default_ptype(x$schema), ...)
}
\arguments{
\item{x}{An \code{\link[=arch_array]{arch_array()}}}

\item{ptype}{An R object to use as a prototype}

\item{...}{Passed to S3 methods}
}
\value{
An object with the same class as \code{ptype}
}
\description{
Note that \code{\link[=from_arch_array]{from_arch_array()}} dispatches on \code{ptype}
}
\examples{
from_arch_array(as_arch_array(c(TRUE, FALSE, NA)), logical())

}
