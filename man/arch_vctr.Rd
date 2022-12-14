% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vctr.R
\name{arch_vctr}
\alias{arch_vctr}
\alias{as_arch_vctr}
\alias{new_arch_vctr}
\title{Create R vector wrappers around 'Arrow' arrays}
\usage{
arch_vctr(array = arch_array())

as_arch_vctr(x, ...)

new_arch_vctr(x = integer(), array = arch_array())
}
\arguments{
\item{array}{A \code{\link[=arch_array]{arch_array()}}}

\item{x}{An object to convert to a \code{\link[=arch_vctr]{arch_vctr()}}}

\item{...}{Passed to \code{\link[=as_arch_array]{as_arch_array()}}}
}
\value{
An object of class 'arch_vctr'
}
\description{
Create R vector wrappers around 'Arrow' arrays
}
\examples{
as_arch_vctr(1:10)

}
