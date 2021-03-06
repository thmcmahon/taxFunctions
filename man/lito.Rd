% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lito.R
\name{lito}
\alias{lito}
\title{Calculate the low income tax offset. This is deducted from non-refundable tax
offset that reduces individuals' tax owed.}
\usage{
lito(income, value = 445, taper_start = 37000, taper_rate = 0.015,
  second_taper_start = NA, second_taper_rate = NA)
}
\arguments{
\item{income}{Numeric. Gross income.}

\item{value}{Numeric. The full value of the offset.}

\item{taper_start}{Numeric. Income level from which to start tapering.}

\item{taper_rate}{Numeric. Rate at which the offset tapers}

\item{second_taper_start}{Numeric. Income level at which the second taper
starts}

\item{second_taper_rate}{Numeric. Rate at which the second taper starts.}
}
\description{
Calculate the low income tax offset. This is deducted from non-refundable tax
offset that reduces individuals' tax owed.
}
\examples{
lito(18000)
}
