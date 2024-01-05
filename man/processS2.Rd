\name{aggregateS2}
\alias{aggregateS2}
\alias{interpol}
\alias{tabulateS2}
\title{Post-processing of Sentinel-2 Raw Data}
\description{
  \code{aggregateS2} aggregates Sentinel-2 data from 10-m resolution to
  either 20-m or 60-m resolution.

  \code{interpol} does bilinear interpolation from 20-m-m resolution to
  10-m resolution.

  \code{tabulateS2} tabulates (efficiently) the values of reflectance
  using preset bin intervals (equal to the precision of the raw data).
}
\usage{
aggregateS2(x, finalreso = 60)
interpol(x)
tabulateS2(x)
}
\arguments{
  \item{x}{a vector of reflectance values for a single band at the 10-m
  resolution (so there must 10980 by 10980 pixels).}
  \item{finalreso}{the target resolution for the aggregation.}
}
\details{
  These functions are under development and are currently intended to be
  used in combination with others functions (e.g., to extract the values
  from the raw rasters).
  The outputs also need to be input to appropriate data structures
  (e.g., rasters).
}
\value{
  a vector of numeric values of the appropriate length.
}
\author{Emmanuel Paradis}
\keyword{manip}
