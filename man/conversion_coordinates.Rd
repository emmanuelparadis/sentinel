\name{lonlat2UTM}
\alias{lonlat2UTM}
\alias{UTM2lonlat}
\alias{getUTMzone}
\alias{getUTMzone2}
\alias{getUTMhemisphere}
\title{Conversions of Coordinates}
\description{
  Handy functions to convert coordinates.
}
\usage{
lonlat2UTM(lon, lat = NULL, details = FALSE)
UTM2lonlat(X, zone, hemisphere)

getUTMzone(lon, lat = NULL)
getUTMzone2(lon)
getUTMhemisphere(lat)
}
\arguments{
  \item{lon, lat}{coordinates given as two vectors or as a matrix (or
    data frame).}
  \item{details}{a logical value indicating whether to return
    information on the UTM zones (see below).}
  \item{X}{a matrix or a data frame giving the UTM coordinates.}
  \item{zone, hemisphere}{an integer and a character string giving the
    UTM square}
}
\details{
  \code{lonlat2UTM} works for all UTM zones. If the coordinates cover
  several UTM zones and/or hemispheres, a warning is issued and the the
  options \code{details} is switched to \code{TRUE}.

  \code{getUTMzone} returns the UTM zone for all cases, including the
  special zones for Svalbard and Norway (latitude between 72 and 84
  degrees North and longitude between 0 and 9 degrees East, and all
  latitudes for longitudes between 9 and 42 degrees East).
  \code{getUTMzone2} is a faster version that works only outside
  these special zones. 
}
\value{
  the returned value depends on the function.
}
\author{Emmanuel Paradis}
\keyword{manip}
