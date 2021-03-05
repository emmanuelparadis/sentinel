\name{lonlat2ECEF}
\alias{lonlat2ECEF}
\title{Conversions of Coordinates}
\description{
  Conversion to ECEF.
}
\usage{
lonlat2ECEF(lon, lat = NULL, alt = 0, as.matrix = FALSE)
}
\arguments{
  \item{lon, lat}{coordinates given as two vectors or as a matrix (or
    data frame).}
  \item{alt}{a vector of altitudes.}
  \item{as.matrix}{a logical value specifying whether to return the
    results in a matrix. The default is to return them in a list.}
}
\details{
  This function uses an ellopsoid model of Earth shape with the
  following parameters: equatorial radius = 6,378,137 m, polar radius
  = 6,356,752 m.
}
\value{
  a list with three vectors or a matrix with three columns.
}
\author{Emmanuel Paradis}
\keyword{manip}
