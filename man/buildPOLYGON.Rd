\name{buildPOLYGON}
\alias{buildPOLYGON}
\alias{as.POLYGON}
\alias{print.POLYGON}
\alias{plot.POLYGON}
\title{Manage Geographical Polygons}
\description{
  Utility functions to manage geographical polygons.
}
\usage{
buildPOLYGON(bottomleftcorner, toprighcorner, interactive = FALSE,
             xlim = c(-180, 180), ylim = c(-90, 90))
as.POLYGON(x, y = NULL)
\method{print}{POLYGON}(x, \dots)
\method{plot}{POLYGON}(x, add = FALSE, col = rgb(1, 1, 0, 0.25), xlim = NULL, ylim = NULL, \dots)
}
\arguments{
  \item{bottomleftcorner}{a numeric vector with two values giving the
    longitude and latitude (in decimal degrees) of the bottom-left
    corner of a rectangle.}
  \item{toprighcorner}{id. for top-right corner.}
  \item{interactive}{a logical value. If \code{TRUE}, the user is
    invited to click on a map plotted to define the coordinates of the
    polygon.}
  \item{xlim, ylim}{the limits of the map.}
  \item{x, y}{two numeric vectors of geographical coordinates.}
  \item{add}{a logical value specifying whether a make a new plot (the
    default), or add to an existing plot.}
  \item{col}{the color of the piolygon (by default: transparent yellow).}
  \item{\dots}{further arguments passed to and from methods.}
}
\details{
  These functions help to build objects of class \code{"POLYGON"} which
  are then optionally used when calling searchCopernicus.
}
\value{an object of class \code{"POLYGON"}.}
\author{Emmanuel Paradis}
\keyword{manip}