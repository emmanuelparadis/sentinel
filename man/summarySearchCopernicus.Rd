\name{summarySearchCopernicus}
\alias{summarySearchCopernicus}
\alias{overviewSearchCopernicus}
\alias{clearXMLcache}
\alias{getFootprint}
\alias{print.footprint}
\alias{plot.footprint}
\alias{lines.footprint}
\alias{points.footprint}
\alias{overlap}
\title{Summarize Information from Copernicus Searches}
\description{
  These functions summarize the information after performing one or
  several searches on Copernicus.

  \code{summarySearchCopernicus} returns a table with information (with
  different levels of precision) on a previous search.

  \code{overviewSearchCopernicus} lists a small table with the number of
  files and of products for each previous searches.

  \code{clearXMLcache} deletes all downloaded XML files.

  \code{getFootprint} returns the footprint (i.e., geographical
  coordinates) of the products in a previous search.

  \code{overlap} computes the overlap between a between and a set of
  footprints.
}
\usage{
summarySearchCopernicus(search = 1, level = 1)
overviewSearchCopernicus()
clearXMLcache()
getFootprint(search = 1)
\method{print}{footprint}(x, ...)
\method{plot}{footprint}(x, xlim = NULL, ylim = NULL, col = rgb(1, 1, 0, 0.1),
      border = "black", around = 1, ...)
\method{lines}{footprint}(x, col = rgb(0, 1, 0, 0.1), border = "black", ...)
\method{points}{footprint}(x, col = rgb(0, 1, 0, 0.1), border = "black", ...)
overlap(POLYGON, footprint)
}
\arguments{
  \item{search}{an integer giving the number of the search.}
  \item{level}{an integer giving the level of details of the returned
    information; should be 1, 2, 3, or 4.}
  \item{x}{an object of class \code{"footprint"}.}
  \item{xlim, ylim, col, border}{graphical parameters used to display
    the footprints.}
  \item{around}{the extra space around the footprints.}
  \item{\dots}{arguments passed among methods.}
  \item{POLYGON}{an object of class \code{"POLYGON"}.}
  \item{footprint}{an object of class \code{"footprint"}.}
}
\details{
  TBD
}
\value{a data frame or nothing for \code{clearXMLcache}.}
\author{Emmanuel Paradis}
\keyword{manip}