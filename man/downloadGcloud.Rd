\name{downloadGcloud}
\alias{downloadGcloud}
\title{Download S2 MSI L1C Products From GoogleCloud}
\description{
  Download Sentinel-2 L1C Products from GoogleCloud. This function does
  not need a Google account since it uses the public URLs to the files.
}
\usage{
downloadGcloud(DF, bands, destdir = ".", engine = "wget", quiet = FALSE)
}
\arguments{
  \item{DF}{a data frame returned by
    \code{\link{summarySearchCopernicus}} with information on the
    products to be downloaded. This must use the \code{level = 4} option
    of this latter function.}
  \item{bands}{the bands to be downloaded. Can be numeric vector with
    value(s) between 1 and 12, or a vector of mode character. In either
    case, the values are checked. If left missing, all bands are
    download. For example, \code{bands = c(2:4, 8)} will download the
    four bands with a 10-m resolution.}
  \item{destdir}{the destination direction (as a character string). By
    default, the current working directory is used.}
  \item{engine}{the program used to download the files.}
  \item{quiet}{a logical value specifying whether the progress of the
    computations should be printed or not.}
}
\details{
  TBD
}
\value{NULL}
\author{Emmanuel Paradis}
\references{
  \url{https://cloud.google.com/storage/docs/public-datasets/sentinel-2}
}
\seealso{
  \code{\link{searchCopernicus}}, \code{\link{summarySearchCopernicus}},
  \code{\link{downloadProducts}}
}
\keyword{manip}
