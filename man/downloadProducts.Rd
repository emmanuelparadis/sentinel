\name{downloadProducts}
\alias{downloadProducts}
\title{Download Sentinel Products}
\description{
  Download Sentinel Products.
}
\usage{
downloadProducts(user = user, password = password, uuid, destdir = ".",
                 engine = "wget", logfile = NULL, quiet = FALSE)
}
\arguments{
  \item{user}{a character string.}
  \item{password}{a character string.}
  \item{uuid}{a vector of character strings with UUIDs of the products
    to be downloaded.}
  \item{destdir}{the destination direction (as a character string). By
    default, the current working directory is used.}
  \item{engine}{the program used to download the files.}
  \item{logfile}{file where the result for each product is written.}
  \item{quiet}{a logical value specifying whether the progress of the
    computations should be printed or not.}
}
\details{
  The values given to the argument \code{uuid} are found with the
  function \code{\link{summarySearchCopernicus}}.

  A query to download a product from the Long Term Archive (LTA)
  triggers the retrieval mechanism which takes a few hours be
  effective.
}
\value{NULL}
\author{Emmanuel Paradis}
\seealso{
  \code{\link{searchCopernicus}}, \code{\link{summarySearchCopernicus}},
  \code{\link{downloadGcloud}}
}
\keyword{manip}
