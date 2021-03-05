\name{downloadProducts}
\alias{downloadProducts}
\title{Download Sentinel Products}
\description{
  Download Sentinel Products.
}
\usage{
downloadProducts(user = user, password = password,
        uuid, destdir = ".", wait = 60, engine = "wget")

}
\arguments{
  \item{user}{a character string.}
  \item{password}{a character string.}
  \item{uuid}{a vector of character strings with UUIDs of the products
    to be downloaded.}
  \item{destdir}{the destination direction (as a character string). By
    default, the current working directory is used.}
  \item{wait}{the number of minutes to wait between two successive
    attemps to download a product from Copernicus's Long Term Archive
    (LTA).}
  \item{engine}{the program used to download the files.}
}
\details{
  The values given to the argument \code{uuid} are found with the
  function \code{\link{summarySearchCopernicus}}.

  A query to download a product from the Long Term Archive (LTA)
  triggers the retrieval mechanism which takes a few hours be
  effective. Thus, the next query of the same product is done after
  waiting \code{wait} minutes to avoid too many queries on the
  Copernicus's server.
}
\value{NULL}
\author{Emmanuel Paradis}
\seealso{\code{\link{searchCopernicus}}, \code{\link{summarySearchCopernicus}}}
\keyword{manip}
