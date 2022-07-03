\name{searchCopernicus}
\alias{searchCopernicus}
\title{Search the Copernicus SciHub}
\description{
  Search the Copernicus SciHub and returns the results in a data frame.
}
\usage{
searchCopernicus(user, password, checkID = FALSE, ask = 1000,
                 POLYGON = NULL, productType = NULL, quiet = FALSE,
                 engine = "wget")
}
\arguments{
  \item{user}{a character string.}
  \item{password}{a character string.}
  \item{checkID}{a logical value specifying whether to check the user
    and password identifiers; see details.}
  \item{ask}{an integer: if the search finds more than \code{ask}
    products, the user is asked to continue or not (see details).}
  \item{POLYGON}{an object of class \code{"POLYGON"}.}
  \item{productType}{a character string giving the product type(s).}
  \item{quiet}{a logical value.}
  \item{engine}{the program used to download the XML files.}
}
\details{
  The user must be registered on Copernicus SciHub. If \code{checkID =
    FALSE} and there are two objects named \code{user} and
  \code{password} that \R can found, these two are used as the first two
  arguments of the function.

  The downloaded XML files contain information on 100 products each, so
  the default value of \code{ask} is equivalent to download not more
  than 10 XML files without asking the user. Note that the first XML
  file which contains the information on the number of found products
  is always downloaded.

  A search without argument (i.e., \code{searchCopernicus()} providing
  \code{user} and \code{password} have been set as explained above)
  returns information on all Sentinel products available on
  Copernicus. As of today (2022-06-01), more than 50,100,000 products
  (with about one product added every minute). A full download will thus
  get 510,000 files each about 370 KB in size (total around 189 GB).
}
\value{a data frame.}
\author{Emmanuel Paradis}
\seealso{\code{\link{summarySearchCopernicus}}, \code{\link{buildPOLYGON}}}
\keyword{manip}
