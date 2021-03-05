\name{wl2rgb}
\alias{wl2rgb}
\title{Wavelengths to Colours}
\description{
  Conversion from wavelengths to colours.
}
\usage{
wl2rgb(x, gamma = 0.8)
}
\arguments{
  \item{x}{the wavelength in nanometers (nm).}
  \item{gamma}{gamma parameter.}
}
\value{
  a matrix with the integer values (between 0 and 255) of red, green,
  and blue arranged in a matrix with  three columns.
}
\author{Emmanuel Paradis}
\keyword{manip}
