\name{pkm}
\alias{pkm}
\title{Probabilistic K-Means}
\description{
  Implements a probabilistic k-means method under a normal model with
  heterogeneous variance among groups.
}
\usage{
pkm(x, cls, K, threshold = 1e-5, iter.lim = 200, quiet = FALSE)
}
\arguments{
  \item{x}{a numeric matrix.}
  \item{cls}{a vector of class membership for each observation in
    \code{x} (e.g., output from \code{kmeans}).}
  \item{K}{an integer giving the number of groups (or clusters); should
    be equal to \code{length(unique(cls))}.}
  \item{threshold}{a value less than one; see details.}
  \item{iter.lim}{an integer giving the maximum number of iterations.}
  \item{quiet}{a logical value specifying whether the progress of the
    computations should be printed or not.}
}
\details{
  The iterations are stopped after either \code{iter.lim} iterations,
  or after a proportion less than \code{threshold} observations are
  reclassified (e.g., if \code{threshold = 0.01}, the procedure is
  stopped when less than 1\% of observations are reclassified (the
  default is 0.001\%).
}
\value{a vector of integers giving the final group assignments.}
\author{Emmanel Paradis}
\seealso{\code{\link[stats]{kmeans}}}
\examples{
set.seed(20)
## simulate two groups with n = 200:
n <- 200
x1 <- rnorm(n, 0, 2)  # ~ N(0, 2)
x2 <- rnorm(n, 6, .5) # ~ N(6, 0.5)
g <- gl(2, n) # the actual groups are known

X <- c(x1, x2) # combine the data in a single vector
km <- kmeans(X, 2) # standard k-means
cls <- km$cluster
table(g, cls)

cls2 <- pkm(X, cls, 2) # probabilistic k-means
table(cls2, g)
}
\keyword{models}
