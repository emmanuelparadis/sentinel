\name{pkm}
\alias{pkm}
\title{Probabilistic K-Means}
\description{
  Implements a probabilistic k-means method under a normal model with
  heterogeneous variance among groups.
}
\usage{
pkm(x, K, threshold = 0, iterlimit = 200)
}
\arguments{
  \item{x}{a numeric matrix.}
  \item{K}{an integer giving the number of groups (or clusters), or a
    vector of integers giving initial assignments; see details.}
  \item{threshold}{a value less than one; see details.}
  \item{iterlimit}{an integer giving the maximum number of iterations.}
}
\details{
  If \code{K} is a single value, it is interpreted as the number of
  groups. In that case, a standard k-means is first performed to provide
  initial assignments before calling the probabilistic algorithm. If
  \code{K} has length greater than one, it is taken as a vector of
  initial assignments. In that case, its length must be equal to the
  number of rows in \code{x}.

  At each iteration, the number of reclassified observations is
  printed.

  The iterations are stopped after either \code{iterlimit} iterations,
  or after a proportion less than \code{threshold} observations are
  reclassified (e.g., if \code{threshold = 0.01}, the procedure is
  stopped when less than 1\% of observations are reclassified.
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

cls2 <- pkm(X, 2) # probabilistic k-means
table(cls2, g)
}
\keyword{models}
