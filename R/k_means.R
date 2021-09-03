## k_means.R (2021-09-03)

##   Conversion Coordinates

## Copyright 2021 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

pkm <- function(x, cls, K, threshold = 1e-5, iter.lim = 200, quiet = FALSE)
{
    if (is.vector(x)) x <- as.matrix(x)
    if (length(cls) != nrow(x))
        stop("number of values in 'cls' not equal to numeber of rows in 'x'")
    PARA <- as.integer(c(K, threshold * length(cls), iter.lim, quiet))
    .Call(C_kmeans_dnorm, x, cls, PARA)
}
