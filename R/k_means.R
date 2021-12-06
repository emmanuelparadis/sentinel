## k_means.R (2021-09-03)

##   Probabilistic K-Means

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

skm <- function(x, cls, K, algorithm = "Hartigan-Wong", iter.lim = 10, quiet = FALSE)
{
    if (is.vector(x)) x <- as.matrix(x)
    if (missing(cls)) {
        if (missing(K)) stop("arguments 'cls' and 'K' cannot be both missing")
        if (length(K) > 1) {
            warning("argument 'K' is longer than one: taking its 1st value")
            K <- K[1]
        }
        cls <- K
    } else {
        if (length(cls) != nrow(x))
            stop("number of values in 'cls' not equal to numeber of rows in 'x'")
    }
    kmeans(x, cls, iter.lim, algorithm = algorithm, trace = quiet)$cluster
}
