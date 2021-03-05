## k_means.R (2021-03-05)

##   Conversion Coordinates

## Copyright 2021 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

pkm <- function(x, K, threshold = 0, iterlimit = 200)
{
    nK <- length(K)
    if (nK == 1) {
        K <- as.integer(K)
        cls0 <- kmeans(x, K, trace = TRUE)$cluster
    } else {
        if (nrow(x) != nK)
            stop("number of values in 'K' should be equal to number of observations")
        cls0 <- as.integer(K)
        K <- nK
    }
    .Call(C_kmeans_dnorm, x, cls0, K, threshold, as.integer(iterlimit))
}
