## processS2.R (2024-01-09)

##   Process Sentinel-2 Raw Data

## Copyright 2024 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

aggregateS2 <- function(x, finalreso = 60)
{
    if (!finalreso %in% c(20, 60))
        stop("'finalresolution' must be 20 or 60")

    n10m <- 120560400 # Nb pixels at 10-m resolution
    n20m <- 30140100  #              20

    n <- length(x)
    if (!n %in% c(n10m, n20m))
        stop("'x' seems to be of the wrong length")

    if (finalreso == 20 && n == n10m) res <- .Call(aggregate_sen2_10to20, x)
    if (finalreso == 60 && n == n10m) res <- .Call(aggregate_sen2_10to60, x)
    if (finalreso == 60 && n == n20m) res <- .Call(aggregate_sen2_20to60, x)

    res
}

interpol <- function(x)
{
    NR <- NC <- as.integer(sqrt(length(x)))
    o <- .Call(bilinear_interpolation_Call, x, c(NR, NC))
    matrix(o, NR*2, NC*2, TRUE)
}

tabulateS2 <- function(x)
{
    .Call(Call_tabulate_sentinel, x)
}
