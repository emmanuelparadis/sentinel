## processS2.R (2024-01-05)

##   Process Sentinel-2 Raw Data

## Copyright 2024 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

aggregateS2 <- function(x, finalreso = 60)
{
    if (!finalreso %in% c(20, 60))
        stop("'finalresolution' must be 20 or 60")

    if (length(x) != 120560400)
        stop("'x' seems to be of the wrong length (!= 120,560,400 pixels)")

    if (finalreso == 20) .Call(aggregate_sen2_10to20, x) else .Call(aggregate_sen2_10to60, x)
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
