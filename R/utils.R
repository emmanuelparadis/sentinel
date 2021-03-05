## utils.R (2021-01-18)

##   Utilities for Sentinel and GIS data

## Copyright 2021 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

.check2cols <- function(x, y)
{
    if (is.null(y)) {
        ncx <- ncol(x)
        if (is.null(ncx) || ncx < 2)
            stop(paste(sQuote(deparse(substitute(x))),
                       "should have at least 2 columns if",
                       sQuote(deparse(substitute(y))), "is not given"),
                 call. = FALSE)
        y <- x[, 2L]
        x <- x[, 1L]
    } else {
        if (length(x) != length(y))
            stop(paste(sQuote(deparse(substitute(x))), "and",
                       sQuote(deparse(substitute(y))),
                       "should have the same length"), call. = FALSE)
    }
    list(x, y) # no names for faster computation
}

area <- function(x, y = NULL)
{
    xy <- .check2cols(x, y)
    x <- xy[[1L]]
    y <- xy[[2L]]
    n <- length(x)
    res <- 0
    for (i in seq_len(n - 1)) {
        j <- i + 1L
        res <- res + x[i] * y[j] - x[j] * y[i]
    }
    res <- res + x[n] * y[1] - x[1] * y[n]
    abs(res) / 2
}

## from https://gist.github.com/friendly/67a7df339aa999e2bcfcfec88311abfc
## vectorized and object renamed (x: vector of wavelengths in nanometer)
wl2rgb <- function(x, gamma = 0.8)
{
    R <- G <- B <- rep(0, length(x))

    s <- x >= 380 & x <= 440
    if (any(s)) {
        attenuation <- 0.3 + 0.7 * (x[s] - 380) / (440 - 380)
        R[s] <- ((-(x[s] - 440) / (440 - 380)) * attenuation) ^ gamma
        G[s] <- 0
        B[s] <- attenuation ^ gamma
    }

    s <- x >= 440 & x <= 490
    if (any(s)) {
        R[s] <- 0
        G[s] <- ((x[s] - 440) / (490 - 440)) ^ gamma
        B[s] <- 1
    }

    s <- x >= 490 & x <= 510
    if (any(s)) {
        R[s] <- 0
        G[s] <- 1
        B[s] <- (-(x[s] - 510) / (510 - 490)) ^ gamma
    }

    s <- x >= 510 & x <= 580
    if (any(s)) {
        R[s] <- ((x[s] - 510) / (580 - 510)) ^ gamma
        G[s] <- 1
        B[s] <- 0
    }

    s <- x >= 580 & x <= 645
    if (any(s)) {
        R[s] <- 1
        G[s] <- (-(x[s] - 645) / (645 - 580)) ^ gamma
        B[s] <- 0
    }

    s <- x >= 645 & x <= 750
    if (any(s)) {
        attenuation <- 0.3 + 0.7 * (750 - x[s]) / (750 - 645)
        R[s] <- attenuation ^ gamma
        G[s] <- B[s] <- 0
    }

    rgb(floor(R * 255), floor(G * 255), floor(B * 255), maxColorValue = 255)
}

rose <- function(x, y, size = 1, width = size/4, cols = c("grey10", "white"),
                 labels = c("N", "S", "E", "W"), offset = 0, ...)
{
    w2 <- width / 2

    xx <- c(x, x, x + w2, NA, x, x + size, x + w2, NA,
            x, x, x - w2, NA, x, x - size, x - w2)
    yy <- c(y, y + size, y + w2, NA, y, y, y - w2, NA,
            y, y - size, y - w2, NA, y, y, y + w2)
    polygon(xx, yy, col = cols[2])
    xx <- c(x, x, x - w2, NA, x, x + size, x + w2, NA,
            x, x, x + w2, NA, x, x - size, x - w2)
    yy <- c(y, y + size, y + w2, NA, y, y, y + w2, NA,
            y, y - size, y - w2, NA, y, y, y - w2)
    polygon(xx, yy, col = cols[1])
    off <- strwidth("M")/2 + offset
    xx <- c(x, x, x + size + off, x - size - off)
    yy <- c(y + size + off, y - size - off, y, y)
    text(xx, yy, labels, ...)

}
