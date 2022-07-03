## utils.R (2022-05-25)

##   Utilities for Sentinel and GIS data

## Copyright 2021-2022 Emmanuel Paradis

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

## Source: http://www.physics.sfasu.edu/astro/color/spectra.html
wl2col <- function(x, gamma = 0.8, RGB = FALSE)
{
    out.of.range <- x < 380 | x > 780
    flag <- any(out.of.range)
    if (flag) {
        w <- which(!out.of.range)
        x <- x[w]
    }
    n <- as.integer(length(x))
    o <- .Fortran(wltocol, as.double(x), n, as.double(gamma), numeric(3 * n))
    res <- o[[4L]]
    dim(res) <- c(n, 3L)
    if (flag) {
        tmp <- matrix(0, length(out.of.range), 3L)
        tmp[w, ] <- res
        res <- tmp
    }
    if (RGB) {
        colnames(res) <- c("red", "green", "blue")
        if (!is.null(names(x))) rownames(res) <- names(x)
        return(res)
    }
    res <- rgb(res[, 1L], res[, 2L], res[, 3L])
    if (!is.null(names(x))) rownames(res) <- names(x)
    res
}

## Source: https://www.fourmilab.ch/documents/specrend/
spectrum2col <- function(spec, RGB = FALSE, no.warn = TRUE, color.system = 3)
{
    if (length(spec) != 81)
        stop("'spec' should have 81 values; see ?spectrum2col")
    o <- .C(C_specrend, spec, numeric(1), numeric(1), numeric(1), integer(1),
            as.integer(color.system))
    if (!no.warn) {
        if (o[[5]]) warning("computations were approximate")
    }
    if (RGB) return(c(red = o[[2]], green = o[[3]], blue = o[[4]]))
    rgb(o[[2]], o[[3]], o[[4]])
}

BlackBodySpectrum <- function(x, Temp = 300)
{
    wlm <- x * 1e-9 # nm -> m
    (3.74183e-16 / wlm^5) / (exp(1.4388e-2 / (wlm * Temp)) - 1)
}
