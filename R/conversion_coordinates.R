## conversion_coordinates.R (2024-01-28)

##   Conversion Coordinates

## Copyright 2020-2024 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

getUTMzone <- function(lon, lat = NULL)
{
    xy <- .check2cols(lon, lat)
    lon <- xy[[1L]]
    lat <- xy[[2L]]
    z <- getUTMzone2(lon)
    ## Special zones for Svalbard and Norway
    if (any(s <- lat >= 72 & lat < 84 & lon >= 0 & lon < 9)) z[s] <- 31L
    if (any(s <- lon >= 9 & lon < 21)) z[s] <- 33L
    if (any(s <- lon >= 21 & lon < 33)) z[s] <- 35L
    if (any(s <- lon >= 33 & lon < 42)) z[s] <- 37L
    z # should be always integer
}

## simpler and faster version
## -> to be used only outside Svalbard and Norway
getUTMzone2 <- function(lon) as.integer(floor((lon + 180) / 6) %% 60) + 1L

getUTMhemisphere <- function(lat)
    c("south", "north")[as.integer(lat > 0) + 1L]

.lonlat2utm <- function(X, crs) spTransform(X, CRS(crs))

## returns a DF containing the UTM values, and
## optionally: zone, hemisphere, and prefix
lonlat2UTM <- function(lon, lat = NULL, details = FALSE)
{
    DF <- .check2cols(lon, lat)
    names(DF) <- c("x", "y")
    DF <- as.data.frame.list(DF, row.names = seq_along(DF$x))
    DF$id <- row.names(DF)
    coordinates(DF) <- c("x", "y")
    hemisphere <- getUTMhemisphere(DF$y)
    zone <- getUTMzone(DF$x, DF$y)
    crs <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84", " +", hemisphere, " +units=m")
    proj4string(DF) <- CRS("+init=epsg:4326")
    Zones <- unique(crs)
    Nzones <- length(Zones)
    if (Nzones > 1) {
        warning("more than one UTM zone: returning details in data frame")
        res <- vector("list", Nzones)
        for (i in 1:Nzones) {
            z <- Zones[i]
            s <- which(crs == z)
            o <- .lonlat2utm(DF[s, ], z)
            o$zone <- zone[s]
            o$hemisphere <- hemisphere[s]
            o$prefix <- paste0(zone[s], toupper(substr(hemisphere[s], 1, 1)))
            res[[i]] <- o
        }
        res <- do.call(rbind, res)
    } else {
        res <- .lonlat2utm(DF, crs[1])
        if (details) {
            res$zone <- zone
            res$hemisphere <- hemisphere
            res$prefix <- paste0(zone, toupper(substr(hemisphere, 1, 1)))
        }
    }
    res@data$id <- NULL
    res
}

## !!! all points must be in the same UTM zone and hemisphere
UTM2lonlat <- function(X, zone, hemisphere)
{
    crs <- paste0("+proj=utm +zone=", zone, " +", hemisphere)
    XY <- SpatialPoints(X, proj4string = CRS(crs))
    spTransform(XY, CRS("+init=epsg:4326"))
}

lonlat2ECEF <- function(lon, lat = NULL, alt = 0, as.matrix = TRUE)
{
    xy <- .check2cols(lon, lat)
    lon <- xy[[1L]]
    lat <- xy[[2L]]
    a <- 6378137 # equatorial radius (m)
    b <- 6356752 # polar radius (m)
    b2a2 <- b^2/a^2
    e2 <- 1 - b2a2
    deg2rad <- pi/180
    lon <- lon * deg2rad
    lat <- lat * deg2rad
    clat <- cos(lat)
    slat <- sin(lat)
    clon <- cos(lon)
    slon <- sin(lon)

    N <- a / sqrt(1 - e2 * slat^2)

    x <- (N + alt) * clat * clon
    y <- (N + alt) * clat * slon
    z <- (b2a2 * N + alt) * slat
    if (as.matrix) cbind(x = x, y = y, z = z) else list(x = x, y = y, z = z)
}

ECEF2lonlat <- function(x, y = NULL, z = NULL)
{
    if (is.null(y) & is.null(z))
        x <- cbind(x, y, z)
    if (ncol(x) < 3) stop("'x' should have at least 3 columns")
    res <- .Call(ECEF2lonlat_Call, x)
    colnames(res) <- c("Lon", "Lat", "Alt")
    res
}
