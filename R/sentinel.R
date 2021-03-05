## sentinel.R (2021-20-18)

##   Sentinel

## Copyright 2021 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

as.POLYGON <- function(x, y = NULL)
{
    if (is.null(y)) {
        if (is.list(x)) {
            y <- x$y
            x <- x$x
            if (length(x) != length(y))
                stop("x is a list with elements $x and $y of different lengths")
        } else if (is.matrix(x)) {
            if (ncol(x) < 2) stop("x is a matrix with less than two columns")
            y <- x[, 2]
            x <- x[, 1]
        } else stop("if y is not given, x should be a matrix or a list with  elements $x and $y")
    }
    n <- length(x)
    if (length(y) != n) stop("x and y have different lengths")
    res <- paste(c(x, x[1]), c(y, y[1]), collapse = ", ")
    class(res) <- "POLYGON"
    attr(res, "n") <- n
    res
}

print.POLYGON <- function(x, ...)
    cat("Object of class \"POLYGON\" with", attr(x, "n"), "points\n")

"[.footprint" <- function(x, ...)
{
    res <- NextMethod("[")
    class(res) <- class(x)
    res
}

plot.POLYGON <- function(x, add = FALSE, col = rgb(1, 1, 0, 0.25), xlim = NULL, ylim = NULL, ...)
{
    n <- attr(x, "n")
    x <- strsplit(gsub(",", " ", x), " +")[[1]]
    x <- as.numeric(x)
    dim(x) <- c(2, n)
    x <- t(x)
    if (!add) {
        if (is.null(xlim)) {
            xlim <- range(x[, 1])
            xlim <- xlim + c(-1, 1) * diff(xlim)/2
        }
        if (is.null(ylim)) {
            ylim <- range(x[, 2])
            ylim <- ylim + c(-1, 1) * diff(ylim)/2
        }
        maps::map("worldHires", xlim = xlim, ylim = ylim,
                  xlab = "Longitude", ylab = "Latitude")
    }
    polygon(x, col = col, ...)
    axis(1)
    axis(2)
    box()
}

buildPOLYGON <- function(bottomleftcorner, toprighcorner, interactive = FALSE,
                         xlim = c(-180, 180), ylim = c(-90, 90))
{
    if (interactive) {
        maps::map("worldHires", xlim = xlim, ylim = ylim)
        cat("Click on points to define polygon. Right-click when finished\n")
        xy <- locator()
        return(as.POLYGON(xy))
    }
    x0 <- bottomleftcorner[1]
    y0 <- bottomleftcorner[2]
    x1 <- toprighcorner[1]
    y1 <- toprighcorner[2]
    res <- paste(paste(x0, y0), paste(x0, y1), paste(x1, y1),
                 paste(x1, y0), paste(x0, y0), sep = ", ")
    class(res) <- "POLYGON"
    attr(res, "n") <- 5L
    res
}

.sec2str <- function(pt) # pt: processing time in seconds
{
    S <- round(pt %% 60, 1)
    res <- paste(S, "sec")
    if (pt < 60) return(res)
    M <- pt %/% 60
    if (M >= 60) {
        H <- M %/% 60
        M <- M %% 60
        res <- paste(M, "min", res)
    } else return(paste(M, "min", res))
    if (H >= 24) {
        D <- H %/% 24
        H <- H %% 24
        res <- paste(H, "hr", res)
    } else return(paste(H, "hr", res))
    paste(D, "d", res)
}

searchCopernicus <- function(user, password, checkID = FALSE, ask = 1000,
             POLYGON = NULL, productType = NULL, quiet = FALSE, engine = "wget")
{
    isNo <- function(x)
        toupper(substr(sub("[[:blank:]]+", "", x), 1, 1)) == "N"
    if (missing(user)) {
        if (!exists("user", envir = .GlobalEnv) || !exists("password", envir = .GlobalEnv)) checkID <- TRUE
        else {
            user <-  get("user", envir = .GlobalEnv)
            password <-  get("password", envir = .GlobalEnv)
        }
    }
    if (checkID) {
        enter <- function(what) {
            cat("Please enter ", what, ": ", sep = "")
            readLines(n = 1)
        }
        if (exists("user", envir = .GlobalEnv)) {
            user <- get("user", envir = .GlobalEnv)
            cat("Found user name: ", user, "\nDo you want to use it? [Y/n] ", sep = "")
            ans <- readLines(n = 1)
            if (isNo(ans)) {
                user <- enter("user name")
                password <- enter("password")
            } else {
                if (exists("password", envir = .GlobalEnv)) {
                    password <- get("password", envir = .GlobalEnv)
                    cat("Found password: ------------\nDo you want to use it? [Y/n] ", sep = "")
                    ans <- readLines(n = 1)
                    if (isNo(ans)) password <- enter("password")
                }
            }
        } else {
            user <- enter("user name")
            password <- enter("password")
        }
    }
    od <- setwd(tempdir())
    on.exit(setwd(od))
    STATS <- get("searchCopernicusStats", envir = .sentinelEnv)
    prefix <- paste0("sentinel_search", nrow(STATS) + 1L, "_")

    s1 <- paste0(engine, " -q --no-check-certificate --user=", user, " --password='", password, "' --output-document=", prefix)
    s2 <- paste0(".xml 'https://scihub.copernicus.eu/dhus/search?q=")
    if (is.null(POLYGON) && is.null(productType)) {
        s2 <- paste0(s2, "*")
    } else {
        if (!is.null(POLYGON)) s2 <- paste0(s2, "footprint:\"Intersects(POLYGON((", POLYGON, ")))\"")
        if (!is.null(productType)) {
            list.of.product <-
                c("SLC", "GRD", "OCN", "S2MSI1C", "S2MSI2A", "S2MSI2Ap",
                  "SR1_SRA___", "SR1_SRA_A_", "SR1_SRA_BS", "SR2_LAN___",
                  "OL_1_EFR___", "OL_1_ERR___", "OL_2_LFR___", "OL_2_LRR___",
                  "SL_1_RBT___", "SL_2_LST___", "SY_2_SYN___", "SY_2_V10___",
                  "SY_2_VG1___", "SY_2_VGP___")
            if (! productType %in% list.of.product)
                stop(paste("option 'productType' must be one of:",
                           paste(list.of.product, collapse = ", ")))
            s2 <- paste0(s2, "+producttype:", productType, "&")
        }
    }
    s2 <- paste0(s2, "&start=")
    s3 <- "&rows=100'"

    t0 <- proc.time()[3]
    ## loop to get all the metadata files
    i <- 1
    repeat {
        cmd <- paste0(s1, i, s2, (i - 1) * 100, s3)
        fl <- paste0(prefix, i, ".xml")
        system(cmd)
        if (i == 1) {
            x <- scan(fl,  what = "", sep = "\n", skip = 2, n = 1, quiet = TRUE)
            N <- as.numeric(gsub("total.*$", "", gsub("^.*of", "", x)))
            if (!length(N) || is.na(N)) {
                message("found no result in your query")
                return(invisible(NULL))
            }
            Nfiles <- ceiling(N/100)
            if (!quiet)
                cat("\nDownloading metadata on", N, "Sentinel products...\n\n")
        }
        if (N > ask) {
            cat("There are more than", ask, "products in your query.\nDo you want to continue? [Y/n] ")
            ans <- readLines(n = 1)
            if (isNo(ans)) {
                Nfiles <- 1
                break
            } else ask  <- Inf
        }
        i <- i + 1
        if (i > Nfiles) { # <- this limit is found on line 3 of each file
            if (!quiet) cat("\r100 %\n")
            break
        }
        if (!quiet) cat("\r", round(100*i/Nfiles), "%")
    }
    t1 <- proc.time()[3]
    if (!quiet) cat("\nProcessing time:", .sec2str(t1 - t0), "\n")
    if (is.na(Nfiles)) Nfiles <- 0
    if (is.na(N)) N <- 0
    STATS <- rbind(STATS, data.frame(Nfiles, N))
    assign("searchCopernicusStats", STATS, envir = .sentinelEnv)
    invisible(1L)
}

.getXfromSearch <- function(search) {
    od <- setwd(tempdir())
    on.exit(setwd(od))
    fls <- dir(pattern = paste0("^sentinel_search", search, "_.*\\.xml"))
    if (!length(fls)) return(NULL)
    X <- character()
    for (fl in fls)
        X <- c(X, scan(fl, what = "", sep = "\n", quiet = TRUE))
    X
}

summarySearchCopernicus <- function(search = 1, level = 1)
{
    if (!level %in% 1:3) stop("'level' should be 1, 2, or 3")
    X <- .getXfromSearch(search)
    if (is.null(X)) {
        warning(paste("no file found for search", search, "(maybe the cache was cleaned)"))
        return(NULL)
    }
    i <- grep("</summary>", X)
    Z <- gsub("<(/|)summary>", "", X[i])
    cat(Z, file = "Z", sep = "\n")
    Z <- read.csv("Z", header = FALSE)
    unlink("Z")
    ncolZ <- ncol(Z)
    names(Z) <- if (ncolZ == 4) c("DateTime", "Instrument", "Satellite", "Size") else c("DateTime", "Instrument", "Mode", "Satellite", "Size")
    Z$DateTime <- as.POSIXct(Z$DateTime, format = "Date: %Y-%m-%dT%H:%M:%OS")
    ZInstrument <- gsub(" Instrument: ", "", Z$Instrument)
    Z$Satellite <- gsub(" Mode: ", "", Z$Satellite)
    if (ncolZ == 5) Z$Mode <- gsub(" Satellite: ", "", Z$Mode)
    Z$Size <- gsub(" Size: ", "", Z$Size)


    if (level == 1) return(Z)

    getVar <- function(name, type) {
        ##ix <- grep(name, X)
        ix <- grep(paste0("<", type, " name=\"", name, "\">"), X)
        strg <- paste0("<", type, " name=\"", name, "\">|</", type, ">")
        VAR <- gsub(strg, "", X[ix])
        if (type != "str") VAR <- as.numeric(VAR)
        k <- integer()
        for (i in ix) {
            j <- i
            while(is.na(match(j, ix))) j <- j - 1
            k <- c(k, j)
        }
        def <- switch(type, "double" = NA_real_, "int" = NA_integer_, "str" = NA_character_)
        res <- rep(def, nrow(Z))
        res[which(ix %in% k)] <- VAR
        res
    }

    Z$cloudcoverpercentage <- getVar("cloudcoverpercentage", "double")
    Z$landpercentage <- getVar("landpercentage", "int")
    Z$sensoroperationalmode <- getVar("sensoroperationalmode", "str")
    Z$producttype <- getVar("producttype", "str")

    ## get file sizes:
    tmp <- unlist(strsplit(Z$Size, " "))
    tmp <- matrix(tmp, ncol = 2, byrow = TRUE)
    ## in in GB:
    Z$filesize.GB <- as.numeric(tmp[, 1]) * ifelse(tmp[, 2] == "MB", 1e-3, 1)

    if (level == 3) Z$uuid <- getVar("uuid", "str")

    Z
}

getFootprint <- function(search = 1)
{
    X <- .getXfromSearch(search)
    if (is.null(X)) {
        warning(paste("no file found for search", search, "(maybe the cache was cleaned)"))
        return(NULL)
    }
    FOOT <- grep("<str name=\"footprint\">", X, fixed = TRUE, value = TRUE)
    ## FOOT <- X[i]
    FOOT <- gsub("<str name=\"footprint\">", "", FOOT, fixed = TRUE)
    FOOT <- gsub("POLYGON|MULTIPOLYGON", "", FOOT)
    FOOT <- gsub("</str>", "", FOOT, fixed = TRUE)
    FOOT <- gsub("\\(*\\)*", "", FOOT)
    FOOT <- gsub("^ *", "", FOOT)
    FOOT <- gsub(" *$", "", FOOT)
    class(FOOT) <- "footprint"
    FOOT
}

print.footprint <- function(x, ...)
    cat("Footprint on", length(x), "products\n")

plot.footprint <- function(x, what = 1:100, xlim = NULL, ylim = NULL,
                           col = rgb(1, 1, 0, .2), border = "black", ...)
{
    if (length(x) < max(what)) what <- seq_along(x)
    foo <- function(x) {
        foot <- as.numeric(strsplit(gsub(",", " ", x), " +")[[1]])
        matrix(foot, ncol = 2, byrow = TRUE)
    }
    xy <- lapply(x, foo)
    if (is.null(xlim)) {
        xlim <- range(unlist(lapply(xy, function(x) x[, 1])))
        xlim <- xlim + c(-1, 1) * diff(xlim)/2
    }
    if (is.null(ylim)) {
        ylim <- range(unlist(lapply(xy, function(x) x[, 2])))
        ylim <- ylim + c(-1, 1) * diff(ylim)/2
    }
    maps::map("worldHires", xlim = xlim, ylim = ylim)
    axis(1); axis(2); box()
    for (i in what) polygon(xy[[i]], col = col, border = border, ...)
}

## area of a polygon
areaPolygon <- function(x, y = NULL)
{
    if (is.null(y)) {
        if (ncol(x) != 2)
            stop("x must have 2 columns if y is not given")
        y <- x[, 2]
        x <- x[, 1]
    } else {
        if (length(x) !=  length(y))
            stop("x and y must have the same length")
    }
    n <- length(x)
    abs(sum(x[-n] * y[-1] - x[-1] * y[-n]) + x[n] * y[1] - x[1] * y[n])/2
}

overlap <- function(POLYGON, footprint)
{
    matrix2PolySet <- function(x) {
        n <- nrow(x)
        PBSmapping::as.PolySet(data.frame(PID = rep(1, n), POS = 1:n,
                                          X = x[, 1], Y = x[, 2]))
    }
    foo <- function(x) {
        foot <- as.numeric(strsplit(gsub(",", " ", x), " +")[[1]])
        matrix(foot, ncol = 2, byrow = TRUE)
    }
    A <- foo(POLYGON)
    Ach <- A[chull(A), ]
    n <- length(footprint)
    res <- numeric(n)
    for (i in 1:n) {
        B <- foo(footprint[i])
        Bch <- B[chull(B), ]
        ## get the intersection:
        C <- PBSmapping::joinPolys(matrix2PolySet(Ach),
                                   matrix2PolySet(Bch))[, 3:4]
        res[i] <- areaPolygon(C)/areaPolygon(Ach) # should be <= 1
    }
    res
}

downloadProducts <-
    function(user = user, password = password,
             uuid, destdir = ".", wait = 60, engine = "wget")
{
    od <- setwd(destdir)
    on.exit(setwd(od))
    s1 <- paste0("--content-disposition --continue --user=", user, " --password='", password, "' \"https://scihub.copernicus.eu/dhus/odata/v1/Products('")
    s3 <- "')/\\$value\""
    nDown <- n500 <- nLTA <- 0L
    for (i in uuid) {
        arg <- paste0(s1, i, s3)
        repeat {
            outcmd <- system2(engine, arg, stdout = TRUE, stderr = TRUE)
            if (any(grepl("ERROR 500: Internal Server Error", outcmd))) {
                Sys.sleep(20) # avoid too many requests error
                n500 <- n500 + 1L
                break
            }
            if (any(grepl("The file is already fully retrieved", outcmd))) break
            if (identical(outcmd, 0L)) {
                nDown <- nDown + 1L
                break
            }
            if (wait) {
                Sys.sleep(wait * 60)
            } else {
                nLTA <- nLTA + 1L
                break
            }
        }
    }
    cat(nDown, " products downloaded successfully\n",
        n500, " products with ERROR 500\n",
        nLTA, "products unsuccessfully retrieved from LTA\n", sep = "")
}

overviewSearchCopernicus <- function()
{
    DF <- get("searchCopernicusStats", envir = .sentinelEnv)
    if (!nrow(DF)) cat("No previous research on Copernicus.\n") else DF
}

clearXMLcache <- function()
{
    od <- setwd(tempdir())
    on.exit(setwd(od))
    unlink("sentinel_search*.xml")
}
