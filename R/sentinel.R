## sentinel.R (2024-01-05)

##   Sentinel

## Copyright 2021-2024 Emmanuel Paradis

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

plot.POLYGON <- function(x, add = FALSE, col = rgb(1, 1, 0, 0.25), xlim = NULL, ylim = NULL, around = 1, ...)
{
    n <- attr(x, "n")
    x <- strsplit(gsub(",", " ", x), " +")[[1]]
    x <- as.numeric(x)
    dim(x) <- c(2, n)
    x <- t(x)
    if (!add) {
        if (is.null(xlim)) {
            xlim <- range(x[, 1])
            xlim <- xlim + c(-around, around)
        }
        if (is.null(ylim)) {
            ylim <- range(x[, 2])
            ylim <- ylim + c(-around, around)
        }
        repeat {
            o <- try(maps::map("worldHires", xlim = xlim, ylim = ylim,
                               xlab = "Longitude", ylab = "Latitude"),
                     silent = TRUE)
            if (!inherits(o, "try-error")) break
            message("expanding 'xlim' and 'ylim' to avoid error from map()")
            xlim <- xlim + c(-1, 1) * diff(xlim)/2
            ylim <- ylim + c(-1, 1) * diff(ylim)/2
        }
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
            POLYGON = NULL, productType = NULL, quiet = FALSE, engine = "wget",
            startDate = NULL, endDate = NULL)
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
### https://scihub.copernicus.eu/userguide/FullTextSearch
    if (!is.null(startDate) || !is.null(endDate)) {
        if (is.null(startDate)) startDate <- "2014-01-01"
        if (is.null(endDate)) endDate <- "NOW"
        timespan <- paste0("+ingestiondate:[", startDate, "T00:00:00.000Z TO ", endDate, "T00:00:00.000Z]")
###        s2 <- paste0(s2, timespan, "&&start=")
        s2 <- paste0(s2, timespan, "&")

    }# else {
    s2 <- paste0(s2, "&start=")
###    }
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
    if (!level %in% 1:4) stop("'level' should be 1, 2, 3, or 4")
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
    Z$DateTime <- as.POSIXct(Z$DateTime, format = "Date: %Y-%m-%dT%H:%M:%OS",
                             tz = "UTC")
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
        n <- nrow(Z)
        if (length(VAR) == n) return(VAR)
        def <- switch(type, "double" = NA_real_, "int" = NA_integer_, "str" = NA_character_)
        if (!length(VAR)) return(rep(def, n))
        if (length(VAR) > n) {
            warning("too many values found in XML files: return NULL")
            return(NULL)
        }
        if (length(VAR) < n) {
            start.entry <- grep("<entry>", X)
            end.entry <- grep("</entry>", X)
            w <- sapply(ix, function(x) which(x > start.entry & x < end.entry))
            if (!is.atomic(w)) {
                warning("issues in XML files: return NULL")
                return(NULL)
            }
            res <- rep(def, n)
            res[w] <- VAR
        }
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

    if (level == 2) return(Z)

    Z$uuid <- getVar("uuid", "str")
    if (level == 3) return(Z)

    Z$filename <- getVar("filename", "str")
    Z$granuleidentifier <- getVar("granuleidentifier", "str")
    Z$datastripidentifier <- getVar("datastripidentifier", "str")
    Z
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

.footprint2xy <- function(x)
{
    foot <- as.numeric(strsplit(gsub(",", " ", x), " +")[[1]])
    matrix(foot, ncol = 2, byrow = TRUE)
}

plot.footprint <- function(x, xlim = NULL, ylim = NULL, col = rgb(1, 1, 0, 0.1),
                           border = "black", around = 1, ...)
{
    xy <- lapply(x, .footprint2xy)
    if (is.null(xlim)) {
        xlim <- range(unlist(lapply(xy, function(x) x[, 1])))
        xlim <- xlim + c(-around, around)# * diff(xlim) / 2
    }
    if (is.null(ylim)) {
        ylim <- range(unlist(lapply(xy, function(x) x[, 2])))
        ylim <- ylim + c(-around, around)# * diff(ylim) / 2
    }
    maps::map("worldHires", xlim = xlim, ylim = ylim)
    axis(1); axis(2); box()
    for (z in xy) polygon(z, col = col, border = border, ...)
}

lines.footprint <- function(x, col = rgb(0, 1, 0, 0.1), border = "black", ...)
{
    xy <- lapply(x, .footprint2xy)
    for (z in xy) polygon(z, col = col, border = border, ...)
}

points.footprint <- lines.footprint

## internal function only called below (x is a 2-col matrix)
areaPolygon <- function(x, y = NULL)
{
    xy <- .check2cols(x, y)
    tigers::area(xy$x, xy$y)
}

overlap <- function(POLYGON, footprint)
{
    foo <- function(x) {
        foot <- as.numeric(strsplit(x, ",* +")[[1]])
        matrix(foot, ncol = 2, byrow = TRUE)
    }
    bar <- function(xy) {
        if (!is.clockwise(xy)) xy <- revPolygon(xy, FALSE)
        if (!is.open(xy)) xy <- xy[-nrow(xy), , drop = FALSE]
        xy
    }
    A <- foo(POLYGON)
    A <- bar(A)
    Ach <- A[chullPolygon(A), ]
    n <- length(footprint)
    res <- numeric(n)
    for (i in 1:n) {
        B <- foo(footprint[i])
        B <- bar(B)
        Bch <- B[chullPolygon(B), ]
        ## get the intersection (Ach and Bch are clearly convex):
        C <- convexPolygonOverlap(Ach, Bch)
        res[i] <- areaPolygon(C)/areaPolygon(Ach) # should be <= 1
    }
    res
}

downloadProducts <-
    function(user = user, password = password, uuid, destdir = ".",
             engine = "wget", logfile = NULL, quiet = FALSE)
{
    od <- setwd(destdir)
    on.exit(setwd(od))
    if (is.null(logfile)) {
        num <- 1L
        fls <- dir(pattern = "^Copernicus[0-9]{3}\\.txt$")
        if (length(fls))
            num <- max(as.integer(substr(fls, 11, 13))) + 1L
        logfile <- sprintf("Copernicus%03d.txt", num)
    }
    ## https://scihub.copernicus.eu/userguide/DataRestoration
    ## HTTP codes are at the bottom of the above Web page.
    ## See also: https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
    cat("## ", date(), " This file can be read with read.table(\"", logfile, "\", header = TRUE)\n",
        "## Codes (column 'Result'):\n",
        "##   200: download successful\n",
        "##   202: successfully submitted to long-term archive (LTA) for retrieval\n",
        "##   403: quota exceeded (forbidden)\n",
        "##   404: not found\n",
        "##   416: already downloaded (nothing to do)\n",
        "##   500: internal server error\n",
        "##   503: service unavailable\n",
        "uuid\tResult\n", sep = "", file = logfile)
    s1 <- paste0("--content-disposition --continue --user=", user, " --password='", password, "' \"https://scihub.copernicus.eu/dhus/odata/v1/Products('")
    s3 <- "')/\\$value\""
    n <- integer(7)
    names(n) <- c("200", "202", "503", "403", "404", "416", "500")
    Nproducts <- length(uuid)
    k <- 0L
    if (!quiet) {
        cat(sprintf("Downloading %d %s from Copernicus SciHub...\n",
                    Nproducts, if (Nproducts > 1) "files" else "file"))
        if (!quiet) cat("\r", k, "/", Nproducts)
    }
    for (i in uuid) {
        k <- k + 1L
        arg <- paste0(s1, i, s3)
        outcmd <- system2(engine, arg, stdout = TRUE, stderr = TRUE)
        if (!quiet) cat("\r", k, "/", Nproducts)
        prefix <- "[[:space:]]*HTTP request sent, awaiting response...[[:space:]]*"
        lastHTTPcode <- tail(grep(prefix, outcmd, value = TRUE), 1)
        lastHTTPcode <- gsub("[[:space:]]*", "", gsub(prefix, "", lastHTTPcode))
        n[lastHTTPcode] <- n[lastHTTPcode] + 1L
        cat(sprintf("%s\t%s\n", i, lastHTTPcode), file = logfile, append = TRUE)
        if (lastHTTPcode == "500") Sys.sleep(20) # avoid too many requests error
    }
    if (!quiet) cat("\n")
    cat(n["200"], " product(s) downloaded successfully (code 200)\n",
        n["202"], " product(s) successfully submitted to LTA for retrieval (code 202)\n",
        n["403"], " quota exceeded (code 403)\n",
        n["404"], " not found (code 404)\n",
        n["416"], " product(s) already downloaded (code 416)\n",
        n["500"], " internal server error (code 500)\n",
        n["503"], " service unavailable (code 503)\n",
        "Logfile: ", logfile, "\n", sep = "")
}

## https://cloud.google.com/storage/docs/public-datasets/sentinel-2
downloadGcloud <- function(DF, bands, destdir = ".", engine = "wget", quiet = FALSE)
{
    od <- setwd(destdir)
    on.exit(setwd(od))
    n <- nrow(DF)
    allbands <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07",
                  "B08", "B09",  "B10", "B11", "B12", "B8A")
    if (missing(bands)) {
        bands <- allbands
    } else {
        if (is.numeric(bands)) {
            if (any(bands > 12) || any(bands < 1))
                stop("'bands' should be between 1 and 12")
            bands <- sprintf("B%02d", as.integer(bands))
        }
        if (is.character(bands)) {
            bands <- toupper(bands)
            if (anyNA(match(bands, allbands))) {
                msg <- paste0("valid values for 'bands' are:\n",
                              paste(dQuote(allbands, '"'), collapse = " "))
                stop(msg)
            }
        }
    }
    baseurl <- "https://storage.googleapis.com/gcp-public-data-sentinel-2/tiles/"
    Nfiles <- n * length(bands)
    k <- 0L
    if (!quiet)
        cat(sprintf("Downloading %d %s from GoogleCloud...\n",
                    Nfiles, if (Nfiles > 1) "files" else "file"))
    for (i in 1:n) {
        fl <-  DF$filename[i]
        GI <-  DF$granuleidentifier[i]
        DSI <- DF$datastripidentifier[i]
        if (is.na(fl) || is.na(GI) || is.na(DSI)) {
            warning(paste("cannot get files for row", i))
            next
        }
        j <- attr(gregexpr("^.*_T[0-9]{2}", fl)[[1]], "match.length")
        strip <- substr(fl, j - 1L, j)
        square <- substr(fl, j + 1L, j + 1L)
        square2 <- substr(fl, j + 2L, j + 3L)
        prefix <- substr(fl, j - 2L, j + 3L)
        midix <- gsub("^.*_", "", gsub(paste0("_", prefix, "_.*$"), "", GI))
        ##suffix <- gsub("\\.SAFE$", "", gsub("^.*_", "", fl))
        suffix <- gsub("_.*$", "", gsub("^.*_S", "", DSI))
        date <- gsub("_.*$", "", gsub("S2[AB]_MSIL1C_", "", fl))
        strtmp <- paste0(baseurl, strip, "/", square, "/", square2, "/",
                         fl, "/GRANULE/L1C_", prefix, "_", midix, "_",
                         suffix, "/IMG_DATA/", prefix, "_", date, "_")
        for (b in bands) {
            URL <- paste0(strtmp, b, ".jp2")
            o <- system2(engine, URL, stdout = TRUE, stderr = TRUE)
            k <- k + 1L
            if (!quiet) cat("\r", k, "/", Nfiles)
        }
    }
    if (!quiet) cat("\n")
}

