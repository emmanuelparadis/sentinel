## zzz.R (2020-03-06)

##   Library Loading

## Copyright 2020 Emmanuel Paradis

## This file is part of the R-package `sentinel'.
## See the file ../COPYING for licensing issues.

.sentinelEnv <- new.env()

assign("searchCopernicusStats",
       data.frame(Files = numeric(), Products = numeric()),
       envir = .sentinelEnv)

