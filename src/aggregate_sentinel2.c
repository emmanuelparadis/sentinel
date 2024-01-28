/* aggregate_sentinel2.c    2024-01-05 */

/* Copyright 2024 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

/* aggregate_10to60: aggregate 36 10-m-resolution cells into a single
     60-m-resolution cell with their mean
   aggregate_10to20: aggregate 4 10-m-resolution cells into a single
     20-m-resolution cell with their mean
   aggregate_20to60: aggregate 9 20-m-resolution cells into a single
     60-m-resolution cell with their mean */

/* Three functions to aggregate Sentinel-2 rasters
   10m->20m, 10m->60m, 20m->60m */

#include <R.h>
#include <Rinternals.h>

void aggregate_10to60(double *x, double *res)
{
    int i, j, k = 0, i0 = 0, i1 = 5, j0 = 0, j1 = 5, o;
    double s;

    for (;;) {
	s = 0;
	for (i = i0; i <= i1; i++) {
	    o = i * 10980 + j0;
	    for (j = j0; j <= j1; j++, o++) s += x[o];
	}
	res[k] = s / 36;
	k++;
	if (k == 3348900) break;
	j0 += 6;
	j1 += 6;
	if (j0 >= 10980) {
	    j0 = 0;
	    j1 = 5;
	    i0 += 6;
	    i1 += 6;
	}
    }
}

void aggregate_10to20(double *x, double *res)
{
    int i, a = 0, b = 1, c = 10980, d = 10981;

    for (i = 0; i < 30140099; i++) {
	res[i] = (x[a] + x[b] + x[c] + x[d]) / 4;
	if (!(b % 10979)) {
	    a += 10982;
	    b = a + 1;
	    c = a + 10980;
	    d = c + 1;
	} else {
	    a += 2;
	    b += 2;
	    c += 2;
	    d += 2;
	}
    }
}

void aggregate_20to60(double *x, double *res)
{
    int i, j, k = 0, i0 = 0, i1 = 2, j0 = 0, j1 = 2, o;
    double s;

    for (;;) {
	s = 0;
	for (i = i0; i <= i1; i++) {
	    o = i * 5490 + j0;
	    for (j = j0; j <= j1; j++, o++) s += x[o];
	}
	res[k] = s / 9;
	k++;
	if (k == 3348900) break;
	j0 += 3;
	j1 += 3;
	if (j0 >= 5490) {
	    j0 = 0;
	    j1 = 2;
	    i0 += 3;
	    i1 += 3;
	}
    }
}

/* X: raster of dim 10980x10980
   res: raster of dim 5490x5490 */
SEXP aggregate_sen2_10to20(SEXP X)
{
    SEXP res;
    PROTECT(X = coerceVector(X, REALSXP));
    PROTECT(res = allocVector(REALSXP, 30140100));
    aggregate_10to20(REAL(X), REAL(res));
    UNPROTECT(2);
    return res;
}

/* X: raster of dim 10980x10980
   res: raster of dim 1830x1830 */
SEXP aggregate_sen2_10to60(SEXP X)
{
    SEXP res;
    PROTECT(X = coerceVector(X, REALSXP));
    PROTECT(res = allocVector(REALSXP, 3348900));
    aggregate_10to60(REAL(X), REAL(res));
    UNPROTECT(2);
    return res;
}


/* X: raster of dim 5490x5490
   res: raster of dim 1830x1830 */
SEXP aggregate_sen2_20to60(SEXP X)
{
    SEXP res;
    PROTECT(X = coerceVector(X, REALSXP));
    PROTECT(res = allocVector(REALSXP, 3348900));
    aggregate_20to60(REAL(X), REAL(res));
    UNPROTECT(2);
    return res;
}
