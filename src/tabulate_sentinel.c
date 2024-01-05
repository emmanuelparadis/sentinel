/* tabulate_sentinel.c    2024-01-05 */

/* Copyright 2021-2024 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <R.h>
#include <Rinternals.h>

void tabulate_sentinel(int *x, int n, int *res)
{
    int i;

    memset(res, 0, 10001 * sizeof(int));

    for (i = 0; i < n; i++) {
	/* negative values counted as 0?
	if (xp[i] < 0) {
	    ++y[0];
	    continue;
        }
	*/
	if (x[i] > 10000) {
	    ++res[10000];
	    continue;
	}
	++res[x[i]];
    }
}

/*
  count the number of (integer) values 0, 1, ..., 1e4

  values > 1e4 are counted as equal to 1e4

  return a vector of integers of length 10001
*/

SEXP Call_tabulate_sentinel(SEXP x) {
    SEXP res;
    PROTECT(x = coerceVector(x, INTSXP));
    PROTECT(res = allocVector(INTSXP, 10001));
    tabulate_sentinel(INTEGER(x), LENGTH(x), INTEGER(res));
    UNPROTECT(2);
    return res;
}

