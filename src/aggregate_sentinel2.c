/* aggregate_sentinel2.c    2021-03-05 */

/* Copyright 2021 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

/* aggregate_10to60: aggregate 36 10-m-resolution cells into a single
     60-m-resolution cell with their mean
   aggregate_10to20: aggregate 4 10-m-resolution cells into a single
     20-m-resolution cell with their mean
   aggregate_20to60: aggregate 9 20-m-resolution cells into a single
     60-m-resolution cell with their mean
   aggregate_10to60_with_var: as above, also returns the variance for each cell
   aggregate_20to60_with_var: id. */

#include <R.h>

void aggregate_10to60(int *x, int *nr, int *nc, int *res)
{
    int i, j, k = 0, i0 = 0, i1 = 5, j0 = 0, j1 = 5, last, s;

    last = nr[0] * nc[0] / 36;

    while (k < last) {
	s = 0;
	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		s += x[j + i * nc[0]];
	    }
	}
	res[k] = s / 36;
	j0 += 6;
	j1 += 6;
	if (j0 >= nc[0]) {
	    j0 = 0;
	    j1 = 5;
	    i0 += 6;
	    i1 += 6;
	}
	k++;
    }
}

void aggregate_10to20(int *x, int *nr, int *nc, int *res)
{
    int i, j, k = 0, i0 = 0, i1 = 1, j0 = 0, j1 = 1, last, s;

    last = nr[0] * nc[0] / 4;

    while (k < last) {
	s = 0;
	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		s += x[j + i * nc[0]];
	    }
	}
	res[k] = s / 4;
	j0 += 2;
	j1 += 2;
	if (j0 >= nc[0]) {
	    j0 = 0;
	    j1 = 1;
	    i0 += 2;
	    i1 += 2;
	}
	k++;
    }
}

void aggregate_20to60(int *x, int *nr, int *nc, int *res)
{
    int i, j, k = 0, s, i0 = 0, i1 = 2, j0 = 0, j1 = 2, last;

    last = nr[0] * nc[0] / 9;

    while (k < last) {
	s = 0;
	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		s += x[j + i * nc[0]];
	    }
	}
	res[k] = s / 9;
	j0 += 3;
	j1 += 3;
	if (j0 >= nc[0]) {
	    j0 = 0;
	    j1 = 2;
	    i0 += 3;
	    i1 += 3;
	}
	k++;
    }
}

void aggregate_10to60_with_var(int *x, int *nr, int *nc, double *res, double *var)
{
    int i, j, k = 0, i0 = 0, i1 = 5, j0 = 0, j1 = 5, last;
    double s, v, d;

    last = nr[0] * nc[0] / 36;

    while (k < last) {
	s = 0;
	v = 0;

	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		s += x[j + i * nc[0]];
	    }
	}
	s /= 36;
	res[k] = s;

	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		d = x[j + i * nc[0]] - s;
		v += d * d;
	    }
	}
	var[k] = v / 35;

	j0 += 6;
	j1 += 6;
	if (j0 >= nc[0]) {
	    j0 = 0;
	    j1 = 5;
	    i0 += 6;
	    i1 += 6;
	}
	k++;
    }
}

void aggregate_20to60_with_var(int *x, int *nr, int *nc, double *res, double *var)
{
    int i, j, k = 0, i0 = 0, i1 = 2, j0 = 0, j1 = 2, last;
    double s, v, d;

    last = nr[0] * nc[0] / 9;

    while (k < last) {
	s = 0;
	v = 0;

	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		s += x[j + i * nc[0]];
	    }
	}
	s /= 9;
	res[k] = s;

	for (i = i0; i <= i1; i++) {
	    for (j = j0; j <= j1; j++) {
		d = x[j + i * nc[0]] - s;
		v += d * d;
	    }
	}
	var[k] = v / 8;

	j0 += 3;
	j1 += 3;
	if (j0 >= nc[0]) {
	    j0 = 0;
	    j1 = 2;
	    i0 += 3;
	    i1 += 3;
	}
	k++;
    }
}
