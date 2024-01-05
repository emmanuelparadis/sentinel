/* bilinear_interpolation.c    2024-01-05 */

/* Copyright 2021-2024 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <R.h>
#include <Rinternals.h>

/*
   input: x: a raster values with NC columns and NR rows
   output: z: a raster values wiht 2*NC columns and 2*NR rows

   See test_bilinear_interpolation.R for explanations on the pixel
   arrangements within the two grids.

   Explanations on the indices used in the code:

        a
   -----------
   |         |
d  |    l    |  b
   |         |
   -----------
        c

        |
        V

   -----------
   | k1 | k2 |
   -----------
   | k3 | k4 |
   -----------

so: k2 = k1 + 1    k3 = k1 + NCz    k4 = k3 + 1 = k2 + NCz
*/

#define SQRT10 3.16227766016838
#define w1 1 / (M_SQRT2 * 0.25)
#define w2 1 / (SQRT10 * 0.25)
#define w3 1 / (M_SQRT2 * 0.75)

void bilinear_interpolation(double *x, double *z, int NR, int NC)
{
    int i, j, a, b, c, d, k1, k2, k3, k4, NCz = 2 * NC, NRz = 2 * NR, stop;
    double A, B, C, D, sw;

    /* process the 4 corners */
    z[0] = x[0]; /* top-left */
    z[NCz - 1] = x[NC - 1]; /* top-right */
    z[(NRz - 1) * NCz] = x[(NR - 1) * NC]; /* bottom-left */
    z[NCz * NRz - 1] = x[NC * NR - 1]; /* bottom-right */

    /* process the 4 "borders" excluding the corners */
    sw = w1 + w2;

    /* the top and bottom rows */
    for (i = 0; i < NR; i += NR - 1) {
	if (i == 0) { /* top row */
	    /* start at the 2nd col of the 1st row of 'z' */
	    a = 0; b = 1; k1 = 1; k2 = 2;
	    stop = NC;
	} else { /* bottom row */
	    a = (NR - 1) * NC; b = a + 1;
	    /* start at the 2nd col of the last row of 'z' */
	    k1 = (NRz - 1) * NCz + 1; k2 = k1 + 1;
	    stop = NC * NR;
	}
	A = x[a];
	for (;;) {
	    B = x[b];
	    z[k1] = (w1 * A + w2 * B) / sw;
	    z[k2] = (w2 * A + w1 * B) / sw;
	    b++;
	    if (b >= stop) break;
	    a++; k1 += 2; k2 += 2; A = B;
	}
    }

    /* the left and right columns */
    for (j = 0; j < NC; j += NC - 1) {
	if (j == 0) { /* left column */
	    a = 0; b = NC; k1 = NCz;
	    stop = (NR - 1) * NC + 1;
	} else { /* right column */
	    a = NC - 1; b = a + NC;
	    k1 = 2 * NCz - 1;
	    stop = NR * NC;
	}
	k2 = k1 + NCz;
	A = x[a];
	for (;;) {
	    B = x[b];
	    z[k1] = (w1 * A + w2 * B) / sw;
	    z[k2] = (w2 * A + w1 * B) / sw;
	    b += NC;
	    if (b >= stop) break;
	    a += NC; k1 += 2 * NCz; k2 += 2 * NCz; A = B;
	}
    }

    /* process all other pixels */
    sw = w1 + 2 * w2 + w3;
    a = 0; b = 1; c = NC; d = c + 1;
    k1 = NCz + 1; k2 = k1 + 1; k3 = k1 + NCz; k4 = k3 + 1;
    A = x[a]; C = x[c];
    stop = NR * NC;

    for (;;) {
	B = x[b]; D = x[d];
	z[k1] = (w1 * A + w2 * B + w2 * C + w3 * D) / sw;
	z[k2] = (w2 * A + w1 * B + w3 * C + w2 * D) / sw;
	z[k3] = (w2 * A + w3 * B + w1 * C + w2 * D) / sw;
	z[k4] = (w3 * A + w2 * B + w2 * C + w1 * D) / sw;
	d++;
	if (d >= stop) break;
	k1 += 2; k2 += 2; k3 += 2; k4 += 2;
	a++; b++; c++; A = B; C = D;
	if (!(k2 % NCz)) {
	    k1 = k4 + 1; k2 = k1 + 1; k3 = k1 + NCz; k4 = k3 + 1;
	    a++; b++; c++; d++;
	    A = x[a]; C = x[c];
	}
    }
}

SEXP bilinear_interpolation_Call(SEXP raster, SEXP dims)
//				 SEXP RESOLUTION)
{
    int NR, NC;
    SEXP res;
    double *x, *z;//, reso;

    PROTECT(raster = coerceVector(raster, REALSXP));
    PROTECT(dims = coerceVector(dims, INTSXP));
    //    PROTECT(RESOLUTION = coerceVector(RESOLUTION, REALSXP));
    NR = INTEGER(dims)[0];
    NC = INTEGER(dims)[1];
    x = REAL(raster);
    PROTECT(res = allocVector(REALSXP, NR * NC * 4));
    z = REAL(res);
    //    reso = REAL(RESOLUTION)[0];

    bilinear_interpolation(x, z, NR, NC);

    UNPROTECT(3);
    return res;
}
