/* kmeans_dnorm.c    2021-03-30 */

/* Copyright 2021 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP C_kmeans_dnorm(SEXP X, SEXP cluster0, SEXP Nclusters, SEXP threshold, SEXP iterlim)
{
    int n, p, i, j, k, l, *cls, *post, K, Nreclass, loop, *N, whichmax, iter_lim;
    double *x, *MEANS, *SD, d, maxdensity, newdensity, thres;
    SEXP res;

    double *TERM1, *TwoVAR;

    PROTECT(X = coerceVector(X, REALSXP));
    PROTECT(cluster0 = coerceVector(cluster0, INTSXP));
    PROTECT(Nclusters = coerceVector(Nclusters, INTSXP));
    PROTECT(threshold = coerceVector(threshold, REALSXP));
    PROTECT(iterlim = coerceVector(iterlim, INTSXP));
    x = REAL(X);
    cls = INTEGER(cluster0);
    K = INTEGER(Nclusters)[0];
    thres = REAL(threshold)[0];
    iter_lim = INTEGER(iterlim)[0];

    n = nrows(X);
    p = ncols(X);

    PROTECT(res = allocVector(INTSXP, n));
    post = INTEGER(res);

    MEANS = (double*)R_alloc(p * K, sizeof(double));
    SD = (double*)R_alloc(p * K, sizeof(double));
    TwoVAR = (double*)R_alloc(p * K, sizeof(double));
    TERM1 = (double*)R_alloc(K, sizeof(double));
    N = (int*)R_alloc(K, sizeof(int));

    loop = 0;

    for (;;) {
	loop++;
	if (loop > iter_lim) {
	    Rprintf("Reached iteration number limit.\n");
	    break;
	}
	Rprintf("iteration %d ", loop);
	/* compute the means and SDs */
	memset(MEANS, 0, p * K * sizeof(double));
	memset(SD, 0, p * K * sizeof(double));
	memset(N, 0, K * sizeof(int));
	for (i = 0; i < n; i++) {
	    k = cls[i] - 1;
	    (N[k])++;
	    for (j = 0; j < p; j++)
		MEANS[j + k * p] += x[i + j * n];
	}
	for (j = 0; j < p; j++) {
	    for (k = 0; k < K; k++)
		MEANS[j + k * p] /= N[k];
	}
	for (i = 0; i < n; i++) {
	    k = cls[i] - 1;
	    for (j = 0; j < p; j++) {
		l = j + k * p;
		d = x[i + j * n] - MEANS[l];
		SD[l] += d * d;
	    }
	}
	for (j = 0; j < p; j++) {
	    for (k = 0; k < K; k++) {
		l = j + k * p;
		d = SD[l] / (N[k] - 1);
		TwoVAR[l] = 2 * d;
		SD[l] = sqrt(d);
	    }
	}

	for (k = 0; k < K; k++) {
	    TERM1[k] = -M_LN_SQRT_2PI;
	    for (j = 0; j < p; j++) {
		l = j + k * p;
		TERM1[k] -= log(SD[l]);
	    }
	}

	for (i = 0; i < n; i++) {
	    maxdensity = 0; /* k = 0 to start*/
	    for (j = 0; j < p; j++) {
		d = x[i + j * n] - MEANS[j]; // x_i - mu
		maxdensity -= (d * d) / TwoVAR[j]; // (x_i - mu)^2 / (2 sigma^2)
	    }
	    maxdensity += TERM1[0];
	    whichmax = 0; /* k */
	    for (k = 1; k < K; k++) {
		newdensity = 0;
		for (j = 0; j < p; j++) {
		    l = j + k * p;
		    d = x[i + j * n] - MEANS[l];
		    newdensity -= (d * d) / TwoVAR[l];
		}
		newdensity += TERM1[k];
		if (newdensity > maxdensity) {
		    maxdensity = newdensity;
		    whichmax = k;
		}
	    }
	    post[i] = whichmax + 1;
	}
	Nreclass = 0;
	for (i = 0; i < n; i++) {
	    if (cls[i] != post[i]) {
		Nreclass++;
		cls[i] = post[i];
	    }
	}
	if (!Nreclass) {
	    Rprintf(" -> 0 reclassified\n");
	    break;
	}
	Rprintf("-> %d reclassified\n", Nreclass);
	if ((double) Nreclass / n < thres) {
	    Rprintf("Less than %f%% reclassified: stopping here.\n", 100 * thres);
	    break;
	}
    }

    UNPROTECT(6);
    return res;
}
