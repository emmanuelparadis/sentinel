/* kmeans_dnorm.c    2022-07-01 */

/* Copyright 2021-2022 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* SEXP Nclusters, SEXP threshold, SEXP iterlim, SEXP QUIET */
SEXP C_kmeans_dnorm(SEXP X, SEXP cluster0, SEXP PARA)
{
    int n, p, i, j, k, kp, l, *cls, *post, K, Nreclass, loop, *N, whichmax, thres, iter_lim, verbose;
    double *x, maxdensity, newdensity;
    /* long double *MEANS, *SD, d; */
    double *MEANS, *SD, d;
    SEXP res;

    double *TERM1, *TwoVAR;

    /* unsigned char *no_update; */

    /*
    Rprintf("sizeof(long double) = %d\n", sizeof(long double));
    Rprintf("sizeof(double) = %d\n", sizeof(double));
    Rprintf("sizeof(_Float64x) = %d\n", sizeof(_Float64x));
    Rprintf("sizeof(__float80) = %d\n", sizeof(__float80));
    Rprintf("sizeof(__float128) = %d\n", sizeof(__float128));
    Rprintf("sizeof(int) = %d\n", sizeof(int));
    Rprintf("sizeof(long) = %d\n", sizeof(long));
    Rprintf("sizeof(short) = %d\n", sizeof(short));
    Rprintf("sizeof(unsigned char) = %d\n", sizeof(unsigned char));
    error("");
    */

    PROTECT(X = coerceVector(X, REALSXP));
    PROTECT(cluster0 = coerceVector(cluster0, INTSXP));
    PROTECT(PARA = coerceVector(PARA, INTSXP));
    x = REAL(X);
    cls = INTEGER(cluster0);
    K = INTEGER(PARA)[0];
    thres = INTEGER(PARA)[1];
    iter_lim = INTEGER(PARA)[2];
    verbose = !INTEGER(PARA)[3];

    n = nrows(X);
    p = ncols(X);

    PROTECT(res = allocVector(INTSXP, n));
    post = INTEGER(res);

    /* MEANS = (long double*)R_alloc(p * K, sizeof(long double));
       SD = (long double*)R_alloc(p * K, sizeof(long double)); */
    MEANS = (double*)R_alloc(p * K, sizeof(double));
    SD = (double*)R_alloc(p * K, sizeof(double));
    TwoVAR = (double*)R_alloc(p * K, sizeof(double));
    TERM1 = (double*)R_alloc(K, sizeof(double));
    N = (int*)R_alloc(K, sizeof(int));

    /* no_update = (unsigned char*)R_alloc(K, sizeof(unsigned char)); */
    /* initialise no_update with 0's, so the means and SDs will be
       calculated at the 1st loop */
    /* memset(no_update, 0, K * sizeof(unsigned char)); */

    loop = 0;

    for (;;) {
	loop++;
	if (loop > iter_lim) {
	    if (verbose) Rprintf("Reached iteration number limit.\n");
	    break;
	 }
	if (verbose) Rprintf("iteration %d ", loop);
	/* compute the means and SDs */

	/* memset(MEANS, 0, p * K * sizeof(double)); */
	/* memset(SD, 0, p * K * sizeof(double)); */
	/* memset(N, 0, K * sizeof(int)); */

	/* initialiase only for the groups that need to */
	for (k = 0; k < K; k++) {
	    /* if (no_update[k]) continue; */
	    N[k] = 0;
	    kp = k * p;
	    for (j = 0; j < p; j++) {
		l = j + kp;
		MEANS[l] = 0;
		SD[l] = 0;
	    }
	}

	/* 1st, calculate the numbers and the sums */
	for (i = 0; i < n; i++) {
	    k = cls[i] - 1;
	    /* if (no_update[k]) continue; */
	    (N[k])++;
	    kp = k * p;
	    for (j = 0; j < p; j++)
		MEANS[j + kp] += x[i + j * n];
	}
	/* then the means by their ratios */
	for (k = 0; k < K; k++) {
	    /* if (no_update[k]) continue; */
	    kp = k * p;
	    for (j = 0; j < p; j++) MEANS[j + kp] /= N[k];
	}
	/* 1st, calculate the sum of squared deviations to the mean */
	for (i = 0; i < n; i++) {
	    k = cls[i] - 1;
	    /* if (no_update[k]) continue; */
	    kp = k * p;
	    for (j = 0; j < p; j++) {
		l = j + kp;
		d = x[i + j * n] - MEANS[l];
		SD[l] += d * d;
	    }
	}
	/* then, the SD themselves, and store twice the variances for later */
	for (k = 0; k < K; k++) {
	    /* if (no_update[k]) continue; */
	    kp = k * p;
	    for (j = 0; j < p; j++) {
		l = j + kp;
		d = SD[l] / (N[k] - 1);
		TwoVAR[l] = 2 * d;
		SD[l] = sqrt(d);
	    }
	}

	/* compute the 1st term of the log-density which is the same for all */
	for (k = 0; k < K; k++) {
	    /* if (no_update[k]) continue; */
	    kp = k * p;
	    TERM1[k] = -M_LN_SQRT_2PI;
	    for (j = 0; j < p; j++) {
		l = j + kp;
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
		kp = k * p;
		newdensity = 0;
		for (j = 0; j < p; j++) {
		    l = j + kp;
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
	/* initialise no_update with 1's, so a priori no means and SDs
	   will need to be recalculated */
	/* for (k = 0; k < K; k++) no_update[k] = 1; */
	for (i = 0; i < n; i++) {
	    k = post[i];
	    if (cls[i] != k) {
		/* no_update[cls[i]] = no_update[k] = 0; */
		Nreclass++;
		cls[i] = k;
	    }
	}
	if (verbose) Rprintf("-> %d reclassified\n", Nreclass);
	if (!Nreclass) break;
	if (Nreclass < thres) {
	    if (verbose) Rprintf("Less than %f%% reclassified: stopping here.\n", (double) 100 * thres / n);
	    break;
	}
    }

    UNPROTECT(4);
    return res;
}
