/* sentinel.c    2021-03-05 */

/* Copyright 2021 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* declare functions here to register them below */

void aggregate_10to60(int *x, int *nr, int *nc, int *res);
void aggregate_10to20(int *x, int *nr, int *nc, int *res);
void aggregate_20to60(int *x, int *nr, int *nc, int *res);
void aggregate_10to60_with_var(int *x, int *nr, int *nc, double *res, double *var);
void aggregate_20to60_with_var(int *x, int *nr, int *nc, double *res, double *var);
SEXP C_kmeans_dnorm(SEXP X, SEXP cluster0, SEXP Nclusters, SEXP threshold, SEXP iterlim);

static R_CMethodDef C_entries[] = {
    {"aggregate_10to60", (DL_FUNC) &aggregate_10to60, 4},
    {"aggregate_10to20", (DL_FUNC) &aggregate_10to20, 4},
    {"aggregate_20to60", (DL_FUNC) &aggregate_20to60, 4},
    {"aggregate_10to60_with_var", (DL_FUNC) &aggregate_10to60_with_var, 5},
    {"aggregate_20to60_with_var", (DL_FUNC) &aggregate_20to60_with_var, 5},
    {NULL, NULL, 0}
};

static R_CallMethodDef Call_entries[] = {
    {"C_kmeans_dnorm", (DL_FUNC) &C_kmeans_dnorm, 5},
    {NULL, NULL, 0}
};

void R_init_sentinel(DllInfo *info)
{
    R_registerRoutines(info, C_entries, Call_entries, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
