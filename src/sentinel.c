/* sentinel.c    2022-05-25 */

/* Copyright 2021-2022 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <R.h>
#include <Rinternals.h>
//#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* declare functions here to register them below */

void aggregate_10to60(int *x, int *nr, int *nc, int *res);
void aggregate_10to20(int *x, int *nr, int *nc, int *res);
void aggregate_20to60(int *x, int *nr, int *nc, int *res);
void aggregate_10to60_with_var(int *x, int *nr, int *nc, double *res, double *var);
void aggregate_20to60_with_var(int *x, int *nr, int *nc, double *res, double *var);
void C_specrend(double *spec_intens, double *R, double *G, double *B, int *approx, int *color_system);
SEXP C_kmeans_dnorm(SEXP X, SEXP cluster0, SEXP PARA);
int F77_NAME(wltocol)(double *WAVELEN, int *N, double *GAMMA, double *RES);

static R_CMethodDef C_entries[] = {
    {"aggregate_10to60", (DL_FUNC) &aggregate_10to60, 4},
    {"aggregate_10to20", (DL_FUNC) &aggregate_10to20, 4},
    {"aggregate_20to60", (DL_FUNC) &aggregate_20to60, 4},
    {"aggregate_10to60_with_var", (DL_FUNC) &aggregate_10to60_with_var, 5},
    {"aggregate_20to60_with_var", (DL_FUNC) &aggregate_20to60_with_var, 5},
    {"C_specrend", (DL_FUNC) &C_specrend, 6},
    {NULL, NULL, 0}
};

static R_CallMethodDef Call_entries[] = {
    {"C_kmeans_dnorm", (DL_FUNC) &C_kmeans_dnorm, 3},
    {NULL, NULL, 0}
};

static R_FortranMethodDef Fortran_entries[] = {
    {"wltocol", (DL_FUNC) &F77_SUB(wltocol), 4},
    {NULL, NULL, 0}
};

void R_init_sentinel(DllInfo *info)
{
    R_registerRoutines(info, C_entries, Call_entries, Fortran_entries, NULL);
    R_useDynamicSymbols(info, FALSE);
}
