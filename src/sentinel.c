/* sentinel.c    2024-01-05 */

/* Copyright 2021-2024 Emmanuel Paradis */

/* This file is part of the R-package `sentinel'. */
/* See the file ../COPYING for licensing issues. */

#include <R.h>
#include <Rinternals.h>
//#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* declare functions here to register them below */

void C_specrend(double *spec_intens, double *R, double *G, double *B, int *approx, int *color_system);
int F77_NAME(wltocol)(double *WAVELEN, int *N, double *GAMMA, double *RES);
SEXP fast2waytable_Call(SEXP X, SEXP Y, SEXP NCAT, SEXP TT);
SEXP aggregate_sen2_10to20(SEXP X);
SEXP aggregate_sen2_10to60(SEXP X);
SEXP bilinear_interpolation_Call(SEXP raster, SEXP dims);
SEXP Call_tabulate_sentinel(SEXP x);

static R_CMethodDef C_entries[] = {
    {"C_specrend", (DL_FUNC) &C_specrend, 6},
    {NULL, NULL, 0}
};

static R_CallMethodDef Call_entries[] = {
    {"fast2waytable_Call", (DL_FUNC) &fast2waytable_Call, 4},
    {"aggregate_sen2_10to20", (DL_FUNC) &aggregate_sen2_10to20, 1},
    {"aggregate_sen2_10to60", (DL_FUNC) &aggregate_sen2_10to60, 1},
    {"bilinear_interpolation_Call", (DL_FUNC) &bilinear_interpolation_Call, 2},
    {"Call_tabulate_sentinel", (DL_FUNC) &Call_tabulate_sentinel, 1},
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
