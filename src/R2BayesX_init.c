#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define USE_FC_LEN_T

SEXP tr(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP getuit(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP cpos(SEXP, SEXP);
SEXP change(SEXP);
SEXP myNArem(SEXP);
SEXP cdist(SEXP, SEXP, SEXP);
SEXP cpoint(SEXP, SEXP);
SEXP unique_id(SEXP, SEXP);

static R_CallMethodDef callMethods[] = {
  {"tr", (DL_FUNC) &tr, 8},
  {"getuit", (DL_FUNC) &getuit, 5},
  {"cpos", (DL_FUNC) &cpos, 2},
  {"change", (DL_FUNC) &change, 1},
  {"myNArem", (DL_FUNC) &myNArem, 1},
  {"cdist", (DL_FUNC) &cdist, 3},
  {"cpoint", (DL_FUNC) &cpoint, 2},
  {"unique_id", (DL_FUNC) &unique_id, 2},
  {NULL, NULL, 0}
};

void R_init_sourcetools(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

