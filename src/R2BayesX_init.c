#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP cpoint(SEXP, SEXP);
SEXP cpos(SEXP, SEXP);
SEXP getuit(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP unique_id(SEXP, SEXP);

static R_CallMethodDef callMethods[] = {
  {"cpoint", (DL_FUNC) &cpoint, 2},
  {"cpos", (DL_FUNC) &cpos, 2},
  {"getuit", (DL_FUNC) &getuit, 5},
  {"unique_id", (DL_FUNC) &unique_id, 2},
  {NULL, NULL, 0}
};

void R_init_sourcetools(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

