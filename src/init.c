#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

extern SEXP r_env_mirror(SEXP, SEXP);

static const R_CallMethodDef call_entries[] = {
  {"rlang_env_mirror",      (DL_FUNC) &r_env_mirror, 2},
  {NULL, NULL, 0}
};

void R_init_rlanglabs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
