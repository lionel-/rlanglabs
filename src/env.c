#include "rlanglab.h"

sexp*
new_bare_environment() {
  sexp* env = Rf_cons(R_NilValue, R_EmptyEnv);
  SET_TYPEOF(env, ENVSXP);
  return env;
}

sexp*
r_env_mirror(sexp* env, sexp* parent) {
  sexp* out = KEEP(new_bare_environment());

  SET_FRAME(out, parent);
  SET_HASHTAB(out, HASHTAB(env));

  FREE(1);
  return out;
}
