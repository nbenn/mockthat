
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP duplicate_(SEXP);
extern SEXP reassign_function(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"duplicate_",         (DL_FUNC) &duplicate_,         1},
    {"reassign_function",  (DL_FUNC) &reassign_function,  4},
    {NULL, NULL, 0}
};

void R_init_mockthat(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
