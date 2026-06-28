#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP C_comfort_pmv_vec(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_comfort_set_vec(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                              SEXP, SEXP, SEXP);
extern SEXP C_comfort_pmv_curve_roots(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                                      SEXP, SEXP, SEXP, SEXP);
extern SEXP C_comfort_pmv_saturation_roots(SEXP, SEXP, SEXP, SEXP, SEXP,
                                           SEXP, SEXP, SEXP, SEXP, SEXP,
                                           SEXP);
extern SEXP C_dew_point_from_hum_ratio(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_comfort_pmv_vec", (DL_FUNC) &C_comfort_pmv_vec, 7},
    {"C_comfort_set_vec", (DL_FUNC) &C_comfort_set_vec, 10},
    {"C_comfort_pmv_curve_roots", (DL_FUNC) &C_comfort_pmv_curve_roots, 10},
    {"C_comfort_pmv_saturation_roots",
        (DL_FUNC) &C_comfort_pmv_saturation_roots, 11},
    {"C_dew_point_from_hum_ratio", (DL_FUNC) &C_dew_point_from_hum_ratio, 4},
    {NULL, NULL, 0}
};

void R_init_ggpsychro(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
