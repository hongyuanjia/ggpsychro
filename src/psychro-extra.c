#include <math.h>

#include "ggpsychro.h"

#define GGPSY_MW_RATIO 0.621945
#define GGPSY_PSI_TO_PA 6894.7572931783

/*
 * Small psychrometric helpers that are hot in chart transforms and comfort
 * root tracing.  They mirror the psychrolib equations but avoid repeated R
 * callbacks and uniroot() calls in tight loops.
 */

double ggpsy_sat_vap_pres_si(double tdb)
{
    /*
     * Saturation vapor pressure in Pa.  The branch at the triple point follows
     * psychrolib/ASHRAE equations for ice below 0.01 deg C and water above it.
     */
    double tk;
    double ln_pws;

    if (!R_FINITE(tdb) || tdb < -100.0 || tdb > 200.0) {
        return NA_REAL;
    }

    tk = tdb + 273.15;
    if (tdb <= 0.01) {
        ln_pws = -5674.5359 / tk + 6.3925247 - 0.009677843 * tk +
            6.2215701e-07 * tk * tk + 2.0747825e-09 * tk * tk * tk -
            9.484024e-13 * tk * tk * tk * tk + 4.1635019 * log(tk);
    } else {
        ln_pws = -5800.2206 / tk + 1.3914993 - 0.048640239 * tk +
            4.1764768e-05 * tk * tk - 1.4452093e-08 * tk * tk * tk +
            6.5459673 * log(tk);
    }

    return exp(ln_pws);
}

double ggpsy_hum_ratio_from_vap_pres_si(double vap_pres, double pressure,
                                        double min_hum_ratio)
{
    double hum_ratio;

    if (!R_FINITE(vap_pres) || !R_FINITE(pressure) || pressure <= vap_pres) {
        return NA_REAL;
    }

    hum_ratio = GGPSY_MW_RATIO * vap_pres / (pressure - vap_pres);
    if (R_FINITE(min_hum_ratio) && hum_ratio < min_hum_ratio) {
        hum_ratio = min_hum_ratio;
    }
    return hum_ratio;
}

double ggpsy_vap_pres_from_hum_ratio_si(double hum_ratio, double pressure,
                                        double min_hum_ratio)
{
    double bounded;

    if (!R_FINITE(hum_ratio) || !R_FINITE(pressure)) {
        return NA_REAL;
    }

    bounded = hum_ratio;
    if (R_FINITE(min_hum_ratio) && bounded < min_hum_ratio) {
        bounded = min_hum_ratio;
    }

    return pressure * bounded / (GGPSY_MW_RATIO + bounded);
}

double ggpsy_dew_point_from_vap_pres_si(double vap_pres)
{
    /*
     * Invert saturation vapor pressure with bisection.  The range matches the
     * valid range used by the R fallback; the early returns avoid needless work
     * at extreme vapor pressures and keep behavior stable near 0 deg C.
     */
    double lo = -100.0;
    double hi = 200.0;
    double mid = 100.0;
    double fmid;
    int i;

    if (!R_FINITE(vap_pres)) {
        return NA_REAL;
    }
    if (vap_pres >= 1555000.0) {
        return 200.0;
    }
    if (vap_pres <= 0.0017) {
        return -100.0;
    }
    if (vap_pres > 611.0 && vap_pres < 611.25) {
        return 0.0;
    }

    for (i = 0; i < 80 && (hi - lo) > 1e-4; ++i) {
        mid = (lo + hi) / 2.0;
        fmid = ggpsy_sat_vap_pres_si(mid) - vap_pres;
        if (!R_FINITE(fmid)) {
            return NA_REAL;
        }
        if (fmid < 0.0) {
            lo = mid;
        } else {
            hi = mid;
        }
    }

    return (lo + hi) / 2.0;
}

double ggpsy_dew_point_from_hum_ratio_si(double hum_ratio, double pressure,
                                         double min_hum_ratio)
{
    double vap_pres = ggpsy_vap_pres_from_hum_ratio_si(
        hum_ratio, pressure, min_hum_ratio
    );
    return ggpsy_dew_point_from_vap_pres_si(vap_pres);
}

double ggpsy_relhum_from_hum_ratio_si(double tdb, double hum_ratio,
                                      double pressure, double min_hum_ratio)
{
    /* Return RH as a fraction, not percent.  PMV conversion happens upstream. */
    double vap_pres = ggpsy_vap_pres_from_hum_ratio_si(
        hum_ratio, pressure, min_hum_ratio
    );
    double sat_vap_pres = ggpsy_sat_vap_pres_si(tdb);

    if (!R_FINITE(vap_pres) || !R_FINITE(sat_vap_pres) || sat_vap_pres <= 0.0) {
        return NA_REAL;
    }

    return vap_pres / sat_vap_pres;
}

double ggpsy_sat_hum_ratio_si(double tdb, double pressure,
                              double min_hum_ratio)
{
    return ggpsy_hum_ratio_from_vap_pres_si(
        ggpsy_sat_vap_pres_si(tdb), pressure, min_hum_ratio
    );
}

/*
 * R-facing dew-point vector wrapper.  Unlike the low-level helpers above, this
 * one accepts SI or IP pressure/temperature conventions through `units`.
 */
SEXP C_dew_point_from_hum_ratio(SEXP hum_ratio_sxp, SEXP pressure_sxp,
                                SEXP units_sxp, SEXP min_hum_ratio_sxp)
{
    R_xlen_t n_hum = XLENGTH(hum_ratio_sxp);
    R_xlen_t n_pressure = XLENGTH(pressure_sxp);
    int units = asInteger(units_sxp);
    double min_hum_ratio = asReal(min_hum_ratio_sxp);
    SEXP out = PROTECT(allocVector(REALSXP, n_hum));
    double *hum_ratio = REAL(hum_ratio_sxp);
    double *pressure = REAL(pressure_sxp);
    double *ans = REAL(out);
    R_xlen_t i;

    if (n_pressure != 1 && n_pressure != n_hum) {
        UNPROTECT(1);
        error("`Pressure` must have length 1 or match `HumRatio`.");
    }

    for (i = 0; i < n_hum; ++i) {
        double p = pressure[n_pressure == 1 ? 0 : i];
        double dew;
        if (units == 2) {
            p *= GGPSY_PSI_TO_PA;
        }
        dew = ggpsy_dew_point_from_hum_ratio_si(
            hum_ratio[i], p, min_hum_ratio
        );
        if (units == 2 && R_FINITE(dew)) {
            dew = dew * 9.0 / 5.0 + 32.0;
        }
        ans[i] = dew;
    }

    UNPROTECT(1);
    return out;
}
