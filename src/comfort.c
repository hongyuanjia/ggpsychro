#include <float.h>
#include <math.h>

#include "ggpsychro.h"

/*
 * Native thermal-comfort kernels used by R/comfort.R.
 *
 * Public R functions still own argument matching, recycling, SI/IP conversion,
 * input limits, rounding, and data-frame assembly.  The .Call routines here are
 * deliberately narrow: they receive already-recycled numeric vectors in SI
 * comfort-model units and return raw numeric results for the R layer to wrap.
 */

/* Fixed PMV parameters reused while tracing one psychrometric chart curve. */
typedef struct {
    double tr;
    double vr;
    double met;
    double clo;
    double wme;
    double pressure;
    double min_hum_ratio;
} PmvParams;

static double scalar_or_na(SEXP x)
{
    if (XLENGTH(x) < 1) {
        return NA_REAL;
    }
    return REAL(x)[0];
}

static int finite_all7(double a, double b, double c, double d,
                       double e, double f, double g)
{
    return R_FINITE(a) && R_FINITE(b) && R_FINITE(c) && R_FINITE(d) &&
        R_FINITE(e) && R_FINITE(f) && R_FINITE(g);
}

static double comfort_p_sat_torr(double tdb)
{
    /*
     * SET uses the Pierce two-node equations in their traditional Torr-based
     * vapor-pressure form.  Psychrometric helpers in psychro-extra.c use SI Pa,
     * so keep this helper private to SET-related calculations.
     */
    return exp(18.6686 - 4030.183 / (tdb + 235.0));
}

/*
 * Fanger / ISO 7730 PMV for one state point.
 *
 * Inputs are SI thermal-comfort units: dry-bulb and radiant temperatures in
 * deg C, air speed in m/s, relative humidity in percent, activity in met,
 * clothing in clo, and external work in met.  The result is intentionally
 * unrounded so root tracing can use it as a smooth objective function.
 */
static double comfort_pmv_scalar(double tdb, double tr, double vr, double rh,
                                 double met, double clo, double wme)
{
    double pa;
    double icl;
    double m;
    double w;
    double mw;
    double f_cl;
    double hcf;
    double taa;
    double tra;
    double t_cla;
    double p1;
    double p2;
    double p3;
    double p4;
    double p5;
    double xn;
    double xf;
    double hcn = NA_REAL;
    double hc = NA_REAL;
    double tcl;
    double hl1;
    double hl2;
    double hl3;
    double hl4;
    double hl5;
    double hl6;
    double ts;
    int i = 0;

    if (!finite_all7(tdb, tr, vr, rh, met, clo, wme)) {
        return NA_REAL;
    }

    pa = rh * 10.0 * exp(16.6536 - 4030.183 / (tdb + 235.0));
    icl = 0.155 * clo;
    m = met * 58.15;
    w = wme * 58.15;
    mw = m - w;
    f_cl = (icl <= 0.078) ? 1.0 + 1.29 * icl : 1.05 + 0.645 * icl;
    hcf = 12.1 * sqrt(vr);
    taa = tdb + 273.0;
    tra = tr + 273.0;
    t_cla = taa + (35.5 - tdb) / (3.5 * icl + 0.1);

    p1 = icl * f_cl;
    p2 = p1 * 3.96;
    p3 = p1 * 100.0;
    p4 = p1 * taa;
    p5 = (308.7 - 0.028 * mw) + p2 * pow(tra / 100.0, 4.0);
    xn = t_cla / 100.0;
    xf = t_cla / 50.0;

    /* Solve clothing surface temperature because convection depends on it. */
    while (fabs(xn - xf) > 0.00015 && i < 150) {
        xf = (xf + xn) / 2.0;
        hcn = 2.38 * pow(fabs(100.0 * xf - taa), 0.25);
        hc = (hcf > hcn) ? hcf : hcn;
        xn = (p5 + p4 * hc - p2 * pow(xf, 4.0)) / (100.0 + p3 * hc);
        ++i;
    }

    tcl = 100.0 * xn - 273.0;
    /*
     * Six ISO heat-loss terms: skin diffusion, sweating, latent respiration,
     * sensible respiration, radiation, and convection.
     */
    hl1 = 3.05e-3 * (5733.0 - 6.99 * mw - pa);
    hl2 = (mw > 58.15) ? 0.42 * (mw - 58.15) : 0.0;
    hl3 = 1.7e-5 * m * (5867.0 - pa);
    hl4 = 0.0014 * m * (34.0 - tdb);
    hl5 = 3.96 * f_cl * (pow(xn, 4.0) - pow(tra / 100.0, 4.0));
    hl6 = f_cl * hc * (tcl - tdb);
    ts = 0.303 * exp(-0.036 * m) + 0.028;

    return ts * (mw - hl1 - hl2 - hl3 - hl4 - hl5 - hl6);
}

/*
 * SET for one state point using the Pierce two-node thermoregulation model.
 *
 * The function has two phases:
 *   1. A minute-step transient simulation that settles skin/core state.
 *   2. A Newton-style solve for the standard environment temperature that
 *      gives the same skin heat loss.
 *
 * The scalar R implementation remains in R/comfort.R as an oracle/fallback for
 * tests; this native version is only an acceleration backend.
 */
static double comfort_set_scalar(double tdb, double tr, double v, double rh,
                                 double met, double clo, double wme,
                                 double body_surface_area, double p_atm,
                                 int position_sitting)
{
    double air_speed;
    const double k_clo = 0.25;
    const double body_weight = 70.0;
    const double met_factor = 58.2;
    const double sbc = 5.6697e-8;
    const double c_sw = 170.0;
    const double c_dil = 120.0;
    const double c_str = 0.5;
    const double temp_skin_neutral = 33.7;
    const double temp_core_neutral = 36.8;
    const double temp_body_neutral = 36.49;
    const double skin_blood_flow_neutral = 6.3;
    double temp_skin = temp_skin_neutral;
    double temp_core = temp_core_neutral;
    double m_bl = skin_blood_flow_neutral;
    double rm;
    double m;
    double pressure_in_atmospheres;
    double vapor_pressure;
    int n_simulation = 1;
    double alfa = 0.1;
    double e_skin;
    double r_clo;
    double f_a_cl;
    double lr;
    double w_max;
    double i_cl;
    double h_cc;
    double h_fc;
    double h_r;
    double h_t;
    double r_a;
    double t_op;
    double temp_body;
    double q_res;
    double c_res;
    double q_sensible = 0.0;
    double e_rsw = 0.0;
    double e_max = 0.0;
    double w = 0.0;
    double q_skin;
    double p_ssk;
    double h_r_s;
    double h_c_s;
    double h_t_s;
    double r_clo_s;
    double r_cl_s;
    double f_a_cl_s;
    double fcls;
    double ims = 0.45;
    double i_m_s;
    double r_a_s;
    double r_ea_s;
    double r_ecl_s;
    double h_d_s;
    double h_e_s;
    double delta = 0.0001;
    double dx = 100.0;
    double set_old;
    int iter;
    int i;
    double t_cl;
    double h_r_factor;
    double t_cl_new;
    double hf_cs;
    double s_core;
    double s_skin;
    double tc_sk;
    double tc_cr;
    double d_t_sk;
    double d_t_cr;
    double sk_sig;
    double warm_sk;
    double cold_sk;
    double c_reg_sig;
    double c_warm;
    double c_cold;
    double bdsig;
    double warm_b;
    double reg_sw;
    double r_ea;
    double r_ecl;
    double pr_sw;
    double e_diff;
    double m_shiv;

    if (!finite_all7(tdb, tr, v, rh, met, clo, wme) ||
            !R_FINITE(body_surface_area) || !R_FINITE(p_atm)) {
        return NA_REAL;
    }

    air_speed = (v > 0.1) ? v : 0.1;
    rm = (met - wme) * met_factor;
    m = met * met_factor;
    pressure_in_atmospheres = p_atm / 101325.0;
    vapor_pressure = rh * comfort_p_sat_torr(tdb) / 100.0;
    e_skin = 0.1 * met;
    r_clo = 0.155 * clo;
    f_a_cl = 1.0 + 0.15 * clo;
    lr = 2.2 / pressure_in_atmospheres;

    if (clo <= 0.0) {
        w_max = 0.38 * pow(air_speed, -0.29);
        i_cl = 1.0;
    } else {
        w_max = 0.59 * pow(air_speed, -0.08);
        i_cl = 0.45;
    }

    h_cc = 3.0 * pow(pressure_in_atmospheres, 0.53);
    h_fc = 8.600001 * pow(air_speed * pressure_in_atmospheres, 0.53);
    h_cc = (h_cc > h_fc) ? h_cc : h_fc;
    if (met > 0.85) {
        double h_met = 5.66 * pow(met - 0.85, 0.39);
        h_cc = (h_cc > h_met) ? h_cc : h_met;
    }
    h_r = 4.7;
    h_t = h_r + h_cc;
    r_a = 1.0 / (f_a_cl * h_t);
    t_op = (h_r * tr + h_cc * tdb) / h_t;
    temp_body = alfa * temp_skin + (1.0 - alfa) * temp_core;
    q_res = 0.0023 * m * (44.0 - vapor_pressure);
    c_res = 0.0014 * m * (34.0 - tdb);
    h_r_factor = position_sitting ? 0.7 : 0.73;

    /* One-minute thermal-balance steps, following the reference SET model. */
    while (n_simulation < 60) {
        ++n_simulation;

        t_cl = (r_a * temp_skin + r_clo * t_op) / (r_a + r_clo);
        for (i = 0; i < 150; ++i) {
            h_r = 4.0 * 0.95 * sbc * pow((t_cl + tr) / 2.0 + 273.15, 3.0) *
                h_r_factor;
            h_t = h_r + h_cc;
            r_a = 1.0 / (f_a_cl * h_t);
            t_op = (h_r * tr + h_cc * tdb) / h_t;
            t_cl_new = (r_a * temp_skin + r_clo * t_op) / (r_a + r_clo);
            if (fabs(t_cl_new - t_cl) <= 0.01) {
                t_cl = t_cl_new;
                break;
            }
            t_cl = t_cl_new;
        }

        q_sensible = (temp_skin - t_op) / (r_a + r_clo);
        hf_cs = (temp_core - temp_skin) * (5.28 + 1.163 * m_bl);
        s_core = m - hf_cs - q_res - c_res - wme;
        s_skin = hf_cs - q_sensible - e_skin;
        tc_sk = 0.97 * alfa * body_weight;
        tc_cr = 0.97 * (1.0 - alfa) * body_weight;
        d_t_sk = (s_skin * body_surface_area) / (tc_sk * 60.0);
        d_t_cr = (s_core * body_surface_area) / (tc_cr * 60.0);
        temp_skin += d_t_sk;
        temp_core += d_t_cr;
        temp_body = alfa * temp_skin + (1.0 - alfa) * temp_core;
        sk_sig = temp_skin - temp_skin_neutral;
        warm_sk = (sk_sig > 0.0) ? sk_sig : 0.0;
        cold_sk = (-sk_sig > 0.0) ? -sk_sig : 0.0;
        c_reg_sig = temp_core - temp_core_neutral;
        c_warm = (c_reg_sig > 0.0) ? c_reg_sig : 0.0;
        c_cold = (-c_reg_sig > 0.0) ? -c_reg_sig : 0.0;
        bdsig = temp_body - temp_body_neutral;
        warm_b = (bdsig > 0.0) ? bdsig : 0.0;
        m_bl = (skin_blood_flow_neutral + c_dil * c_warm) /
            (1.0 + c_str * cold_sk);
        if (m_bl < 0.5) m_bl = 0.5;
        if (m_bl > 90.0) m_bl = 90.0;
        reg_sw = c_sw * warm_b * exp(warm_sk / 10.7);
        if (reg_sw > 500.0) reg_sw = 500.0;
        e_rsw = 0.68 * reg_sw;
        r_ea = 1.0 / (lr * f_a_cl * h_cc);
        r_ecl = r_clo / (lr * i_cl);
        e_max = (comfort_p_sat_torr(temp_skin) - vapor_pressure) /
            (r_ea + r_ecl);
        if (e_max == 0.0) {
            e_max = 0.001;
        }
        pr_sw = e_rsw / e_max;
        w = 0.06 + 0.94 * pr_sw;
        e_diff = w * e_max - e_rsw;
        if (w > w_max) {
            w = w_max;
            pr_sw = w_max / 0.94;
            e_rsw = pr_sw * e_max;
            e_diff = 0.06 * (1.0 - pr_sw) * e_max;
        }
        if (e_max < 0.0) {
            e_diff = 0.0;
            e_rsw = 0.0;
            w = w_max;
        }
        e_skin = e_rsw + e_diff;
        m_shiv = 19.4 * cold_sk * c_cold;
        m = rm + m_shiv;
        alfa = 0.0417737 + 0.7451833 / (m_bl + 0.585417);
    }

    q_skin = q_sensible + e_skin;
    p_ssk = comfort_p_sat_torr(temp_skin);
    h_r_s = h_r;
    h_c_s = 3.0 * pow(pressure_in_atmospheres, 0.53);
    if (met > 0.85) {
        double h_met = 5.66 * pow(met - 0.85, 0.39);
        h_c_s = (h_c_s > h_met) ? h_c_s : h_met;
    }
    if (h_c_s < 3.0) h_c_s = 3.0;
    h_t_s = h_c_s + h_r_s;
    r_clo_s = 1.52 / ((met - wme / met_factor) + 0.6944) - 0.1835;
    if (r_clo_s < 0.0) r_clo_s = 0.0;
    r_cl_s = 0.155 * r_clo_s;
    f_a_cl_s = 1.0 + k_clo * r_clo_s;
    fcls = 1.0 / (1.0 + 0.155 * f_a_cl_s * h_t_s * r_clo_s);
    i_m_s = ims * h_c_s / h_t_s * (1.0 - fcls) /
        (h_c_s / h_t_s - fcls * ims);
    r_a_s = 1.0 / (f_a_cl_s * h_t_s);
    r_ea_s = 1.0 / (lr * f_a_cl_s * h_c_s);
    r_ecl_s = r_cl_s / (lr * i_m_s);
    h_d_s = 1.0 / (r_a_s + r_cl_s);
    h_e_s = 1.0 / (r_ea_s + r_ecl_s);

    /* Solve the standard effective temperature from equivalent heat loss. */
    set_old = nearbyint((temp_skin - q_skin / h_d_s) * 100.0) / 100.0;
    for (iter = 0; fabs(dx) > 0.01 && iter < 1000; ++iter) {
        double err_1 = q_skin - h_d_s * (temp_skin - set_old) -
            w * h_e_s * (p_ssk - 0.5 * comfort_p_sat_torr(set_old));
        double err_2 = q_skin - h_d_s * (temp_skin - (set_old + delta)) -
            w * h_e_s * (p_ssk - 0.5 * comfort_p_sat_torr(set_old + delta));
        double denom = err_2 - err_1;
        double set_new;
        if (!R_FINITE(denom) || fabs(denom) < DBL_EPSILON) {
            return NA_REAL;
        }
        set_new = set_old - delta * err_1 / denom;
        dx = set_new - set_old;
        set_old = set_new;
    }

    return set_old;
}

static double comfort_pmv_at_humratio(double tdb, double hum_ratio,
                                      const PmvParams *p)
{
    /*
     * Root tracing happens in psychrometric chart coordinates: x = dry-bulb
     * temperature and y = humidity ratio.  PMV needs RH, so convert on demand.
     * Tiny over/under-shoots are clamped to keep roots continuous at the
     * saturation boundary; real out-of-domain points still return NA.
     */
    double rh = ggpsy_relhum_from_hum_ratio_si(
        tdb, hum_ratio, p->pressure, p->min_hum_ratio
    );
    double tr;

    if (!R_FINITE(rh)) {
        return NA_REAL;
    }
    rh *= 100.0;
    if (rh < 0.0 && rh >= -1e-3) {
        rh = 0.0;
    }
    if (rh > 100.0 && rh <= 100.0 + 1e-3) {
        rh = 100.0;
    }
    if (rh < 0.0 || rh > 100.0) {
        return NA_REAL;
    }

    tr = R_FINITE(p->tr) ? p->tr : tdb;
    return comfort_pmv_scalar(tdb, tr, p->vr, rh, p->met, p->clo, p->wme);
}

static PmvParams pmv_params_from_sexp(SEXP tr_sxp, SEXP vr_sxp, SEXP met_sxp,
                                      SEXP clo_sxp, SEXP wme_sxp,
                                      SEXP pressure_sxp,
                                      SEXP min_hum_ratio_sxp)
{
    PmvParams p;
    p.tr = scalar_or_na(tr_sxp);
    p.vr = scalar_or_na(vr_sxp);
    p.met = scalar_or_na(met_sxp);
    p.clo = scalar_or_na(clo_sxp);
    p.wme = scalar_or_na(wme_sxp);
    p.pressure = scalar_or_na(pressure_sxp);
    p.min_hum_ratio = scalar_or_na(min_hum_ratio_sxp);
    return p;
}

/* Vector wrappers assume R has already recycled all inputs to a common length. */
SEXP C_comfort_pmv_vec(SEXP tdb_sxp, SEXP tr_sxp, SEXP vr_sxp, SEXP rh_sxp,
                       SEXP met_sxp, SEXP clo_sxp, SEXP wme_sxp)
{
    R_xlen_t n = XLENGTH(tdb_sxp);
    SEXP out = PROTECT(allocVector(REALSXP, n));
    double *tdb = REAL(tdb_sxp);
    double *tr = REAL(tr_sxp);
    double *vr = REAL(vr_sxp);
    double *rh = REAL(rh_sxp);
    double *met = REAL(met_sxp);
    double *clo = REAL(clo_sxp);
    double *wme = REAL(wme_sxp);
    double *ans = REAL(out);
    R_xlen_t i;

    for (i = 0; i < n; ++i) {
        ans[i] = comfort_pmv_scalar(
            tdb[i], tr[i], vr[i], rh[i], met[i], clo[i], wme[i]
        );
    }

    UNPROTECT(1);
    return out;
}

/* Vector SET wrapper; scalar-only parameters stay scalar by design. */
SEXP C_comfort_set_vec(SEXP tdb_sxp, SEXP tr_sxp, SEXP v_sxp, SEXP rh_sxp,
                       SEXP met_sxp, SEXP clo_sxp, SEXP wme_sxp,
                       SEXP body_surface_area_sxp, SEXP p_atm_sxp,
                       SEXP position_sitting_sxp)
{
    R_xlen_t n = XLENGTH(tdb_sxp);
    double body_surface_area = scalar_or_na(body_surface_area_sxp);
    double p_atm = scalar_or_na(p_atm_sxp);
    int position_sitting = asLogical(position_sitting_sxp);
    SEXP out = PROTECT(allocVector(REALSXP, n));
    double *tdb = REAL(tdb_sxp);
    double *tr = REAL(tr_sxp);
    double *v = REAL(v_sxp);
    double *rh = REAL(rh_sxp);
    double *met = REAL(met_sxp);
    double *clo = REAL(clo_sxp);
    double *wme = REAL(wme_sxp);
    double *ans = REAL(out);
    R_xlen_t i;

    for (i = 0; i < n; ++i) {
        ans[i] = comfort_set_scalar(
            tdb[i], tr[i], v[i], rh[i], met[i], clo[i], wme[i],
            body_surface_area, p_atm, position_sitting == TRUE
        );
    }

    UNPROTECT(1);
    return out;
}

/*
 * Trace one PMV isoline by solving PMV(tdb, fixed_humratio) = level on each
 * humidity-ratio row.  The left bracket is moved to the dew point so the
 * solution never enters supersaturated air; the right bracket is the chart's
 * high dry-bulb limit.
 */
SEXP C_comfort_pmv_curve_roots(SEXP level_sxp, SEXP hum_ratio_sxp,
                               SEXP tdb_lim_sxp, SEXP pressure_sxp,
                               SEXP tr_sxp, SEXP vr_sxp, SEXP met_sxp,
                               SEXP clo_sxp, SEXP wme_sxp,
                               SEXP min_hum_ratio_sxp)
{
    double level = asReal(level_sxp);
    double *hum_ratio = REAL(hum_ratio_sxp);
    R_xlen_t n = XLENGTH(hum_ratio_sxp);
    double *tdb_lim = REAL(tdb_lim_sxp);
    PmvParams p = pmv_params_from_sexp(
        tr_sxp, vr_sxp, met_sxp, clo_sxp, wme_sxp,
        pressure_sxp, min_hum_ratio_sxp
    );
    SEXP tdb_out;
    SEXP hum_out;
    SEXP out;
    SEXP names;
    double *tdb_tmp;
    double *hum_tmp;
    R_xlen_t count = 0;
    R_xlen_t i;

    tdb_tmp = (double *) R_alloc((size_t) n, sizeof(double));
    hum_tmp = (double *) R_alloc((size_t) n, sizeof(double));

    for (i = 0; i < n; ++i) {
        double hum = hum_ratio[i];
        double lo = tdb_lim[0];
        double hi = tdb_lim[1];
        double saturation_hi;
        double flo;
        double fhi;
        double root;
        int exact_lo;
        int exact_hi;
        int j;

        /* At this humidity ratio, temperatures below dew point are invalid. */
        if (hum > 0.0) {
            double dew = ggpsy_dew_point_from_hum_ratio_si(
                hum, p.pressure, p.min_hum_ratio
            );
            if (R_FINITE(dew) && dew > lo) {
                lo = dew;
            }
        }
        saturation_hi = ggpsy_sat_hum_ratio_si(hi, p.pressure, p.min_hum_ratio);
        if (!R_FINITE(lo) || !R_FINITE(hi) || lo >= hi ||
                hum < 0.0 || !R_FINITE(hum) ||
                !R_FINITE(saturation_hi) || hum > saturation_hi) {
            continue;
        }

        /* PMV is monotone enough over valid chart rows for bisection here. */
        flo = comfort_pmv_at_humratio(lo, hum, &p) - level;
        fhi = comfort_pmv_at_humratio(hi, hum, &p) - level;
        if (!R_FINITE(flo) || !R_FINITE(fhi) || flo * fhi > 0.0) {
            continue;
        }

        exact_lo = fabs(flo) < 1e-8;
        exact_hi = fabs(fhi) < 1e-8;
        if (exact_lo) {
            root = lo;
        } else if (exact_hi) {
            root = hi;
        } else {
            for (j = 0; j < 44; ++j) {
                double mid = (lo + hi) / 2.0;
                double fmid = comfort_pmv_at_humratio(mid, hum, &p) - level;
                if (!R_FINITE(fmid)) {
                    break;
                }
                if ((fmid > 0.0) == (flo > 0.0)) {
                    lo = mid;
                    flo = fmid;
                } else {
                    hi = mid;
                    fhi = fmid;
                }
            }
            root = (lo + hi) / 2.0;
        }

        if (R_FINITE(root)) {
            tdb_tmp[count] = root;
            hum_tmp[count] = hum;
            ++count;
        }
    }

    tdb_out = PROTECT(allocVector(REALSXP, count));
    hum_out = PROTECT(allocVector(REALSXP, count));
    for (i = 0; i < count; ++i) {
        REAL(tdb_out)[i] = tdb_tmp[i];
        REAL(hum_out)[i] = hum_tmp[i];
    }
    out = PROTECT(allocVector(VECSXP, 2));
    names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("tdb"));
    SET_STRING_ELT(names, 1, mkChar("humratio"));
    SET_VECTOR_ELT(out, 0, tdb_out);
    SET_VECTOR_ELT(out, 1, hum_out);
    setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(4);
    return out;
}

/*
 * Find where a PMV isoline intersects the 100% RH saturation curve.  This is
 * needed because root-traced PMV curves are mostly sampled by horizontal chart
 * rows, while the psychrometric domain is closed by a curved saturation edge.
 */
SEXP C_comfort_pmv_saturation_roots(SEXP level_sxp, SEXP tdb_lim_sxp,
                                    SEXP hum_lim_sxp, SEXP n_sxp,
                                    SEXP pressure_sxp, SEXP tr_sxp,
                                    SEXP vr_sxp, SEXP met_sxp,
                                    SEXP clo_sxp, SEXP wme_sxp,
                                    SEXP min_hum_ratio_sxp)
{
    double level = asReal(level_sxp);
    double *tdb_lim = REAL(tdb_lim_sxp);
    double *hum_lim = REAL(hum_lim_sxp);
    int n = asInteger(n_sxp);
    PmvParams p = pmv_params_from_sexp(
        tr_sxp, vr_sxp, met_sxp, clo_sxp, wme_sxp,
        pressure_sxp, min_hum_ratio_sxp
    );
    SEXP tdb_out;
    SEXP hum_out;
    SEXP out;
    SEXP names;
    double *tdb_tmp;
    double *hum_tmp;
    double *tdb_grid;
    double *hum_grid;
    double *value;
    int count = 0;
    int i;

    if (n < 80) {
        n = 80;
    }

    tdb_tmp = (double *) R_alloc((size_t) n, sizeof(double));
    hum_tmp = (double *) R_alloc((size_t) n, sizeof(double));
    tdb_grid = (double *) R_alloc((size_t) n, sizeof(double));
    hum_grid = (double *) R_alloc((size_t) n, sizeof(double));
    value = (double *) R_alloc((size_t) n, sizeof(double));

    /* First scan the saturation curve to locate sign-changing intervals. */
    for (i = 0; i < n; ++i) {
        double frac = (n == 1) ? 0.0 : (double) i / (double) (n - 1);
        double tdb = tdb_lim[0] + (tdb_lim[1] - tdb_lim[0]) * frac;
        double hum = ggpsy_sat_hum_ratio_si(tdb, p.pressure, p.min_hum_ratio);
        tdb_grid[i] = tdb;
        hum_grid[i] = hum;
        if (R_FINITE(tdb) && R_FINITE(hum) &&
                hum >= hum_lim[0] && hum <= hum_lim[1]) {
            value[i] = comfort_pmv_at_humratio(tdb, hum, &p) - level;
        } else {
            value[i] = NA_REAL;
        }
        if (R_FINITE(value[i]) && fabs(value[i]) < 1e-8) {
            tdb_tmp[count] = tdb;
            hum_tmp[count] = hum;
            ++count;
        }
    }

    /* Then refine each crossing with bisection in dry-bulb temperature. */
    for (i = 0; i < n - 1; ++i) {
        double lo;
        double hi;
        double flo;
        double root;
        int j;

        if (!R_FINITE(value[i]) || !R_FINITE(value[i + 1]) ||
                value[i] * value[i + 1] >= 0.0) {
            continue;
        }

        lo = tdb_grid[i];
        hi = tdb_grid[i + 1];
        flo = value[i];
        for (j = 0; j < 44; ++j) {
            double mid = (lo + hi) / 2.0;
            double hum = ggpsy_sat_hum_ratio_si(
                mid, p.pressure, p.min_hum_ratio
            );
            double fmid = comfort_pmv_at_humratio(mid, hum, &p) - level;
            if (!R_FINITE(fmid)) {
                break;
            }
            if ((fmid > 0.0) == (flo > 0.0)) {
                lo = mid;
                flo = fmid;
            } else {
                hi = mid;
            }
        }
        root = (lo + hi) / 2.0;
        if (R_FINITE(root)) {
            double hum = ggpsy_sat_hum_ratio_si(
                root, p.pressure, p.min_hum_ratio
            );
            if (R_FINITE(hum) && hum >= hum_lim[0] && hum <= hum_lim[1]) {
                tdb_tmp[count] = root;
                hum_tmp[count] = hum;
                ++count;
            }
        }
    }

    tdb_out = PROTECT(allocVector(REALSXP, count));
    hum_out = PROTECT(allocVector(REALSXP, count));
    for (i = 0; i < count; ++i) {
        REAL(tdb_out)[i] = tdb_tmp[i];
        REAL(hum_out)[i] = hum_tmp[i];
    }
    out = PROTECT(allocVector(VECSXP, 2));
    names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("tdb"));
    SET_STRING_ELT(names, 1, mkChar("humratio"));
    SET_VECTOR_ELT(out, 0, tdb_out);
    SET_VECTOR_ELT(out, 1, hum_out);
    setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(4);
    return out;
}
