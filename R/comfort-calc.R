#' @include comfort-core.R comfort-pmv.R comfort-adaptive.R comfort-heat-index.R
NULL

# Public calculation entry points convert user-facing units and return compact
# data frames; model equations stay in their own comfort-* modules.

#' Thermal comfort calculations
#'
#' These functions evaluate common comfort models without requiring Python at
#' runtime. Relative humidity is always supplied in percent. Temperature and
#' air-speed inputs follow `units`: SI uses degree C and m/s; IP uses degree F
#' and ft/s.
#'
#' @param tdb Dry-bulb air temperature.
#' @param tr Mean radiant temperature. Defaults to `tdb`.
#' @param vr Relative air speed for PMV.
#' @param v Air speed for SET and adaptive comfort.
#' @param rh Relative humidity in percent.
#' @param solar_exposure Relative solar exposure for heat index, from 0 to 1.
#' @param met Metabolic rate in met.
#' @param clo Clothing insulation in clo.
#' @param wme External work in met.
#' @param units Unit system, `"SI"` or `"IP"`.
#' @param limit_inputs If `TRUE`, values outside the model applicability range
#'   are returned as `NA`.
#' @param round_output If `TRUE`, round outputs like `pythermalcomfort`.
#'
#' @return A data frame with model outputs.
#'
#' @examples
#' comfort_pmv(25, rh = 50, met = 1.4, clo = 0.5)
#' comfort_set(25, rh = 50)
#' comfort_adaptive(25, t_running = 20)
#' comfort_heat_index(32, rh = 70)
#'
#' @export
comfort_pmv <- function(
    tdb,
    tr = tdb,
    vr = 0.1,
    rh,
    met = 1.2,
    clo = 0.5,
    wme = 0,
    units = c("SI", "IP"),
    limit_inputs = TRUE,
    round_output = TRUE
) {
    units <- match.arg(units)
    x <- comfort__recycle(
        tdb = tdb,
        tr = tr,
        vr = vr,
        rh = rh,
        met = met,
        clo = clo,
        wme = wme
    )

    tdb_si <- comfort__to_si_temp(x$tdb, units)
    tr_si <- comfort__to_si_temp(x$tr, units)
    vr_si <- comfort__to_si_speed(x$vr, units)

    pmv <- pmv__vec(tdb_si, tr_si, vr_si, x$rh, x$met, x$clo, x$wme)

    # ISO 7730 maps PMV to predicted percentage dissatisfied with an empirical
    # even-power curve, so warm and cool deviations are treated symmetrically.
    ppd <- 100 - 95 * exp(-0.03353 * pmv^4 - 0.2179 * pmv^2)

    if (isTRUE(limit_inputs)) {
        valid <- comfort__between(tdb_si, 10, 30) &
            comfort__between(tr_si, 10, 40) &
            comfort__between(vr_si, 0, 1) &
            comfort__between(x$rh, 0, 100) &
            comfort__between(x$met, 0.8, 4) &
            comfort__between(x$clo, 0, 2) &
            comfort__between(pmv, -2, 2)
        pmv[!valid] <- NA_real_
        ppd[!valid] <- NA_real_
    }

    if (isTRUE(round_output)) {
        pmv <- round(pmv, 2L)
        ppd <- round(ppd, 1L)
    }

    util__new_data_frame(list(
        pmv = pmv,
        ppd = ppd,
        tsv = pmv__thermal_sensation(pmv)
    ))
}

#' @rdname comfort_pmv
#' @param body_surface_area Body surface area in square meters for SET.
#' @param p_atm Atmospheric pressure in Pa.
#' @param position Body position, `"standing"` or `"sitting"`.
#' @export
comfort_set <- function(
    tdb,
    tr = tdb,
    v = 0.1,
    rh,
    met = 1.2,
    clo = 0.5,
    wme = 0,
    units = c("SI", "IP"),
    limit_inputs = TRUE,
    round_output = TRUE,
    body_surface_area = 1.8258,
    p_atm = 101325,
    position = c("standing", "sitting")
) {
    units <- match.arg(units)
    position <- match.arg(position)
    x <- comfort__recycle(
        tdb = tdb,
        tr = tr,
        v = v,
        rh = rh,
        met = met,
        clo = clo,
        wme = wme
    )

    tdb_si <- comfort__to_si_temp(x$tdb, units)
    tr_si <- comfort__to_si_temp(x$tr, units)
    v_si <- comfort__to_si_speed(x$v, units)

    set <- set__vec(
        tdb_si,
        tr_si,
        v_si,
        x$rh,
        x$met,
        x$clo,
        x$wme,
        body_surface_area = body_surface_area,
        p_atm = p_atm,
        position = position
    )

    if (isTRUE(limit_inputs)) {
        valid <- comfort__between(tdb_si, 10, 40) &
            comfort__between(tr_si, 10, 40) &
            comfort__between(v_si, 0, 2) &
            comfort__between(x$rh, 0, 100) &
            comfort__between(x$met, 1, 4) &
            comfort__between(x$clo, 0, 1.5)
        set[!valid] <- NA_real_
    }

    set <- comfort__from_si_temp(set, units)
    if (isTRUE(round_output)) {
        set <- round(set, 1L)
    }

    util__new_data_frame(list(set = set))
}

#' @rdname comfort_pmv
#' @param t_running Running mean outdoor temperature for adaptive comfort.
#' @param standard Adaptive comfort standard, either `"ashrae55"` or
#'   `"en16798"`.
#' @param category Comfort category. For ASHRAE 55 use `"80"` or `"90"`; for
#'   EN 16798 use `"I"`, `"II"`, or `"III"`.
#' @export
comfort_adaptive <- function(
    tdb,
    tr = tdb,
    t_running,
    v = 0.1,
    standard = c("ashrae55", "en16798"),
    category = NULL,
    units = c("SI", "IP"),
    limit_inputs = TRUE,
    round_output = TRUE
) {
    units <- match.arg(units)
    standard <- match.arg(standard)
    x <- comfort__recycle(tdb = tdb, tr = tr, t_running = t_running, v = v)

    tdb_si <- comfort__to_si_temp(x$tdb, units)
    tr_si <- comfort__to_si_temp(x$tr, units)
    t_running_si <- comfort__to_si_temp(x$t_running, units)
    v_si <- comfort__to_si_speed(x$v, units)

    if (standard == "ashrae55") {
        out <- adaptive__ashrae(
            tdb_si,
            tr_si,
            t_running_si,
            v_si,
            category = category,
            limit_inputs = limit_inputs,
            round_output = round_output
        )
    } else {
        out <- adaptive__en(
            tdb_si,
            tr_si,
            t_running_si,
            v_si,
            category = category,
            limit_inputs = limit_inputs,
            round_output = round_output
        )
    }

    temp_cols <- setdiff(
        names(out),
        c(
            "standard",
            "acceptability",
            "acceptability_80",
            "acceptability_90",
            "acceptability_cat_i",
            "acceptability_cat_ii",
            "acceptability_cat_iii"
        )
    )
    for (col in temp_cols) {
        if (is.numeric(out[[col]])) {
            out[[col]] <- comfort__from_si_temp(out[[col]], units)
        }
    }

    out
}

#' @rdname comfort_pmv
#' @export
comfort_heat_index <- function(
    tdb,
    rh,
    solar_exposure = 0,
    units = c("SI", "IP"),
    limit_inputs = TRUE,
    round_output = TRUE
) {
    units <- match.arg(units)
    x <- comfort__recycle(
        tdb = tdb,
        rh = rh,
        solar_exposure = solar_exposure
    )

    tdb_si <- comfort__to_si_temp(x$tdb, units)
    tdb_f <- unit__f_from_c(tdb_si)
    exposure <- x$solar_exposure
    # Missing exposure follows the vectorized calculator convention and returns
    # NA, but finite values outside Marsh's 0..1 scale are input errors.
    bad_exposure <- is.finite(exposure) & (exposure < 0 | exposure > 1)
    if (any(bad_exposure)) {
        stop("`solar_exposure` must be from 0 to 1.", call. = FALSE)
    }
    heat_index_f <- heat_index__value_f(tdb_f, x$rh, exposure)

    if (isTRUE(limit_inputs)) {
        valid <- comfort__between(tdb_si, -50, 100) &
            comfort__between(x$rh, 0, 100) &
            is.finite(exposure)
        heat_index_f[!valid] <- NA_real_
    }

    category <- heat_index__category(heat_index_f)
    heat_index <- if (units == "IP") {
        heat_index_f
    } else {
        unit__c_from_f(heat_index_f)
    }
    if (isTRUE(round_output)) {
        heat_index <- round(heat_index, 1L)
    }

    util__new_data_frame(list(
        heat_index = heat_index,
        category = category$category,
        category_id = category$category_id
    ))
}
