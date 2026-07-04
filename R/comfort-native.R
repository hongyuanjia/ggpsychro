#' @include comfort-core.R
NULL

# Native comfort bridges keep R-facing calculators separate from C entry points.

# Call the native vectorized PMV evaluator.
pmv__vec <- function(tdb, tr, vr, rh, met, clo, wme) {
    .Call(
        C_comfort_pmv_vec,
        as.numeric(tdb),
        as.numeric(tr),
        as.numeric(vr),
        as.numeric(rh),
        as.numeric(met),
        as.numeric(clo),
        as.numeric(wme)
    )
}

# Call the native vectorized SET evaluator.
set__vec <- function(
    tdb,
    tr,
    v,
    rh,
    met,
    clo,
    wme,
    body_surface_area,
    p_atm,
    position
) {
    .Call(
        C_comfort_set_vec,
        as.numeric(tdb),
        as.numeric(tr),
        as.numeric(v),
        as.numeric(rh),
        as.numeric(met),
        as.numeric(clo),
        as.numeric(wme),
        as.numeric(body_surface_area),
        as.numeric(p_atm),
        isTRUE(position == "sitting")
    )
}

# Convert scalar PMV model parameters into the native root-tracer contract.
pmv__native_params <- function(model, units, pres) {
    p <- model$params
    if (isTRUE(p$limit_inputs)) {
        return(NULL)
    }
    # The C root tracer assumes scalar fixed parameters and no input clipping;
    # vectorized or limited models fall back to the R implementation.
    numeric_params <- c("vr", "met", "clo", "wme")
    for (param in numeric_params) {
        value <- p[[param]]
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
            return(NULL)
        }
    }
    tr <- NA_real_
    if (!is.null(p$tr)) {
        if (!is.numeric(p$tr) || length(p$tr) != 1L || !is.finite(p$tr)) {
            return(NULL)
        }
        tr <- comfort_to_si_temp(as.numeric(p$tr), units)
    }
    list(
        tr = tr,
        vr = comfort_to_si_speed(as.numeric(p$vr), units),
        met = as.numeric(p$met),
        clo = as.numeric(p$clo),
        wme = as.numeric(p$wme),
        pressure = comfort_pressure_pa(as.numeric(pres), units),
        min_hum_ratio = psychrolib__options()$MIN_HUM_RATIO
    )
}

# Trace PMV roots at fixed humidity ratios with the native implementation.
pmv__native_curve_roots <- function(
    model,
    level,
    humratio,
    tdb_lim,
    units,
    pres
) {
    p <- pmv__native_params(model, units, pres)
    if (is.null(p)) {
        return(NULL)
    }
    roots <- .Call(
        C_comfort_pmv_curve_roots,
        as.numeric(level),
        as.numeric(humratio),
        as.numeric(comfort_to_si_temp(tdb_lim, units)),
        as.numeric(p$pressure),
        as.numeric(p$tr),
        as.numeric(p$vr),
        as.numeric(p$met),
        as.numeric(p$clo),
        as.numeric(p$wme),
        as.numeric(p$min_hum_ratio)
    )
    roots$tdb <- comfort_from_si_temp(roots$tdb, units)
    roots
}

# Trace PMV roots along saturation with the native implementation.
pmv__native_saturation_roots <- function(
    model,
    level,
    tdb_lim,
    hum_lim,
    units,
    pres,
    n
) {
    p <- pmv__native_params(model, units, pres)
    if (is.null(p)) {
        return(NULL)
    }
    roots <- .Call(
        C_comfort_pmv_saturation_roots,
        as.numeric(level),
        as.numeric(comfort_to_si_temp(tdb_lim, units)),
        as.numeric(unit__hum_from_chart(hum_lim, units)),
        as.integer(max(as.integer(n), 80L)),
        as.numeric(p$pressure),
        as.numeric(p$tr),
        as.numeric(p$vr),
        as.numeric(p$met),
        as.numeric(p$clo),
        as.numeric(p$wme),
        as.numeric(p$min_hum_ratio)
    )
    roots$tdb <- comfort_from_si_temp(roots$tdb, units)
    roots
}
