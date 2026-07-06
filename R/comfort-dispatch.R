#' @include comfort-core.R comfort-calc.R comfort-model.R
NULL

# Comfort dispatch bridges layer stats to public calculators while keeping
# plotting computations unrounded and unit-aware.

# Resolve the metric that a model should expose to field-based layers.
comfort_dispatch__model_metric <- function(model, metric = NULL) {
    comfort__check_model(model)
    if (!is.null(metric)) {
        return(as.character(metric)[[1L]])
    }
    switch(
        model$type,
        pmv = "pmv",
        set = "set",
        adaptive = "acceptability",
        heat_index = "heat_index"
    )
}

# Extract one numeric metric vector from a calculator result.
comfort_dispatch__metric_value <- function(result, metric) {
    if (!metric %in% names(result)) {
        stop(
            "Metric `",
            metric,
            "` is not produced by this comfort model.",
            call. = FALSE
        )
    }
    value <- result[[metric]]
    if (is.logical(value)) {
        return(as.numeric(value))
    }
    as.numeric(value)
}

# Evaluate a comfort model object on chart-space temperature and humidity values.
comfort_dispatch__apply_model <- function(model, tdb, rh, units, pres) {
    comfort__check_model(model)
    p <- model$params
    tr <- if (is.null(p$tr)) tdb else p$tr

    # Layer stats operate in chart coordinates, so dispatch here converts fixed
    # model parameters to the calculator API while preserving unrounded outputs.
    switch(
        model$type,
        pmv = comfort_pmv(
            tdb = tdb,
            tr = tr,
            vr = p$vr,
            rh = rh,
            met = p$met,
            clo = p$clo,
            wme = p$wme,
            units = units,
            limit_inputs = p$limit_inputs,
            round_output = p$round_output
        ),
        set = comfort_set(
            tdb = tdb,
            tr = tr,
            v = p$v,
            rh = rh,
            met = p$met,
            clo = p$clo,
            wme = p$wme,
            units = units,
            limit_inputs = p$limit_inputs,
            round_output = p$round_output,
            body_surface_area = p$body_surface_area,
            p_atm = if (is.null(p$p_atm)) {
                comfort__pressure_pa(pres, units)
            } else {
                p$p_atm
            },
            position = p$position
        ),
        adaptive = comfort_adaptive(
            tdb = tdb,
            tr = tr,
            t_running = p$t_running,
            v = p$v,
            standard = p$standard,
            category = p$category,
            units = units,
            limit_inputs = p$limit_inputs,
            round_output = p$round_output
        ),
        heat_index = comfort_heat_index(
            tdb = tdb,
            rh = rh,
            solar_exposure = p$solar_exposure,
            units = units,
            limit_inputs = p$limit_inputs,
            round_output = p$round_output
        )
    )
}

# Convert chart humidity ratio to relative humidity for comfort calculators.
comfort_dispatch__relhum_from_humratio <- function(tdb, humratio, units, pres) {
    rh <- psychrolib__with_units(
        units,
        psychrolib::GetRelHumFromHumRatio(tdb, humratio, pres)
    )
    rh * 100
}

# Clip tiny psychrolib roundoff outside the valid relative-humidity interval.
comfort_dispatch__clip_grid_rh <- function(rh) {
    tol <- 1e-3
    rh[is.finite(rh) & rh < 0 & rh >= -tol] <- 0
    rh[is.finite(rh) & rh > 100 & rh <= 100 + tol] <- 100
    rh
}

# Identify relative-humidity values valid for model evaluation.
comfort_dispatch__valid_grid_rh <- function(rh) {
    is.finite(rh) & rh >= 0 & rh <= 100
}
