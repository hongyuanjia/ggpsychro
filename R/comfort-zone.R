#' @include comfort-band.R comfort-pmv.R comfort-adaptive.R comfort-heat-index.R comfort-dispatch.R
NULL

# Comfort zone helpers turn model-specific acceptable ranges into filled
# polygon regions in the active psychrometric coordinate system.

# Build filled comfort zone polygons for PMV, SET, heat-index, or adaptive models.
comfort_zone__data <- function(
    model,
    metric,
    range,
    n,
    gap,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    na.rm = FALSE,
    rootband_levels = NULL,
    rootband_cache = NULL,
    psychro_scales = NULL
) {
    if (comfort__model_type(model) == "adaptive") {
        return(adaptive__zone(
            model,
            units,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        ))
    }

    metric <- comfort_dispatch__model_metric(model, metric)
    range <- comfort_zone__range(model, metric, range, units)
    if (comfort__model_type(model) == "pmv" && metric == "pmv") {
        return(pmv__band_data(
            model,
            range,
            n[[1L]],
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            rootband_levels = rootband_levels,
            rootband_cache = rootband_cache,
            psychro_scales = psychro_scales
        ))
    }
    comfort_band__data(
        model,
        metric,
        range,
        n,
        units,
        pres,
        mollier,
        tdb_lim,
        hum_lim,
        psychro_scales = psychro_scales
    )
}

# Resolve the value interval used by zone rendering.
comfort_zone__range <- function(model, metric, range, units = "SI") {
    if (!is.null(range)) {
        if (
            !is.numeric(range) ||
                length(range) != 2L ||
                any(!is.finite(range)) ||
                range[[1L]] >= range[[2L]]
        ) {
            stop("`range` must be a finite increasing pair.", call. = FALSE)
        }
        return(range)
    }
    switch(
        metric,
        pmv = c(-0.5, 0.5),
        set = c(22.2, 25.6),
        heat_index = heat_index__thresholds(units)[c(1L, 2L)],
        stop("A comfort `range` is required for this metric.", call. = FALSE)
    )
}
