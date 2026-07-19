#' @include comfort-core.R
NULL

# Comfort grid helpers build sampled dry-bulb/humidity-ratio domains shared
# by tile, band, contour, and label computations.

# Validate two-dimensional grid resolution for sampled comfort fields.
comfort_grid__n <- function(n) {
    n <- util__check_whole_count(n, "`n`", min = 2L, max_len = 2L)
    rep(n, length.out = 2L)
}

# Select model-specific default grid resolution when users omit n.
comfort_grid__default_n <- function(model, n = NULL) {
    if (!is.null(n)) {
        return(n)
    }
    switch(
        comfort__model_type(model),
        pmv = c(360L, 220L),
        set = c(80L, 50L),
        adaptive = c(240L, 160L),
        heat_index = c(160L, 100L)
    )
}
# Resolve chart limits used by grid-based comfort computations.
comfort_grid__limits <- function(units, tdb_lim, hum_lim) {
    default <- psychro__default_limits(units)
    list(
        tdb = if (is.null(tdb_lim)) default$tdb else tdb_lim,
        hum = if (is.null(hum_lim)) default$hum else hum_lim
    )
}

# Evaluate one comfort metric on a regular dry-bulb/humidity-ratio grid.
comfort_grid__matrix <- function(
    model,
    metric,
    n,
    units,
    pres,
    tdb_lim,
    hum_lim,
    at = c("centers", "nodes"),
    boundary = c("na", "saturation")
) {
    at <- match.arg(at)
    boundary <- match.arg(boundary)
    n <- comfort_grid__n(n)
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
    # Grid consumers need different sampling locations: nodes for isoband
    # topology, centers for tile values and label placement.
    tdb_edges <- seq(lim$tdb[[1L]], lim$tdb[[2L]], length.out = n[[1L]] + 1L)
    hum_display_edges <- seq(
        lim$hum[[1L]],
        lim$hum[[2L]],
        length.out = n[[2L]] + 1L
    )
    humratio_edges <- unit__hum_from_chart(hum_display_edges, units)
    if (at == "nodes") {
        tdb <- tdb_edges
        humratio <- humratio_edges
    } else {
        tdb <- (tdb_edges[-1L] + tdb_edges[-length(tdb_edges)]) / 2
        humratio <- (humratio_edges[-1L] +
            humratio_edges[-length(humratio_edges)]) /
            2
    }
    grid <- expand.grid(tdb = tdb, humratio = humratio)

    humratio_eval <- grid$humratio
    if (boundary == "saturation") {
        # Evaluate just inside saturation so psychrolib RH conversion remains
        # finite while contours still trace the visible saturation boundary.
        saturation <- zone__saturation_humratio(grid$tdb, units, pres)
        saturation_eps <- pmax(abs(saturation), 1) * sqrt(.Machine$double.eps)
        humratio_eval <- pmin(humratio_eval, saturation - saturation_eps)
        humratio_eval <- pmax(humratio_eval, 0)
    }

    rh <- comfort_dispatch__relhum_from_humratio(
        grid$tdb,
        humratio_eval,
        units,
        pres
    )
    metric <- comfort_dispatch__model_metric(model, metric)
    result <- comfort_dispatch__apply_model(model, grid$tdb, rh, units, pres)
    value <- comfort_dispatch__metric_value(result, metric)
    value[!comfort_dispatch__valid_grid_rh(rh)] <- NA_real_
    matrix_value <- matrix(value, nrow = length(tdb), ncol = length(humratio))

    list(
        tdb = tdb,
        humratio = humratio,
        tdb_edges = tdb_edges,
        humratio_edges = humratio_edges,
        value = matrix_value,
        metric = metric
    )
}

# Convert grid-center samples into tile data for direct sampled rendering.
comfort_grid__data <- function(
    model,
    metric,
    n,
    gap,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    na.rm = FALSE,
    psychro_scales = NULL
) {
    m <- comfort_grid__matrix(model, metric, n, units, pres, tdb_lim, hum_lim)
    gap <- bin__gap(gap)
    sat <- zone__saturation_humratio(m$tdb_edges, units, pres)

    nx <- length(m$tdb)
    ny <- length(m$humratio)
    ix <- rep(seq_len(nx), times = ny)
    iy <- rep(seq_len(ny), each = nx)

    x0 <- m$tdb_edges[ix]
    x1 <- m$tdb_edges[ix + 1L]
    y0 <- m$humratio_edges[iy]
    y1 <- m$humratio_edges[iy + 1L]
    s0 <- sat[ix]
    s1 <- sat[ix + 1L]
    x_width <- x1 - x0
    y_height <- y1 - y0

    keep <- is.finite(s0) &
        is.finite(s1) &
        x_width > 0 &
        y_height > 0 &
        y0 < pmax(s0, s1)
    if (!any(keep)) {
        return(comfort_grid__empty_tile())
    }

    value <- as.vector(m$value)
    missing <- keep & !is.finite(value)
    if (any(missing)) {
        # Cell centers can lie above saturation even when part of the tile is
        # visible; resample near the valid saturated edge instead of dropping it.
        value[missing] <- comfort_grid__boundary_values(
            model,
            m$metric,
            units,
            pres,
            x0[missing],
            x1[missing],
            y0[missing],
            y1[missing],
            s0[missing],
            s1[missing]
        )
    }

    keep <- keep & is.finite(value)
    if (!any(keep)) {
        return(comfort_grid__empty_tile())
    }

    out <- util__new_data_frame(list(
        tdb = (x0[keep] + x1[keep]) / 2,
        humratio = (y0[keep] + y1[keep]) / 2,
        width = x_width[keep] * (1 - gap),
        height = y_height[keep] * (1 - gap),
        value = value[keep],
        metric = rep(m$metric, sum(keep)),
        group = seq_len(sum(keep))
    ))
    out <- state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
    state__output_tile_size(
        out,
        x0[keep],
        x1[keep],
        y0[keep],
        y1[keep],
        mollier,
        psychro_scales = psychro_scales,
        units = units,
        gap = gap
    )
}
# Return an empty tile-shaped data frame with stable columns.
comfort_grid__empty_tile <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        x = numeric(),
        y = numeric(),
        width = numeric(),
        height = numeric(),
        value = numeric(),
        metric = character(),
        group = integer()
    ))
}

# Resample partially saturated tiles at an in-domain representative point.
comfort_grid__boundary_values <- function(
    model,
    metric,
    units,
    pres,
    x0,
    x1,
    y0,
    y1,
    s0,
    s1
) {
    # For partially clipped tiles, choose a representative point inside the
    # valid psychrometric domain so the color reflects the visible fragment.
    tdb <- (x0 + x1) / 2
    sat_mid <- (s0 + s1) / 2
    humratio <- pmin((y0 + y1) / 2, sat_mid - sqrt(.Machine$double.eps))

    outside_mid <- humratio <= y0
    if (any(outside_mid)) {
        eps <- sqrt(.Machine$double.eps)
        use_left <- s0[outside_mid] >= s1[outside_mid]
        tdb[outside_mid] <- ifelse(
            use_left,
            x0[outside_mid] + (x1[outside_mid] - x0[outside_mid]) * eps,
            x1[outside_mid] - (x1[outside_mid] - x0[outside_mid]) * eps
        )
        humratio[outside_mid] <- y0[outside_mid] +
            (y1[outside_mid] - y0[outside_mid]) * eps
    }

    rh <- comfort_dispatch__relhum_from_humratio(tdb, humratio, units, pres)
    rh <- comfort_dispatch__clip_grid_rh(rh)
    out <- rep(NA_real_, length(tdb))
    valid <- comfort_dispatch__valid_grid_rh(rh)
    if (any(valid)) {
        out[valid] <- comfort_dispatch__metric_value(
            comfort_dispatch__apply_model(
                model,
                tdb[valid],
                rh[valid],
                units,
                pres
            ),
            metric
        )
    }
    out
}
