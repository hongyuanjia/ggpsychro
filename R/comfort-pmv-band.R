#' @include comfort-band.R comfort-dispatch.R comfort-pmv-curve.R comfort-pmv-label.R
NULL

# PMV band helpers assemble root-traced contour edges into filled polygons.

# Build filled PMV bands by tracing roots instead of gridded isobands.
pmv__root_band_data <- function(
    model,
    metric,
    levels,
    n,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    rootband_cache = NULL,
    psychro_scales = NULL
) {
    metric <- comfort_dispatch__model_metric(model, metric)
    if (comfort__model_type(model) != "pmv" || metric != "pmv") {
        stop(
            "Root-traced comfort overlay bands are only available for PMV.",
            call. = FALSE
        )
    }

    model <- pmv__curve_model(model)
    breaks <- pmv__root_band_breaks(levels)
    n <- comfort_grid__n(n)
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
    key <- pmv__cache_key(
        kind = "rootband",
        model = model,
        metric = metric,
        breaks = breaks,
        n = n,
        units = units,
        pres = pres,
        tdb = lim$tdb,
        hum = lim$hum,
        mollier = mollier,
        scale_tdb = psychro_scale_cache_key(psychro_scales$pos_tdb),
        scale_hum = psychro_scale_cache_key(psychro_scales$pos_hum)
    )
    if (
        !is.null(rootband_cache) &&
            exists(key, envir = rootband_cache, inherits = FALSE)
    ) {
        return(get(key, envir = rootband_cache, inherits = FALSE))
    }
    # Saturation roots are shared by the humidity sampling grid and by each band
    # edge, avoiding duplicate solves on the curved upper boundary.
    saturation_roots <- pmv__root_band_saturation_roots(
        model,
        breaks,
        lim$tdb,
        lim$hum,
        units,
        pres,
        n[[1L]],
        rootband_cache = rootband_cache
    )
    humratio <- pmv__root_band_humratio(
        model,
        breaks,
        n[[1L]],
        units,
        pres,
        lim$tdb,
        lim$hum,
        saturation_roots = saturation_roots
    )
    domain <- pmv__root_band_domain(humratio, lim$tdb, units, pres)
    valid <- domain$valid
    if (!any(valid)) {
        out <- comfort_band__empty()
        if (!is.null(rootband_cache)) {
            assign(key, out, envir = rootband_cache)
        }
        return(out)
    }

    humratio <- humratio[valid]
    xlo <- domain$xlo[valid]
    xhi <- domain$xhi[valid]
    pmv_lo <- pmv__value_at(model, xlo, humratio, units, pres)
    pmv_hi <- pmv__value_at(model, xhi, humratio, units, pres)
    valid <- is.finite(pmv_lo) & is.finite(pmv_hi) & pmv_lo <= pmv_hi
    if (!any(valid)) {
        out <- comfort_band__empty()
        if (!is.null(rootband_cache)) {
            assign(key, out, envir = rootband_cache)
        }
        return(out)
    }

    humratio <- humratio[valid]
    xlo <- xlo[valid]
    xhi <- xhi[valid]
    pmv_lo <- pmv_lo[valid]
    pmv_hi <- pmv_hi[valid]

    # Build one root vector per PMV break on the shared humidity grid. When a
    # break intersects saturation exactly, overwrite the row with that root.
    roots <- lapply(breaks, function(level) {
        roots <- pmv__curve_root_vector(
            model,
            level,
            humratio,
            lim$tdb,
            units,
            pres
        )
        sat_roots <- saturation_roots[[as.character(level)]]
        key <- match(round(sat_roots$humratio, 12L), round(humratio, 12L))
        ok <- !is.na(key)
        roots[key[ok]] <- sat_roots$tdb[ok]
        roots
    })
    names(roots) <- as.character(breaks)

    lows <- c(-Inf, breaks)
    highs <- c(breaks, Inf)
    values <- c(
        breaks[[1L]],
        (breaks[-length(breaks)] + breaks[-1L]) / 2,
        breaks[[length(breaks)]]
    )
    overlap <- diff(range(lim$tdb)) / 10000

    # Bands are assembled row-wise between adjacent PMV roots, then contiguous
    # rows are stitched into polygons for ggplot.
    polys <- list()
    for (i in seq_along(values)) {
        low <- lows[[i]]
        high <- highs[[i]]
        band <- pmv__root_band_edges(
            low,
            high,
            breaks,
            roots,
            xlo,
            xhi,
            pmv_lo,
            pmv_hi,
            overlap
        )
        if (!any(band$keep)) {
            next
        }

        runs <- split(
            which(band$keep),
            cumsum(c(TRUE, diff(which(band$keep)) != 1L))
        )
        for (run in runs) {
            if (length(run) < 2L) {
                next
            }
            left <- band$left[run]
            right <- band$right[run]
            y <- humratio[run]
            width_tol <- pmv__root_band_width_tol(left, right)
            ok <- is.finite(left) &
                is.finite(right) &
                is.finite(y) &
                left <= right + width_tol
            if (sum(ok) < 2L) {
                next
            }
            left <- left[ok]
            right <- right[ok]
            right <- pmax(right, left)
            y <- y[ok]
            group <- length(polys) + 1L
            polys[[group]] <- util__new_data_frame(list(
                tdb = c(left, rev(right)),
                humratio = c(y, rev(y)),
                edge = c(
                    rep("left", length(left)),
                    rep("right", length(right))
                ),
                edge_level = c(
                    band$left_level[run][ok],
                    rev(band$right_level[run][ok])
                ),
                level = sprintf(
                    "%s:%s",
                    pmv__format_band_level(low),
                    pmv__format_band_level(high)
                ),
                level_low = low,
                level_high = high,
                level_mid = values[[i]],
                value = values[[i]],
                group = group,
                subgroup = 1L,
                metric = "pmv"
            ))
        }
    }

    if (!length(polys)) {
        out <- comfort_band__empty()
        if (!is.null(rootband_cache)) {
            assign(key, out, envir = rootband_cache)
        }
        return(out)
    }

    out <- do.call(rbind, polys)
    row.names(out) <- NULL
    out <- state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
    if (!is.null(rootband_cache)) {
        assign(key, out, envir = rootband_cache)
    }
    out
}

# Extract one PMV comfort band from a root-traced band set.
pmv__band_data <- function(
    model,
    range,
    n,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    rootband_levels = NULL,
    rootband_cache = NULL,
    psychro_scales = NULL
) {
    range <- comfort__check_breaks(range, "`range`", n_min = 2L)
    if (length(range) != 2L) {
        stop("`range` must contain exactly two PMV boundaries.", call. = FALSE)
    }
    # When a caller supplies a wider break set, compute the full rootband once
    # and filter back to this visible band. Plain comfort_layer__zone() leaves it
    # NULL so its historical per-range behavior is unchanged.
    levels <- if (is.null(rootband_levels)) {
        range
    } else {
        comfort__check_breaks(
            c(rootband_levels, range),
            "`rootband_levels`",
            n_min = 2L
        )
    }

    bands <- pmv__root_band_data(
        model,
        "pmv",
        levels,
        n,
        units,
        pres,
        mollier,
        tdb_lim,
        hum_lim,
        rootband_cache = rootband_cache,
        psychro_scales = psychro_scales
    )
    if (!nrow(bands)) {
        return(comfort_band__empty())
    }

    keep <- is.finite(bands$level_low) &
        is.finite(bands$level_high) &
        abs(bands$level_low - range[[1L]]) <= 1e-8 &
        abs(bands$level_high - range[[2L]]) <= 1e-8
    out <- bands[keep, , drop = FALSE]
    if (!nrow(out)) {
        return(comfort_band__empty())
    }
    out$value <- mean(range)
    out$level_mid <- mean(range)
    out
}

# Normalize root-band levels into sorted PMV break points.
pmv__root_band_breaks <- function(levels) {
    if (is.null(levels)) {
        return(seq(-3, 3, by = 0.25))
    }
    if (length(levels) == 1L) {
        n <- util__check_whole_count(
            levels[[1L]],
            "`levels`",
            min = 1L,
            len = 1L
        )
        return(seq(-3, 3, length.out = n + 1L))
    }
    comfort__check_breaks(levels, "`levels`", n_min = 2L)
}

# Format root-band open-ended and finite PMV boundaries.
pmv__format_band_level <- function(level) {
    if (!is.finite(level)) {
        return(if (level < 0) "-Inf" else "Inf")
    }
    pmv__format_level(level)
}

# Compute saturation-boundary roots for all PMV root-band breaks.
pmv__root_band_saturation_roots <- function(
    model,
    breaks,
    tdb_lim,
    hum_lim,
    units,
    pres,
    n,
    rootband_cache = NULL
) {
    roots <- lapply(breaks, function(level) {
        pmv__saturation_roots_cached(
            model,
            level,
            tdb_lim,
            hum_lim,
            units,
            pres,
            n,
            rootband_cache = rootband_cache
        )
    })
    names(roots) <- as.character(breaks)
    roots
}

# Return cached saturation-boundary roots for one PMV break.
pmv__saturation_roots_cached <- function(
    model,
    level,
    tdb_lim,
    hum_lim,
    units,
    pres,
    n,
    rootband_cache = NULL
) {
    # Adjacent PMV bands share saturation-boundary intersections. Caching them
    # separately avoids resolving the same curved-boundary root for each band.
    key <- pmv__cache_key(
        kind = "saturation_roots",
        model = model,
        level = level,
        n = n,
        units = units,
        pres = pres,
        tdb = tdb_lim,
        hum = hum_lim
    )
    if (
        !is.null(rootband_cache) &&
            exists(key, envir = rootband_cache, inherits = FALSE)
    ) {
        return(get(key, envir = rootband_cache, inherits = FALSE))
    }
    roots <- pmv__curve_saturation_roots(
        model,
        level,
        tdb_lim,
        hum_lim,
        units,
        pres,
        n
    )
    if (!is.null(rootband_cache)) {
        assign(key, roots, envir = rootband_cache)
    }
    roots
}

# Build the humidity-ratio sampling grid for root-traced PMV bands.
pmv__root_band_humratio <- function(
    model,
    breaks,
    n,
    units,
    pres,
    tdb_lim,
    hum_lim,
    saturation_roots = NULL
) {
    hum <- seq(
        unit__hum_from_chart(hum_lim[[1L]], units),
        unit__hum_from_chart(hum_lim[[2L]], units),
        length.out = n
    )
    sat <- zone__saturation_humratio(tdb_lim, units, pres)
    hum <- c(hum, sat[is.finite(sat)])
    if (is.null(saturation_roots)) {
        saturation_roots <- pmv__root_band_saturation_roots(
            model,
            breaks,
            tdb_lim,
            hum_lim,
            units,
            pres,
            n
        )
    }
    for (level in breaks) {
        roots <- saturation_roots[[as.character(level)]]
        # Include saturation-root humidity ratios in the sampling grid so band
        # polygons close exactly where PMV boundaries meet the chart envelope.
        hum <- c(hum, roots$humratio)
    }
    hum <- sort(unique(round(hum[is.finite(hum)], 12L)))
    hum_lim <- unit__hum_from_chart(hum_lim, units)
    hum[hum >= hum_lim[[1L]] & hum <= hum_lim[[2L]]]
}

# Compute dry-bulb domain limits at each PMV band humidity sample.
pmv__root_band_domain <- function(humratio, tdb_lim, units, pres) {
    xlo <- rep(tdb_lim[[1L]], length(humratio))
    positive <- humratio > 0
    if (any(positive)) {
        dew <- psychrolib__with_units(
            units,
            GetTDewPointFromHumRatioOnly(
                humratio[positive],
                rep(as.numeric(pres), length.out = sum(positive))
            )
        )
        xlo[positive] <- pmax(xlo[positive], dew)
    }
    xhi <- rep(tdb_lim[[2L]], length(humratio))
    valid <- is.finite(xlo) & is.finite(xhi) & humratio >= 0 & xlo < xhi
    list(xlo = xlo, xhi = xhi, valid = valid)
}

# Compute left and right dry-bulb edges for one root-traced PMV band.
pmv__root_band_edges <- function(
    low,
    high,
    breaks,
    roots,
    xlo,
    xhi,
    pmv_lo,
    pmv_hi,
    overlap = 0
) {
    # Root tracing at saturation can differ from endpoint PMV by a few ulps;
    # keep those edge rows so filled bands do not show visible holes.
    value_tol <- 5e-5
    keep <- is.finite(pmv_lo) &
        is.finite(pmv_hi) &
        pmv_hi >= low - value_tol &
        pmv_lo <= high + value_tol
    left <- rep(NA_real_, length(xlo))
    right <- rep(NA_real_, length(xlo))
    left_level <- rep(NA_real_, length(xlo))
    right_level <- rep(NA_real_, length(xlo))
    if (!any(keep)) {
        return(list(
            left = left,
            right = right,
            keep = keep,
            left_level = left_level,
            right_level = right_level
        ))
    }

    if (is.finite(low)) {
        low_root <- roots[[as.character(low)]]
        use_root <- pmv_lo < low - value_tol &
            pmv_hi >= low - value_tol &
            is.finite(low_root)
        on_domain <- is.finite(pmv_lo) & abs(pmv_lo - low) <= value_tol
        left <- ifelse(use_root, low_root, xlo)
        left_level[use_root | on_domain] <- low
    } else {
        left <- xlo
    }

    if (is.finite(high)) {
        high_root <- roots[[as.character(high)]]
        use_root <- pmv_lo <= high + value_tol &
            pmv_hi > high + value_tol &
            is.finite(high_root)
        on_domain <- is.finite(pmv_lo) & abs(pmv_lo - high) <= value_tol
        right <- ifelse(use_root, high_root, xhi)
        right[on_domain] <- xlo[on_domain]
        right_level[use_root | on_domain] <- high
    } else {
        right <- xhi
    }

    inner_left <- is.finite(low) & is.finite(left) & abs(left - xlo) > overlap
    inner_right <- is.finite(high) &
        is.finite(right) &
        abs(right - xhi) > overlap
    # A tiny overlap avoids visual cracks between adjacent filled PMV bands
    # while keeping the computed edge level available for boundary validation.
    left[inner_left] <- pmax(xlo[inner_left], left[inner_left] - overlap)
    right[inner_right] <- pmin(xhi[inner_right], right[inner_right] + overlap)
    width_tol <- pmv__root_band_width_tol(left, right)
    keep <- keep &
        is.finite(left) &
        is.finite(right) &
        left <= right + width_tol
    right[keep] <- pmax(right[keep], left[keep])
    list(
        left = left,
        right = right,
        keep = keep,
        left_level = left_level,
        right_level = right_level
    )
}

# Return a scale-aware tolerance for comparing PMV band edge widths.
pmv__root_band_width_tol <- function(left, right) {
    x <- c(left, right)
    x <- x[is.finite(x)]
    if (!length(x)) {
        return(sqrt(.Machine$double.eps))
    }
    sqrt(.Machine$double.eps) * max(1, max(abs(x)))
}
