#' @include comfort-core.R
NULL

# PMV/SET native bridges and PMV root tracing. R root tracers remain as
# fallbacks when the native tracer's scalar-parameter contract is not met.
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

# Convert PMV values into thermal sensation vote labels.
pmv__thermal_sensation <- function(pmv) {
    labels <- c(
        "Cold",
        "Cool",
        "Slightly Cool",
        "Neutral",
        "Slightly Warm",
        "Warm",
        "Hot"
    )
    out <- labels[findInterval(pmv, c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)) + 1L]
    out[is.na(pmv)] <- NA_character_
    out
}

# Convert PMV curve roots into plot-ready line or label data.
pmv__curve_data <- function(
    model,
    levels,
    n,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    label = c("none", "sensation", "boundary", "comfort"),
    label_hjust = NULL,
    label_vjust = NULL,
    reverse = FALSE,
    curve_cache = NULL,
    psychro_scales = NULL
) {
    label <- match.arg(label)
    out <- pmv__curve_base_data(
        model,
        levels,
        n,
        units,
        pres,
        tdb_lim,
        hum_lim,
        curve_cache = curve_cache
    )
    if (!nrow(out)) {
        return(pmv__empty_curve())
    }
    out$label <- vapply(
        out$level,
        pmv__curve_label,
        character(1L),
        label = label
    )
    out$hjust <- pmv__curve_hjust(label, label_hjust)
    out$vjust <- vapply(
        out$level,
        pmv__curve_vjust,
        numeric(1L),
        label = label,
        override = label_vjust,
        mollier = mollier
    )

    if (label != "none") {
        out <- out[!is.na(out$label), , drop = FALSE]
    }
    if (!nrow(out)) {
        return(pmv__empty_curve())
    }
    if (isTRUE(reverse)) {
        out <- pmv__reverse_groups(out)
    }
    psychro_output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}

# Trace base constant-PMV curves before coordinate transformation.
pmv__curve_base_data <- function(
    model,
    levels,
    n,
    units,
    pres,
    tdb_lim,
    hum_lim,
    curve_cache = NULL
) {
    levels <- comfort_check_breaks(levels, "`levels`", n_min = 1L)
    n <- pmv__curve_n(n)
    model <- pmv__curve_model(model)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    # Constant-PMV curves are traced by solving dry-bulb roots on humidity-ratio
    # samples, then adding saturation-boundary roots so curves close cleanly.
    humratio <- seq(
        unit__hum_from_chart(lim$hum[[1L]], units),
        unit__hum_from_chart(lim$hum[[2L]], units),
        length.out = n
    )

    curves <- vector("list", length(levels))
    for (i in seq_along(levels)) {
        roots <- pmv__curve_level_roots(
            model,
            levels[[i]],
            humratio,
            n,
            units,
            pres,
            lim,
            curve_cache = curve_cache
        )
        if (!length(roots$tdb)) {
            next
        }
        curves[[i]] <- util__new_data_frame(list(
            tdb = roots$tdb,
            humratio = roots$humratio,
            level = levels[[i]],
            value = levels[[i]],
            group = i,
            linetype = pmv__linetype(levels[[i]]),
            metric = "pmv"
        ))
    }
    curves <- curves[!vapply(curves, is.null, logical(1L))]
    if (!length(curves)) {
        return(pmv__empty_curve_base())
    }
    out <- do.call(rbind, curves)
    row.names(out) <- NULL
    out
}

# Build a cache key for root-tracing work in one coordinate context.
pmv__cache_key <- function(...) {
    # Root caches are local environments, not global memoization. The key still
    # includes model parameters and chart limits so sibling stats cannot reuse
    # roots computed for a different coordinate context.
    paste(
        utils::capture.output(utils::str(list(...), give.attr = FALSE)),
        collapse = "\n"
    )
}

# Return cached roots for one PMV level, including saturation intersections.
pmv__curve_level_roots <- function(
    model,
    level,
    humratio,
    n,
    units,
    pres,
    lim,
    curve_cache = NULL
) {
    key <- pmv__cache_key(
        kind = "curve_level",
        model = model,
        level = level,
        n = n,
        units = units,
        pres = pres,
        tdb = lim$tdb,
        hum = lim$hum
    )
    if (
        !is.null(curve_cache) &&
            exists(key, envir = curve_cache, inherits = FALSE)
    ) {
        return(get(key, envir = curve_cache, inherits = FALSE))
    }

    roots <- pmv__curve_roots(
        model,
        level,
        humratio,
        lim$tdb,
        units,
        pres
    )
    sat_roots <- pmv__curve_saturation_roots(
        model,
        level,
        lim$tdb,
        lim$hum,
        units,
        pres,
        n
    )
    roots <- pmv__merge_roots(roots, sat_roots)
    if (!is.null(curve_cache)) {
        assign(key, roots, envir = curve_cache)
    }
    roots
}

# Return an empty untransformed PMV curve data frame.
pmv__empty_curve_base <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        level = numeric(),
        value = numeric(),
        group = integer(),
        linetype = character(),
        metric = character()
    ))
}

# Keep only integer PMV levels with named sensation labels.
pmv__sensation_levels <- function(levels) {
    levels[!is.na(vapply(levels, pmv__sensation_label, character(1L)))]
}

# Build plot-ready labels placed along PMV curves near the chart axis.
pmv__axis_label_data <- function(
    model,
    levels,
    n,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    axis_label_hjust = ggplot2::waiver(),
    curve_cache = NULL,
    psychro_scales = NULL
) {
    levels <- comfort_check_breaks(levels, "`levels`", n_min = 1L)
    n <- pmv__curve_n(n)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    hum_lim_narrow <- unit__hum_from_chart(lim$hum, units)
    label_start <- hum_lim_narrow[[1L]] +
        diff(hum_lim_narrow) * pmv__axis_label_offset(axis_label_hjust)
    label_end <- hum_lim_narrow[[1L]] +
        diff(hum_lim_narrow) * pmv__axis_label_end(axis_label_hjust)
    curves <- pmv__curve_base_data(
        model,
        levels,
        n,
        units,
        pres,
        tdb_lim,
        hum_lim,
        curve_cache = curve_cache
    )
    if (!nrow(curves)) {
        return(pmv__empty_curve())
    }

    labels <- vector("list", length(levels))
    for (i in seq_along(levels)) {
        curve <- curves[curves$level == levels[[i]], , drop = FALSE]
        curve <- curve[order(curve$humratio, curve$tdb), , drop = FALSE]
        segment <- pmv__axis_label_segment(
            curve,
            label_start,
            label_end
        )
        if (is.null(segment)) {
            next
        }
        labels[[i]] <- util__new_data_frame(list(
            tdb = segment$tdb,
            humratio = segment$humratio,
            level = levels[[i]],
            value = levels[[i]],
            group = i,
            label = pmv__format_level(levels[[i]]),
            hjust = pmv__axis_label_text_hjust(axis_label_hjust),
            vjust = 0.5,
            metric = "pmv"
        ))
    }
    labels <- labels[!vapply(labels, is.null, logical(1L))]
    if (!length(labels)) {
        return(pmv__empty_curve())
    }
    out <- do.call(rbind, labels)
    row.names(out) <- NULL
    out <- pmv__reverse_groups(out)
    psychro_output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}

# Extract the PMV curve segment used for an axis-side label.
pmv__axis_label_segment <- function(curve, label_start, label_end) {
    if (
        nrow(curve) < 2L ||
            !is.finite(label_start) ||
            !is.finite(label_end) ||
            label_end <= label_start
    ) {
        return(NULL)
    }

    curve <- curve[order(curve$humratio, curve$tdb), , drop = FALSE]
    if (
        label_start < min(curve$humratio) || label_start > max(curve$humratio)
    ) {
        return(NULL)
    }

    label_end <- min(label_end, max(curve$humratio))
    if (label_end <= label_start) {
        return(NULL)
    }

    humratio <- unique(c(
        label_start,
        curve$humratio[
            curve$humratio > label_start &
                curve$humratio < label_end
        ],
        label_end
    ))
    tdb <- stats::approx(
        curve$humratio,
        curve$tdb,
        xout = humratio,
        rule = 2,
        ties = "ordered"
    )$y
    keep <- is.finite(tdb) & is.finite(humratio)
    if (sum(keep) < 2L) {
        return(NULL)
    }

    list(tdb = tdb[keep], humratio = humratio[keep])
}

# Resolve horizontal justification for PMV axis-side labels.
pmv__axis_label_text_hjust <- function(axis_label_hjust) {
    if (util__is_waive(axis_label_hjust)) {
        return(0.95)
    }
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(1 - max(0, min(0.2, axis_label_hjust[[1L]])))
    }
    0.95
}

# Resolve vertical justification for PMV axis-side labels.
pmv__axis_label_text_vjust <- function(axis_label_vjust, size = NULL) {
    if (util__is_waive(axis_label_vjust)) {
        size <- if (is.null(size)) 2.8 else as.numeric(size)[[1L]]
        offset <- max(3.5, size * ggplot2::.pt * 0.42)
        return(grid::unit(offset, "pt"))
    }
    if (grid::is.unit(axis_label_vjust)) {
        return(axis_label_vjust)
    }
    if (is.numeric(axis_label_vjust) && length(axis_label_vjust)) {
        return(axis_label_vjust[[1L]])
    }
    0.5
}

# Convert axis label hjust into the start offset along humidity ratio.
pmv__axis_label_offset <- function(axis_label_hjust) {
    if (util__is_waive(axis_label_hjust)) {
        return(0.025)
    }
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(max(0, min(0.08, axis_label_hjust[[1L]])))
    }
    0.025
}

# Convert axis label hjust into the end offset along humidity ratio.
pmv__axis_label_end <- function(axis_label_hjust) {
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(min(0.16, max(0, axis_label_hjust[[1L]]) + 0.055))
    }
    0.07
}

# Reverse each PMV group while preserving group membership.
pmv__reverse_groups <- function(data) {
    pieces <- lapply(split(data, data$group), function(x) {
        x[rev(seq_len(nrow(x))), , drop = FALSE]
    })
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
    out
}

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
    metric <- comfort_model_metric(model, metric)
    if (comfort_model_type(model) != "pmv" || metric != "pmv") {
        stop(
            "Root-traced comfort overlay bands are only available for PMV.",
            call. = FALSE
        )
    }

    model <- pmv__curve_model(model)
    breaks <- pmv__root_band_breaks(levels)
    n <- comfort_grid_n(n)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
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
        out <- comfort_empty_band()
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
        out <- comfort_empty_band()
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
        out <- comfort_empty_band()
        if (!is.null(rootband_cache)) {
            assign(key, out, envir = rootband_cache)
        }
        return(out)
    }

    out <- do.call(rbind, polys)
    row.names(out) <- NULL
    out <- psychro_output_xy(
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

# Return an empty transformed PMV curve data frame.
pmv__empty_curve <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        x = numeric(),
        y = numeric(),
        level = numeric(),
        value = numeric(),
        group = integer(),
        linetype = character(),
        label = character(),
        hjust = numeric(),
        vjust = numeric(),
        metric = character()
    ))
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
    range <- comfort_check_breaks(range, "`range`", n_min = 2L)
    if (length(range) != 2L) {
        stop("`range` must contain exactly two PMV boundaries.", call. = FALSE)
    }
    # When a caller supplies a wider break set, compute the full rootband once
    # and filter back to this visible band. Plain geom_comfort_zone() leaves it
    # NULL so its historical per-range behavior is unchanged.
    levels <- if (is.null(rootband_levels)) {
        range
    } else {
        comfort_check_breaks(
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
        return(comfort_empty_band())
    }

    keep <- is.finite(bands$level_low) &
        is.finite(bands$level_high) &
        abs(bands$level_low - range[[1L]]) <= 1e-8 &
        abs(bands$level_high - range[[2L]]) <= 1e-8
    out <- bands[keep, , drop = FALSE]
    if (!nrow(out)) {
        return(comfort_empty_band())
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
        n <- as.integer(levels[[1L]])
        if (!is.finite(n) || n < 1L) {
            stop(
                "`levels` must be a positive band count or numeric breaks.",
                call. = FALSE
            )
        }
        return(seq(-3, 3, length.out = n + 1L))
    }
    comfort_check_breaks(levels, "`levels`", n_min = 2L)
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
    sat <- psychro_saturation_humratio(tdb_lim, units, pres)
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

# Convert sparse PMV roots into a vector aligned with the band sampling grid.
pmv__curve_root_vector <- function(
    model,
    level,
    humratio,
    tdb_lim,
    units,
    pres
) {
    out <- rep(NA_real_, length(humratio))
    roots <- pmv__curve_roots(
        model,
        level,
        humratio,
        tdb_lim,
        units,
        pres
    )
    if (!length(roots$tdb)) {
        return(out)
    }
    key <- match(round(roots$humratio, 12L), round(humratio, 12L))
    ok <- !is.na(key)
    out[key[ok]] <- roots$tdb[ok]
    out
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

# Validate that a model can be used for root-traced PMV curves.
pmv__curve_model <- function(model) {
    comfort_check_model(model)
    if (model$type != "pmv") {
        stop(
            "Root-traced PMV curves require `comfort_model_pmv()`.",
            call. = FALSE
        )
    }
    model$params$round_output <- FALSE
    model
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

# Trace PMV roots at fixed humidity ratios, falling back to R when needed.
pmv__curve_roots <- function(
    model,
    level,
    humratio,
    tdb_lim,
    units,
    pres
) {
    roots <- pmv__native_curve_roots(
        model,
        level,
        humratio,
        tdb_lim,
        units,
        pres
    )
    if (!is.null(roots)) {
        return(roots)
    }
    pmv__curve_roots_r(model, level, humratio, tdb_lim, units, pres)
}

# Trace PMV roots at fixed humidity ratios with the R fallback implementation.
pmv__curve_roots_r <- function(
    model,
    level,
    humratio,
    tdb_lim,
    units,
    pres
) {
    # At a fixed humidity ratio, valid dry-bulb temperatures start at the
    # dew-point line and end at the chart limit; roots are bracketed there.
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
    saturation_hi <- psychro_saturation_humratio(xhi, units, pres)
    valid <- is.finite(xlo) &
        is.finite(xhi) &
        xlo < xhi &
        humratio >= 0 &
        humratio <= saturation_hi
    if (!any(valid)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    lo <- xlo[valid]
    hi <- xhi[valid]
    hum <- humratio[valid]
    flo <- pmv__value_at(model, lo, hum, units, pres) - level
    fhi <- pmv__value_at(model, hi, hum, units, pres) - level
    bracket <- is.finite(flo) & is.finite(fhi) & flo * fhi <= 0
    if (!any(bracket)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    lo <- lo[bracket]
    hi <- hi[bracket]
    hum <- hum[bracket]
    flo <- flo[bracket]
    fhi <- fhi[bracket]

    exact_lo <- abs(flo) < 1e-8
    exact_hi <- abs(fhi) < 1e-8
    solved <- exact_lo | exact_hi
    root <- numeric(length(hum))
    root[exact_lo] <- lo[exact_lo]
    root[!exact_lo & exact_hi] <- hi[!exact_lo & exact_hi]

    active <- !solved
    # A fixed 44-step bisection is enough to reach double-precision chart
    # accuracy without depending on optimizers that allocate per row.
    for (i in seq_len(44L)) {
        if (!any(active)) {
            break
        }
        mid <- (lo[active] + hi[active]) / 2
        fmid <- pmv__value_at(model, mid, hum[active], units, pres) -
            level
        same <- sign(fmid) == sign(flo[active])
        same[!is.finite(same)] <- FALSE
        idx <- which(active)
        lo[idx[same]] <- mid[same]
        flo[idx[same]] <- fmid[same]
        hi[idx[!same]] <- mid[!same]
        fhi[idx[!same]] <- fmid[!same]
    }
    root[active] <- (lo[active] + hi[active]) / 2

    finite <- is.finite(root)
    list(tdb = root[finite], humratio = hum[finite])
}

# Trace PMV roots along saturation, falling back to R when needed.
pmv__curve_saturation_roots <- function(
    model,
    level,
    tdb_lim,
    hum_lim,
    units,
    pres,
    n
) {
    roots <- pmv__native_saturation_roots(
        model,
        level,
        tdb_lim,
        hum_lim,
        units,
        pres,
        n
    )
    if (!is.null(roots)) {
        return(roots)
    }
    pmv__curve_saturation_roots_r(
        model,
        level,
        tdb_lim,
        hum_lim,
        units,
        pres,
        n
    )
}

# Trace PMV roots along saturation with the R fallback implementation.
pmv__curve_saturation_roots_r <- function(
    model,
    level,
    tdb_lim,
    hum_lim,
    units,
    pres,
    n
) {
    # Trace roots along the saturation curve so PMV contours and filled bands
    # can close against the psychrometric chart boundary instead of stopping.
    n <- max(as.integer(n), 80L)
    tdb <- seq(tdb_lim[[1L]], tdb_lim[[2L]], length.out = n)
    hum <- psychro_saturation_humratio(tdb, units, pres)
    hum_lim <- unit__hum_from_chart(hum_lim, units)
    valid <- is.finite(tdb) &
        is.finite(hum) &
        hum >= hum_lim[[1L]] &
        hum <= hum_lim[[2L]]
    if (!any(valid)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    value <- pmv__value_at(model, tdb, hum, units, pres) - level
    valid <- valid & is.finite(value)
    if (!any(valid)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    roots <- tdb[valid & abs(value) < 1e-8]
    segment <- which(
        valid[-length(valid)] &
            valid[-1L] &
            value[-length(value)] * value[-1L] < 0
    )
    if (length(segment)) {
        roots <- c(
            roots,
            vapply(
                segment,
                function(i) {
                    lo <- tdb[[i]]
                    hi <- tdb[[i + 1L]]
                    flo <- value[[i]]
                    for (j in seq_len(44L)) {
                        mid <- (lo + hi) / 2
                        fmid <- pmv__value_at(
                            model,
                            mid,
                            psychro_saturation_humratio(mid, units, pres),
                            units,
                            pres
                        ) -
                            level
                        if (!is.finite(fmid)) {
                            break
                        }
                        if (sign(fmid) == sign(flo)) {
                            lo <- mid
                            flo <- fmid
                        } else {
                            hi <- mid
                        }
                    }
                    (lo + hi) / 2
                },
                numeric(1L)
            )
        )
    }

    roots <- roots[is.finite(roots)]
    if (!length(roots)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    hum <- psychro_saturation_humratio(roots, units, pres)
    keep <- is.finite(hum) & hum >= hum_lim[[1L]] & hum <= hum_lim[[2L]]
    list(tdb = roots[keep], humratio = hum[keep])
}

# Merge multiple PMV root sets into a sorted de-duplicated root list.
pmv__merge_roots <- function(...) {
    roots <- list(...)
    tdb <- unlist(lapply(roots, `[[`, "tdb"), use.names = FALSE)
    humratio <- unlist(lapply(roots, `[[`, "humratio"), use.names = FALSE)
    keep <- is.finite(tdb) & is.finite(humratio)
    if (!any(keep)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    tdb <- tdb[keep]
    humratio <- humratio[keep]
    ord <- order(humratio, tdb)
    tdb <- tdb[ord]
    humratio <- humratio[ord]
    key <- paste(round(tdb, 8L), round(humratio, 12L), sep = ":")
    keep <- !duplicated(key)
    list(tdb = tdb[keep], humratio = humratio[keep])
}

# Evaluate PMV at fixed dry-bulb and humidity-ratio coordinates.
pmv__value_at <- function(model, tdb, humratio, units, pres) {
    rh <- comfort_relhum_from_humratio(tdb, humratio, units, pres)
    rh <- comfort_clip_grid_rh(rh)
    out <- rep(NA_real_, length(tdb))
    valid <- comfort_valid_grid_rh(rh)
    if (any(valid)) {
        out[valid] <- comfort_metric_value(
            comfort_apply_model(model, tdb[valid], rh[valid], units, pres),
            "pmv"
        )
    }
    out
}

# Return the PMV contour linetype for a level.
pmv__linetype <- function(level) {
    if (abs(level) < 1e-8) "dashed" else "solid"
}

# Build the displayed PMV curve label for a level and label mode.
pmv__curve_label <- function(level, label) {
    switch(
        label,
        none = NA_character_,
        sensation = pmv__sensation_label(level),
        boundary = paste("PMV", pmv__format_level(level)),
        comfort = "COMFORT"
    )
}

# Resolve horizontal justification for PMV curve labels.
pmv__curve_hjust <- function(label, override = NULL) {
    if (!is.null(override)) {
        return(override)
    }
    switch(
        label,
        none = 0.5,
        sensation = 0.52,
        boundary = 0.045,
        comfort = 0.52
    )
}

# Resolve vertical justification for PMV curve labels.
pmv__curve_vjust <- function(
    level,
    label,
    override = NULL,
    mollier = FALSE
) {
    if (!is.null(override)) {
        return(override)
    }
    if (label == "boundary") {
        if (isTRUE(mollier)) {
            return(if (level <= 0) 1.25 else -0.25)
        }
        return(if (level <= 0) -0.25 else 1.25)
    }
    if (label == "comfort") {
        return(0.5)
    }
    if (abs(level) < 1e-8) {
        return(0.5)
    }
    0.5
}

# Return the named thermal sensation label for an integer PMV level.
pmv__sensation_label <- function(level) {
    if (abs(level - round(level)) > 1e-8) {
        return(NA_character_)
    }
    labels <- c(
        "-3" = "COLD",
        "-2" = "COOL",
        "-1" = "SLIGHTLY COOL",
        "0" = "NEUTRAL",
        "1" = "SLIGHTLY WARM",
        "2" = "WARM",
        "3" = "HOT"
    )
    labels[[as.character(as.integer(round(level)))]] %||% NA_character_
}

# Format a PMV level with an explicit sign for positive values.
pmv__format_level <- function(level) {
    ifelse(level > 0, sprintf("+%.1f", level), sprintf("%.1f", level))
}
