#' @include comfort-core.R comfort-grid.R comfort-dispatch.R comfort-native.R comfort-pmv-label.R
NULL

# PMV curve helpers trace contour roots and convert them into plot-ready data.

# Validate the scalar sampling count used by PMV root-traced curves.
pmv__curve_n <- function(n) {
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 8) {
        stop(
            "`n` must be a single finite number greater than or equal to 8.",
            call. = FALSE
        )
    }
    as.integer(n)
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
    levels <- comfort__check_breaks(levels, "`levels`", n_min = 1L)
    n <- pmv__curve_n(n)
    model <- pmv__curve_model(model)
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
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
    levels <- comfort__check_breaks(levels, "`levels`", n_min = 1L)
    n <- pmv__curve_n(n)
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
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

# Reverse each PMV group while preserving group membership.
pmv__reverse_groups <- function(data) {
    pieces <- lapply(split(data, data$group), function(x) {
        x[rev(seq_len(nrow(x))), , drop = FALSE]
    })
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
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

# Validate that a model can be used for root-traced PMV curves.
pmv__curve_model <- function(model) {
    comfort__check_model(model)
    if (model$type != "pmv") {
        stop(
            "Root-traced PMV curves require `comfort_model_pmv()`.",
            call. = FALSE
        )
    }
    model$params$round_output <- FALSE
    model
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
            "pmv"
        )
    }
    out
}
