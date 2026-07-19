#' @include comfort-grid.R
NULL

# Comfort band helpers convert sampled fields and isoband output into
# psychrometric polygon/path data frames.

# Build filled comfort bands from a sampled metric grid.
comfort_band__data <- function(
    model,
    metric,
    levels,
    n,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    psychro_scales = NULL
) {
    # Filled bands are generated on node grids so isoband can preserve polygon
    # topology across adjacent cells.
    m <- comfort_grid__matrix(
        model,
        metric,
        n,
        units,
        pres,
        tdb_lim,
        hum_lim,
        at = "nodes",
        boundary = "saturation"
    )
    breaks <- comfort_band__breaks(m$metric, m$value, levels, units)
    if (length(breaks) < 2L) {
        return(comfort_band__empty())
    }

    bands <- isoband::isobands(
        x = m$tdb,
        y = m$humratio,
        z = t(m$value),
        levels_low = breaks[-length(breaks)],
        levels_high = breaks[-1L]
    )
    comfort_band__isoband_data(
        bands,
        breaks[-length(breaks)],
        breaks[-1L],
        m$metric,
        mollier,
        geom = "polygon",
        psychro_scales = psychro_scales,
        units = units
    )
}

# Return an empty polygon-band data frame with stable columns.
comfort_band__empty <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        x = numeric(),
        y = numeric(),
        level = character(),
        level_low = numeric(),
        level_high = numeric(),
        level_mid = numeric(),
        value = numeric(),
        group = character(),
        subgroup = integer(),
        metric = character()
    ))
}

# Derive band breakpoints from metric defaults, user levels, and sampled values.
comfort_band__breaks <- function(metric, z, levels = NULL, units = "SI") {
    if (identical(metric, "acceptability") && is.null(levels)) {
        return(c(-0.5, 0.5, 1.5))
    }

    if (!is.null(levels) && length(levels) > 1L) {
        breaks <- sort(unique(as.numeric(levels)))
        return(breaks[is.finite(breaks)])
    }

    if (identical(metric, "heat_index") && is.null(levels)) {
        z_range <- range(z, finite = TRUE)
        if (!all(is.finite(z_range))) {
            return(numeric())
        }
        eps <- max(1, abs(z_range)) * 1e-9
        breaks <- c(
            z_range[[1L]] - eps,
            heat_index__thresholds(units),
            z_range[[2L]] + eps
        )
        breaks <- sort(unique(breaks[
            breaks > z_range[[1L]] - 2 * eps &
                breaks < z_range[[2L]] + 2 * eps
        ]))
        return(breaks)
    }

    n <- if (is.null(levels)) {
        64L
    } else {
        util__check_whole_count(levels[[1L]], "`levels`", min = 1L, len = 1L)
    }

    z_range <- range(z, finite = TRUE)
    if (!all(is.finite(z_range))) {
        return(numeric())
    }
    if (z_range[[1L]] == z_range[[2L]]) {
        z_range <- z_range + c(-0.5, 0.5)
    } else {
        pad <- diff(z_range) * 1e-9
        z_range <- z_range + c(-pad, pad)
    }
    seq(z_range[[1L]], z_range[[2L]], length.out = n + 1L)
}

# Normalize isoband polygon/path output to comfort layer data columns.
comfort_band__isoband_data <- function(
    iso,
    low,
    high,
    metric,
    mollier,
    geom = c("polygon", "path"),
    psychro_scales = NULL,
    units = NULL
) {
    geom <- match.arg(geom)
    lengths <- vapply(iso, function(x) length(x$x), integer(1L))
    if (!any(lengths)) {
        return(
            if (geom == "polygon") {
                comfort_band__empty()
            } else {
                comfort_contour__empty()
            }
        )
    }

    out <- vector("list", length(iso))
    for (i in seq_along(iso)) {
        item <- iso[[i]]
        n <- length(item$x)
        if (!n) {
            next
        }

        if (geom == "polygon") {
            level_low <- low[[i]]
            level_high <- high[[i]]
            level_mid <- (level_low + level_high) / 2
            out[[i]] <- util__new_data_frame(list(
                tdb = item$x,
                humratio = item$y,
                level = sprintf("%s:%s", level_low, level_high),
                level_low = level_low,
                level_high = level_high,
                level_mid = level_mid,
                value = level_mid,
                group = i,
                subgroup = item$id,
                metric = metric
            ))
        } else {
            level <- low[[i]]
            out[[i]] <- util__new_data_frame(list(
                tdb = item$x,
                humratio = item$y,
                level = level,
                group = i * 100000L + item$id,
                metric = metric
            ))
        }
    }
    out <- do.call(rbind, out[!vapply(out, is.null, logical(1L))])
    row.names(out) <- NULL
    state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}
