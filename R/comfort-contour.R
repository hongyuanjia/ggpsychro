#' @include comfort-band.R comfort-pmv.R comfort-dispatch.R
NULL

# Comfort contour helpers draw metric isolines and textpath-ready labels.

# Build contour paths for the requested comfort metric.
comfort_contour__data <- function(
    model,
    metric,
    breaks,
    n,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    label_path = FALSE,
    psychro_scales = NULL
) {
    metric <- comfort_dispatch__model_metric(model, metric)
    # PMV curves are root-traced because grid isolines can miss steep segments
    # near saturation; other metrics keep the cheaper grid/isoband path.
    use_root <- comfort__model_type(model) == "pmv" && metric == "pmv"
    if (use_root) {
        if (is.null(breaks)) {
            breaks <- comfort_contour__breaks("pmv", numeric(), units)
        }
        # Root-traced PMV contours already return curve vertices; the common
        # label code below can treat them like isoband isolines.
        out <- pmv__curve_data(
            model,
            breaks,
            n[[1L]],
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            label = "none",
            psychro_scales = psychro_scales
        )
        out <- comfort_contour__add_labels(out)
        if (isTRUE(label_path)) {
            out <- comfort_contour__orient_label_paths(out)
        }
        return(out)
    }

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
    z <- m$value
    if (is.null(breaks)) {
        breaks <- comfort_contour__breaks(m$metric, z, units)
    }
    breaks <- breaks[is.finite(breaks)]
    if (!length(breaks)) {
        return(comfort_contour__empty())
    }

    lines <- isoband::isolines(
        x = m$tdb,
        y = m$humratio,
        z = t(z),
        levels = breaks
    )
    # Normalize isoband's path representation to the columns expected by
    # ggplot stats and psychrometric coordinate transforms.
    out <- comfort_band__isoband_data(
        lines,
        breaks,
        breaks,
        m$metric,
        mollier,
        geom = "path",
        psychro_scales = psychro_scales,
        units = units
    )
    out <- comfort_contour__add_labels(out)
    if (isTRUE(label_path)) {
        out <- comfort_contour__orient_label_paths(out)
    }
    out
}

# Return an empty contour data frame with stable columns.
comfort_contour__empty <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        x = numeric(),
        y = numeric(),
        level = numeric(),
        value = numeric(),
        group = character(),
        label = character(),
        metric = character()
    ))
}

# Attach display labels derived from contour levels.
comfort_contour__add_labels <- function(data) {
    if (!nrow(data)) {
        return(data)
    }
    data$value <- data$level
    data$label <- comfort_contour__format_level(data$level, data$metric)
    data
}

# Orient contour paths so text labels follow a consistent reading direction.
comfort_contour__orient_label_paths <- function(data) {
    if (!nrow(data) || !"group" %in% names(data)) {
        return(data)
    }

    x_scale <- diff(range(data$x, finite = TRUE))
    y_scale <- diff(range(data$y, finite = TRUE))
    if (!is.finite(x_scale) || x_scale <= 0) {
        x_scale <- 1
    }
    if (!is.finite(y_scale) || y_scale <= 0) {
        y_scale <- 1
    }

    groups <- split(seq_len(nrow(data)), data$group)
    out <- lapply(groups, function(i) {
        group_data <- data[i, , drop = FALSE]
        if (nrow(group_data) < 2L) {
            return(group_data)
        }

        dx <- (group_data$x[[nrow(group_data)]] - group_data$x[[1L]]) / x_scale
        dy <- (group_data$y[[nrow(group_data)]] - group_data$y[[1L]]) / y_scale
        if (!is.finite(dx)) {
            dx <- 0
        }
        if (!is.finite(dy)) {
            dy <- 0
        }

        reverse <- if (abs(dy) >= abs(dx)) dy < 0 else dx < 0
        if (isTRUE(reverse)) {
            group_data <- group_data[
                rev(seq_len(nrow(group_data))),
                ,
                drop = FALSE
            ]
        }
        group_data
    })

    out <- do.call(rbind, out)
    row.names(out) <- NULL
    out
}

# Format contour levels with metric-specific conventions.
comfort_contour__format_level <- function(level, metric) {
    metric <- rep(metric, length.out = length(level))
    out <- scales::number(level, accuracy = NULL, trim = TRUE)
    pmv <- metric == "pmv"
    out[pmv] <- pmv__format_level(level[pmv])
    out
}

# Merge contour label aesthetics into a user-supplied mapping.
comfort_contour__label_mapping <- function(mapping) {
    label <- NULL
    out <- mapping %||% ggplot2::aes()
    label_mapping <- ggplot2::aes(label = ggplot2::after_stat(label))
    out$label <- label_mapping$label
    out
}

# Fill textpath defaults for labelled comfort contours.
comfort_contour__label_params <- function(params, label_size = NULL) {
    params$size <- label_size %||% params$size %||% 2.8
    params$text_only <- FALSE
    params$upright <- FALSE
    params$remove_long <- TRUE
    params$gap <- TRUE
    params$padding <- params$padding %||% grid::unit(1, "pt")
    params
}

# Choose default contour breaks for PMV, heat index, or sampled metrics.
comfort_contour__breaks <- function(metric, z, units = "SI") {
    if (metric == "pmv") {
        return(seq(-3, 3, by = 0.5))
    }
    if (metric == "heat_index") {
        return(heat_index__thresholds(units))
    }
    z <- z[is.finite(z)]
    if (!length(z)) {
        return(numeric())
    }
    z_range <- range(z)
    pretty(z_range, n = 8)
}
