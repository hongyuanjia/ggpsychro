#' @include comfort-core.R
NULL

# Heat-index model helpers and derived overlay geometry. These functions stay
# isolated from PMV/SET because their thresholds and categories are discrete.
comfort_heat_index_f <- function(tdb_f, rh, solar_exposure) {
    hi <- tdb_f
    valid <- is.finite(tdb_f) & is.finite(rh) & is.finite(solar_exposure)
    warm <- valid & tdb_f > 40
    if (any(warm)) {
        # NOAA heat index starts with the simple Steadman regression and only
        # switches to the Rothfusz regression when the apparent heat is high.
        simple <- 0.5 * (
            tdb_f[warm] + (61 + 1.2 * (tdb_f[warm] - 68) + 0.094 * rh[warm])
        )
        idx <- which(warm)
        hi[idx] <- simple

        roth <- idx[simple > 79]
        if (length(roth)) {
            t <- tdb_f[roth]
            r <- rh[roth]
            # Rothfusz regression is defined in degrees F and relative humidity
            # percent; callers convert SI temperatures before reaching here.
            value <- 2.04901523 * t - 42.379 + 10.14333127 * r -
                0.22475541 * t * r - 0.00683783 * t^2 -
                0.05481717 * r^2 + 0.00122874 * t^2 * r +
                0.00085282 * t * r^2 - 0.00000199 * t^2 * r^2

            # NOAA applies small empirical corrections for very dry hot air and
            # very humid warm air after the base Rothfusz regression.
            low_rh <- r <= 13 & t >= 80 & t <= 112
            if (any(low_rh)) {
                value[low_rh] <- value[low_rh] -
                    (13 - r[low_rh]) / 4 *
                    sqrt((17 - abs(t[low_rh] - 95)) / 17)
            }
            high_rh <- r > 85 & t >= 80 & t <= 87
            if (any(high_rh)) {
                value[high_rh] <- value[high_rh] +
                    (r[high_rh] - 85) / 10 * ((87 - t[high_rh]) / 5)
            }
            hi[roth] <- value
        }
    }

    # Marsh's solar-exposure adjustment is a post-regression offset from shaded
    # to full sun, scaled by a validated 0..1 exposure fraction.
    hi[valid] <- hi[valid] + 8 * solar_exposure[valid]
    hi[!valid] <- NA_real_
    hi
}

comfort_heat_index_thresholds <- function(units) {
    units <- match.arg(units, c("SI", "IP"))
    if (units == "IP") {
        c(80, 90, 103, 125)
    } else {
        get_c_from_f(c(80, 90, 103, 125))
    }
}

comfort_heat_index_category <- function(heat_index_f) {
    labels <- c(
        "none", "caution", "extreme caution", "danger", "extreme danger"
    )
    id <- findInterval(heat_index_f, c(80, 90, 103, 125))
    id[!is.finite(heat_index_f)] <- NA_integer_
    category <- labels[id + 1L]
    category[is.na(id)] <- NA_character_
    new_data_frame(list(category = category, category_id = id))
}

comfort_heat_index_zone_specs <- function() {
    list(
        list(id = 1L, label = "CAUTION", fill = "#FFE66D"),
        list(id = 2L, label = "EXTREME CAUTION", fill = "#FFB347"),
        list(id = 3L, label = "DANGER", fill = "#FF7043"),
        list(id = 4L, label = "EXTREME DANGER", fill = "#D73027")
    )
}

comfort_heat_index_zone_data <- function(model, category_id = NULL, n, units, pres,
                                         mollier, tdb_lim, hum_lim,
                                         grid_cache = NULL) {
    thresholds <- comfort_heat_index_thresholds(units)
    category_ids <- if (is.null(category_id)) {
        seq_along(thresholds)
    } else {
        as.integer(category_id)
    }
    category_ids <- category_ids[category_ids %in% seq_along(thresholds)]
    if (!length(category_ids)) {
        return(comfort_empty_band())
    }

    m <- comfort_heat_index_grid_matrix(
        model, n, units, pres, tdb_lim, hum_lim, grid_cache = grid_cache
    )
    z_range <- range(m$value, finite = TRUE)
    if (!all(is.finite(z_range))) {
        return(comfort_empty_band())
    }

    specs <- comfort_heat_index_zone_specs()
    zones <- lapply(category_ids, function(id) {
        comfort_heat_index_zone_from_grid(
            m, id, thresholds, specs, z_range, mollier
        )
    })
    zones <- zones[vapply(zones, nrow, integer(1L)) > 0L]
    if (!length(zones)) {
        return(comfort_empty_band())
    }

    group_offset <- 0L
    for (i in seq_along(zones)) {
        zones[[i]]$group <- zones[[i]]$group + group_offset
        group_offset <- max(zones[[i]]$group, group_offset)
    }
    out <- do.call(rbind, zones)
    row.names(out) <- NULL
    out
}

comfort_heat_index_grid_matrix <- function(model, n, units, pres, tdb_lim,
                                           hum_lim, grid_cache = NULL) {
    key <- comfort_heat_index_grid_key(model, n, units, pres, tdb_lim, hum_lim)
    if (!is.null(grid_cache) && exists(key, envir = grid_cache, inherits = FALSE)) {
        return(get(key, envir = grid_cache, inherits = FALSE))
    }

    # Heat-index zone layers all need the same node grid; the caller supplies a
    # short-lived environment so reuse stays local to one layer composition.
    m <- comfort_grid_matrix(
        model, "heat_index", n, units, pres, tdb_lim, hum_lim,
        at = "nodes", boundary = "saturation"
    )
    if (!is.null(grid_cache)) {
        assign(key, m, envir = grid_cache)
    }
    m
}

comfort_heat_index_grid_key <- function(model, n, units, pres, tdb_lim, hum_lim) {
    paste(
        utils::capture.output(utils::str(list(
            model = model, n = n, units = units, pres = pres,
            tdb_lim = tdb_lim, hum_lim = hum_lim
        ), give.attr = FALSE)),
        collapse = "\n"
    )
}

comfort_heat_index_zone_from_grid <- function(m, category_id, thresholds, specs,
                                              z_range, mollier) {
    low <- thresholds[[category_id]]
    high <- if (category_id < length(thresholds)) {
        thresholds[[category_id + 1L]]
    } else {
        max(z_range[[2L]], low) + max(1, abs(low)) * 1e-6
    }
    if (high <= low || z_range[[2L]] < low) {
        return(comfort_empty_band())
    }
    high <- min(high, z_range[[2L]] + max(1, abs(z_range[[2L]])) * 1e-6)

    bands <- isoband::isobands(
        x = m$tdb, y = m$humratio, z = t(m$value),
        levels_low = low, levels_high = high
    )
    out <- comfort_isoband_data(
        bands, low, high, m$metric, mollier, geom = "polygon"
    )
    if (nrow(out)) {
        out$category_id <- category_id
        out$category <- specs[[category_id]]$label
        out$fill <- specs[[category_id]]$fill
    }
    out
}

comfort_heat_index_label_data <- function(model, n, units, pres, mollier,
                                          tdb_lim, hum_lim) {
    m <- comfort_grid_matrix(
        model, "heat_index", n, units, pres, tdb_lim, hum_lim,
        at = "centers", boundary = "saturation"
    )
    specs <- comfort_heat_index_zone_specs()
    thresholds <- comfort_heat_index_thresholds(units)
    labels <- vector("list", length(specs))
    values <- as.vector(m$value)
    grid <- expand.grid(tdb = m$tdb, humratio = m$humratio)
    for (i in seq_along(specs)) {
        low <- thresholds[[i]]
        high <- if (i < length(thresholds)) thresholds[[i + 1L]] else Inf
        # Place labels near the category midpoint using the sampled cell whose
        # computed heat index is closest to that target.
        target <- if (is.finite(high)) (low + high) / 2 else low + 4
        keep <- is.finite(values) & values >= low & values < high
        if (!any(keep)) {
            next
        }
        idx <- which(keep)[which.min(abs(values[keep] - target))]
        labels[[i]] <- new_data_frame(list(
            tdb = grid$tdb[[idx]],
            humratio = grid$humratio[[idx]],
            label = specs[[i]]$label,
            category_id = specs[[i]]$id,
            category = specs[[i]]$label,
            angle = 0,
            group = i
        ))
    }
    labels <- labels[!vapply(labels, is.null, logical(1L))]
    if (!length(labels)) {
        return(new_data_frame(list(
            tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
            label = character(), category_id = integer(), category = character(),
            angle = numeric(), group = integer()
        )))
    }
    out <- do.call(rbind, labels)
    row.names(out) <- NULL
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}
