#' @include psychro-state.R
NULL

#' Draw psychrometric zones
#'
#' `stat_psychro_zone()` samples psychrometric boundary curves and returns
#' polygon coordinates. `geom_psychro_zone()` draws those polygons.
#'
#' @details
#' Supported zone types are:
#'
#' * `"dbt-rh"`: `tdb_min`, `tdb_max`, `relhum_min`, and `relhum_max`
#' * `"enthalpy-rh"`: `enthalpy_min`, `enthalpy_max`, `relhum_min`, and `relhum_max`
#' * `"specvol-rh"` or `"volume-rh"`: `specvol_min`, `specvol_max`,
#'   `relhum_min`, and `relhum_max`
#' * `"dbt-wmax"`: `tdb_min`, `tdb_max`, `humratio_max`, and optional
#'   `humratio_min`
#' * `"xy-points"`: `tdb`, `humratio`, and optional `group`
#'
#' `relhum` values are supplied in percent. `humratio` values are supplied in
#' chart display units: g/kg in SI and gr/lb in IP.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param type A zone type.
#' @param n Number of samples used for computed zone boundaries.
#'
#' @rdname psychro_zone
#' @examples
#' comfort <- data.frame(
#'     name = c("cool", "warm"),
#'     tdb_min = c(20, 24),
#'     tdb_max = c(26, 32),
#'     relhum_min = c(35, 45),
#'     relhum_max = c(60, 75)
#' )
#'
#' ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
#'     geom_psychro_zone(
#'         aes(tdb_min = tdb_min, tdb_max = tdb_max,
#'             relhum_min = relhum_min, relhum_max = relhum_max,
#'             fill = name),
#'         data = comfort,
#'         type = "dbt-rh",
#'         alpha = 0.28,
#'         colour = NA
#'     )
#'
#' # Dry-bulb range with humidity-ratio limits.
#' humidity_cap <- data.frame(
#'     tdb_min = 10,
#'     tdb_max = 34,
#'     humratio_min = 4,
#'     humratio_max = 12
#' )
#' ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
#'     geom_psychro_zone(
#'         aes(tdb_min = tdb_min, tdb_max = tdb_max,
#'             humratio_min = humratio_min, humratio_max = humratio_max),
#'         data = humidity_cap,
#'         type = "dbt-wmax",
#'         alpha = 0.25
#'     )
#'
#' # Property-bounded zones.
#' enthalpy_zone <- data.frame(
#'     enthalpy_min = 40000,
#'     enthalpy_max = 65000,
#'     relhum_min = 30,
#'     relhum_max = 80
#' )
#' ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
#'     geom_psychro_zone(
#'         aes(enthalpy_min = enthalpy_min, enthalpy_max = enthalpy_max,
#'             relhum_min = relhum_min, relhum_max = relhum_max),
#'         data = enthalpy_zone,
#'         type = "enthalpy-rh",
#'         alpha = 0.25
#'     )
#'
#' # Draw an already-specified polygon in chart display units.
#' polygon_zone <- data.frame(
#'     tdb = c(20, 26, 28),
#'     humratio = c(6, 8, 6)
#' )
#' ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
#'     geom_psychro_zone(
#'         aes(tdb = tdb, humratio = humratio),
#'         data = polygon_zone,
#'         type = "xy-points",
#'         alpha = 0.25
#'     )
#' @export
geom_psychro_zone <- function(mapping = NULL, data = NULL, stat = "psychro_zone",
                              position = "identity", ..., type = "dbt-rh",
                              n = 100L, na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
    psychro_layer(
        stat = stat, data = data, mapping = mapping, geom = "polygon",
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(type = type, n = n, na.rm = na.rm, ...)
    )
}

#' @rdname psychro_zone
#' @export
stat_psychro_zone <- function(mapping = NULL, data = NULL, geom = "polygon",
                              position = "identity", ..., type = "dbt-rh",
                              n = 100L, na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
    psychro_layer(
        stat = StatPsychroZone, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(type = type, n = n, na.rm = na.rm, ...)
    )
}

psychro_zone_types <- function() {
    c("dbt-rh", "enthalpy-rh", "specvol-rh", "volume-rh", "dbt-wmax", "xy-points")
}

standardize_psychro_zone_type <- function(type) {
    type <- match.arg(type, psychro_zone_types())
    if (identical(type, "volume-rh")) "specvol-rh" else type
}

psychro_required_columns <- function(data, columns) {
    missing <- setdiff(columns, names(data))
    if (length(missing)) {
        stop("Missing required aesthetics: ", paste(missing, collapse = ", "),
            call. = FALSE)
    }
}

psychro_tdb_sequence <- function(tdb_min, tdb_max, n) {
    if (tdb_min > tdb_max) {
        stop("`tdb_min` must be less than or equal to `tdb_max`.", call. = FALSE)
    }
    seq(tdb_min, tdb_max, length.out = n)
}

psychro_zone_chart_tdb_range <- function(units, tdb_lim) {
    if (!is.null(tdb_lim)) {
        return(tdb_lim)
    }
    default_psychro_limits(units)$tdb
}

psychro_zone_complete <- function(row, columns, na.rm = FALSE) {
    row <- psychro_check_finite(row, columns, na.rm = na.rm)
    nrow(row) == 1L
}

psychro_zone_relhum <- function(min, max) {
    psychro_check_relhum_percent(c(min, max))
    if (min > max) {
        stop("`relhum_min` must be less than or equal to `relhum_max`.",
            call. = FALSE)
    }
    c(min, max) / 100
}

psychro_saturation_humratio <- function(tdb, units, pres) {
    with_units(units, psychrolib::GetHumRatioFromRelHum(tdb, 1, pres))
}

psychro_curve_humratio <- function(tdb, value, type, units, pres) {
    with_units(units, with_no_hum_limit(switch(type,
        relhum = psychrolib::GetHumRatioFromRelHum(tdb, value, pres),
        enthalpy = GetHumRatioFromEnthalpyAndTDryBulb(value, tdb),
        specvol = GetHumRatioFromMoistAirVolumeAndTDryBulb(value, tdb, pres),
        stop("Invalid psychrometric zone curve type.", call. = FALSE)
    )))
}

psychro_zone_polygon_data <- function(row, tdb, lower, upper, mollier, group) {
    keep <- is.finite(tdb) & is.finite(lower) & is.finite(upper) & lower <= upper
    tdb <- tdb[keep]
    lower <- lower[keep]
    upper <- upper[keep]

    if (length(tdb) < 2L) {
        stop("Psychrometric zone does not contain a drawable area.",
            call. = FALSE)
    }

    out <- row[rep(1L, length(tdb) * 2L), , drop = FALSE]
    out$group <- group
    out <- psychro_output_xy(
        out,
        c(tdb, rev(tdb)),
        c(lower, rev(upper)),
        mollier
    )
    row.names(out) <- NULL
    out
}

psychro_zone_dbt_rh <- function(row, units, pres, mollier, n, group, na.rm) {
    cols <- c("tdb_min", "tdb_max", "relhum_min", "relhum_max")
    if (!psychro_zone_complete(row, cols, na.rm)) return(row[0, , drop = FALSE])

    rh <- psychro_zone_relhum(row$relhum_min, row$relhum_max)
    tdb <- psychro_tdb_sequence(row$tdb_min, row$tdb_max, n)
    lower <- psychro_curve_humratio(tdb, rh[[1L]], "relhum", units, pres)
    upper <- psychro_curve_humratio(tdb, rh[[2L]], "relhum", units, pres)
    upper <- pmin(upper, psychro_saturation_humratio(tdb, units, pres))
    lower <- pmax(lower, 0)

    psychro_zone_polygon_data(row, tdb, lower, upper, mollier, group)
}

psychro_zone_property_rh <- function(row, property, units, pres, mollier, n,
                                     group, na.rm, tdb_lim) {
    min_col <- paste0(property, "_min")
    max_col <- paste0(property, "_max")
    cols <- c(min_col, max_col, "relhum_min", "relhum_max")
    if (!psychro_zone_complete(row, cols, na.rm)) return(row[0, , drop = FALSE])

    if (row[[min_col]] > row[[max_col]]) {
        stop("`", min_col, "` must be less than or equal to `", max_col, "`.",
            call. = FALSE)
    }

    rh <- psychro_zone_relhum(row$relhum_min, row$relhum_max)
    tdb_range <- psychro_zone_chart_tdb_range(units, tdb_lim)
    tdb <- psychro_tdb_sequence(tdb_range[[1L]], tdb_range[[2L]], n)
    prop_low <- psychro_curve_humratio(tdb, row[[min_col]], property, units, pres)
    prop_high <- psychro_curve_humratio(tdb, row[[max_col]], property, units, pres)
    rh_low <- psychro_curve_humratio(tdb, rh[[1L]], "relhum", units, pres)
    rh_high <- psychro_curve_humratio(tdb, rh[[2L]], "relhum", units, pres)
    saturation <- psychro_saturation_humratio(tdb, units, pres)

    lower <- pmax(pmin(prop_low, prop_high), rh_low, 0)
    upper <- pmin(pmax(prop_low, prop_high), rh_high, saturation)

    psychro_zone_polygon_data(row, tdb, lower, upper, mollier, group)
}

psychro_zone_dbt_wmax <- function(row, units, pres, mollier, n, group, na.rm) {
    cols <- c("tdb_min", "tdb_max", "humratio_max")
    if (!"humratio_min" %in% names(row)) {
        row$humratio_min <- 0
    }
    if (!psychro_zone_complete(row, c(cols, "humratio_min"), na.rm)) {
        return(row[0, , drop = FALSE])
    }
    if (row$humratio_min > row$humratio_max) {
        stop("`humratio_min` must be less than or equal to `humratio_max`.",
            call. = FALSE)
    }

    tdb <- psychro_tdb_sequence(row$tdb_min, row$tdb_max, n)
    lower <- rep(narrow_hum(row$humratio_min, units), length(tdb))
    upper <- rep(narrow_hum(row$humratio_max, units), length(tdb))
    upper <- pmin(upper, psychro_saturation_humratio(tdb, units, pres))
    lower <- pmax(lower, 0)

    psychro_zone_polygon_data(row, tdb, lower, upper, mollier, group)
}

psychro_close_xy_group <- function(data) {
    if (nrow(data) < 3L) {
        stop("`xy-points` zones require at least three points per group.",
            call. = FALSE)
    }

    first <- data[1L, , drop = FALSE]
    last <- data[nrow(data), , drop = FALSE]
    closed <- isTRUE(all.equal(first$tdb, last$tdb, tolerance = 1e-8)) &&
        isTRUE(all.equal(first$humratio, last$humratio, tolerance = 1e-8))

    if (!closed) {
        data <- rbind(data, first)
    }
    data
}

psychro_zone_xy_points <- function(data, units, mollier, na.rm) {
    cols <- c("tdb", "humratio")
    psychro_required_columns(data, cols)
    data <- psychro_check_finite(data, cols, na.rm = na.rm)
    if (!nrow(data)) return(data)

    if (!"group" %in% names(data)) {
        data$group <- 1L
    }

    groups <- split(data, data$group)
    out <- lapply(groups, function(group_data) {
        group_data <- psychro_close_xy_group(group_data)
        humratio <- narrow_hum(group_data$humratio, units)
        psychro_output_xy(group_data, group_data$tdb, humratio, mollier)
    })

    do.call(rbind, out)
}

psychro_compute_zone <- function(data, type, n, units, pres, mollier, tdb_lim,
                                 na.rm = FALSE) {
    type <- standardize_psychro_zone_type(type)
    n <- as.integer(n)
    if (length(n) != 1L || is.na(n) || n < 2L) {
        stop("`n` must be a single integer greater than or equal to 2.",
            call. = FALSE)
    }

    if (identical(type, "xy-points")) {
        return(psychro_zone_xy_points(data, units, mollier, na.rm))
    }

    zones <- lapply(seq_len(nrow(data)), function(i) {
        row <- data[i, , drop = FALSE]
        group <- i
        switch(type,
            `dbt-rh` = {
                psychro_required_columns(row, c("tdb_min", "tdb_max", "relhum_min", "relhum_max"))
                psychro_zone_dbt_rh(row, units, pres, mollier, n, group, na.rm)
            },
            `enthalpy-rh` = {
                psychro_required_columns(row, c("enthalpy_min", "enthalpy_max", "relhum_min", "relhum_max"))
                psychro_zone_property_rh(row, "enthalpy", units, pres, mollier, n, group, na.rm, tdb_lim)
            },
            `specvol-rh` = {
                psychro_required_columns(row, c("specvol_min", "specvol_max", "relhum_min", "relhum_max"))
                psychro_zone_property_rh(row, "specvol", units, pres, mollier, n, group, na.rm, tdb_lim)
            },
            `dbt-wmax` = {
                psychro_required_columns(row, c("tdb_min", "tdb_max", "humratio_max"))
                psychro_zone_dbt_wmax(row, units, pres, mollier, n, group, na.rm)
            },
            stop("Invalid psychrometric zone type.", call. = FALSE)
        )
    })

    do.call(rbind, zones)
}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
StatPsychroZone <- ggplot2::ggproto(
    "StatPsychroZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    required_aes = character(),

    optional_aes = c(
        "tdb", "humratio", "tdb_min", "tdb_max", "humratio_min",
        "humratio_max", "relhum_min", "relhum_max", "enthalpy_min",
        "enthalpy_max", "specvol_min", "specvol_max"
    ),

    extra_params = c("na.rm", "type", "n", "units", "pres", "mollier", "tdb_lim"),

    compute_panel = function(self, data, scales, type = "dbt-rh", n = 100L,
                             units, pres, mollier = FALSE, tdb_lim = NULL,
                             na.rm = FALSE) {
        psychro_compute_zone(data, type, n, units, pres, mollier, tdb_lim, na.rm)
    }
)
