#' @include comfort-core.R
NULL

# Givoni strategy geometry is fixed-shape chart construction rather than a
# continuous comfort model, so it lives outside the generic grid/contour helpers.

#' Givoni-Milne strategy overlay
#'
#' `comfort_strategy_givoni()` stores the fixed inputs used by
#' `geom_comfort_givoni()`. The adaptive variant shifts the base comfort zone
#' from a mean outdoor temperature. The fixed variant anchors the comfort zone
#' to the common Givoni/Milne 1979 bounds: 20 to 25.5 degrees C dry-bulb
#' temperature and 20% to 80% relative humidity with the hot-humid corner
#' clipped. Strategy zones are drawn in dry-bulb/relative-humidity space before
#' conversion to humidity ratio.
#'
#' @details
#' This overlay is a climate-screening and design-strategy aid. It should not be
#' interpreted as a comfort-standard compliance method or as a substitute for
#' building energy/thermal simulation. In particular, the high-mass and night
#' ventilation regions indicate potential strategy ranges; using them to count
#' comfort hours requires daily temperature profiles, nighttime conditions, and
#' building assumptions that are outside this layer.
#'
#' @param mean_outdoor Mean or running-mean outdoor temperature used to adapt the
#'   base comfort zone when `variant = "adaptive"`. It is ignored when
#'   `variant = "fixed"` and can be `NULL` for that variant.
#' @param units Unit system for `mean_outdoor`, `"SI"` or `"IP"`.
#' @param variant Givoni-Milne strategy variant. `"adaptive"` shifts the comfort
#'   anchor from `mean_outdoor`; `"fixed"` uses the fixed 1979 comfort anchor.
#' @param tdb_range Optional dry-bulb comfort-anchor range used when
#'   `variant = "fixed"`. Values use `units`. When `NULL`, the fixed variant
#'   uses 20 to 25.5 degrees C.
#' @param relhum_range Relative-humidity comfort-anchor range in percent.
#'
#' @return A Givoni-Milne comfort strategy object.
#'
#' @references
#' Andrew Marsh, Psychrometric Chart,
#' \url{https://andrewmarsh.com/software/psychro-chart-web/}
#'
#' Herb S, Wolk S, Reinhart C. Beyond the bioclimatic chart: An automated
#' simulation-based method for the assessment of natural ventilation and passive
#' design potential. Building and Environment, 269, 112362.
#' \doi{10.1016/j.buildenv.2024.112362}
#'
#' @examples
#' # Create an adaptive Givoni-Milne strategy for a warm outdoor mean.
#' comfort_strategy_givoni(mean_outdoor = 22)
#'
#' # Use the fixed Givoni/Milne 1979 comfort anchor instead.
#' comfort_strategy_givoni(variant = "fixed", mean_outdoor = NULL)
#'
#' # Or provide project-specific comfort anchor ranges.
#' comfort_strategy_givoni(
#'     variant = "fixed",
#'     mean_outdoor = NULL,
#'     tdb_range = c(22, 27),
#'     relhum_range = c(30, 70)
#' )
#'
#' # Draw the Givoni strategy overlay for that outdoor mean.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         strategy = comfort_strategy_givoni(mean_outdoor = 22),
#'         labels = FALSE
#'     )
#'
#' @export
comfort_strategy_givoni <- function(
    mean_outdoor = 19,
    units = c("SI", "IP"),
    variant = c("adaptive", "fixed"),
    tdb_range = NULL,
    relhum_range = c(20, 80)
) {
    units <- match.arg(units)
    variant <- match.arg(variant)
    if (variant == "adaptive" && !is.null(tdb_range)) {
        stop(
            "`tdb_range` is only used when `variant = \"fixed\"`.",
            call. = FALSE
        )
    }
    tdb_range <- givoni__check_tdb_range(tdb_range)
    relhum_range <- givoni__check_relhum_range(relhum_range)
    if (is.null(mean_outdoor)) {
        if (variant == "adaptive") {
            stop(
                "`mean_outdoor` must be supplied when `variant = \"adaptive\"`.",
                call. = FALSE
            )
        }
        mean_outdoor <- NA_real_
    } else {
        mean_outdoor <- as.numeric(mean_outdoor)
    }
    if (
        variant == "adaptive" &&
            (length(mean_outdoor) != 1L || !is.finite(mean_outdoor))
    ) {
        stop(
            "`mean_outdoor` must be a single finite temperature.",
            call. = FALSE
        )
    } else if (variant == "fixed" && length(mean_outdoor) != 1L) {
        stop(
            "`mean_outdoor` must be a single temperature or `NULL`.",
            call. = FALSE
        )
    }
    structure(
        list(
            mean_outdoor = mean_outdoor,
            units = units,
            variant = variant,
            tdb_range = tdb_range,
            relhum_range = relhum_range
        ),
        class = c("PsyComfortGivoniStrategy", "list")
    )
}

# Validate an optional dry-bulb anchor range supplied in the strategy units.
givoni__check_tdb_range <- function(tdb_range) {
    if (is.null(tdb_range)) {
        return(NULL)
    }
    tdb_range <- as.numeric(tdb_range)
    if (length(tdb_range) != 2L || any(!is.finite(tdb_range))) {
        stop(
            "`tdb_range` must be a finite numeric vector of length 2.",
            call. = FALSE
        )
    }
    if (tdb_range[[1L]] >= tdb_range[[2L]]) {
        stop("`tdb_range` must be strictly increasing.", call. = FALSE)
    }
    tdb_range
}

# Validate the relative-humidity anchor range supplied in percent.
givoni__check_relhum_range <- function(relhum_range) {
    relhum_range <- as.numeric(relhum_range)
    if (length(relhum_range) != 2L || any(!is.finite(relhum_range))) {
        stop(
            "`relhum_range` must be a finite numeric vector of length 2.",
            call. = FALSE
        )
    }
    if (
        relhum_range[[1L]] < 0 ||
            relhum_range[[2L]] > 100 ||
            relhum_range[[1L]] >= relhum_range[[2L]]
    ) {
        stop(
            "`relhum_range` must be strictly increasing and within [0, 100].",
            call. = FALSE
        )
    }
    relhum_range
}

#' Comfort zone style element
#'
#' `element_givoni_zone()` creates a small style object for comfort strategy
#' zones. It is used by `geom_comfort_givoni()` through the `zone_style`
#' argument to override the default Givoni-Milne zone styles.
#'
#' @param fill,colour,color,linewidth,linetype,alpha,linejoin Zone drawing
#'   properties. Values left as [ggplot2::waiver()] inherit the layer default.
#'
#' @return A comfort zone style element.
#'
#' @examples
#' # Fill and outline the comfort zone with custom colours.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         labels = FALSE,
#'         zone_style = list(
#'             comfort = element_givoni_zone(
#'                 fill = "#6FCF97",
#'                 colour = "#1B7F4A",
#'                 alpha = 0.35
#'             )
#'         )
#'     )
#'
#' # Emphasize the air-conditioning region with a light fill.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         labels = FALSE,
#'         zone_style = list(
#'             air_conditioning = element_givoni_zone(
#'                 fill = "#7BC8F6",
#'                 colour = "#1B5E8C",
#'                 alpha = 0.18,
#'                 linetype = "solid"
#'             )
#'         )
#'     )
#'
#' # Restyle a line-only region without filling it.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         labels = FALSE,
#'         zone_style = list(
#'             winter = element_givoni_zone(
#'                 colour = "#C44536",
#'                 linewidth = 1.2,
#'                 linetype = "dashed"
#'             )
#'         )
#'     )
#'
#' @export
element_givoni_zone <- function(
    fill = ggplot2::waiver(),
    colour = ggplot2::waiver(),
    linewidth = ggplot2::waiver(),
    linetype = ggplot2::waiver(),
    alpha = ggplot2::waiver(),
    linejoin = ggplot2::waiver(),
    color = NULL
) {
    if (!is.null(color)) {
        colour <- color
    }
    structure(
        list(
            fill = fill,
            colour = colour,
            linewidth = linewidth,
            linetype = linetype,
            alpha = alpha,
            linejoin = linejoin
        ),
        class = c("PsyComfortZoneElement", "list")
    )
}

# Store Givoni mean-outdoor foreground marker metadata for coord rendering.
givoni__foreground_marker <- function(
    strategy,
    show_label,
    colour,
    linewidth,
    linetype,
    label_size,
    fontface
) {
    structure(
        list(
            type = "givoni_mean_outdoor",
            strategy = strategy,
            show_label = show_label,
            colour = colour,
            linewidth = linewidth,
            linetype = linetype,
            label_size = label_size,
            fontface = fontface
        ),
        class = "PsyComfortForeground"
    )
}

# Return the normalized strategy variant stored by the public constructor.
givoni__variant <- function(strategy) {
    strategy$variant %||% "adaptive"
}

# Test whether the strategy geometry is adapted from mean outdoor temperature.
givoni__is_adaptive <- function(strategy) {
    identical(givoni__variant(strategy), "adaptive")
}

# Compute the Givoni-Milne lower comfort dry-bulb temperature in SI units.
givoni__base_temp <- function(strategy) {
    if (!givoni__is_adaptive(strategy)) {
        if (!is.null(strategy$tdb_range)) {
            return(comfort__to_si_temp(strategy$tdb_range, strategy$units)[[
                1L
            ]])
        }
        return(20)
    }
    mean_outdoor_si <- comfort__to_si_temp(
        strategy$mean_outdoor,
        strategy$units
    )
    # The adaptive variant uses a neutral temperature shifted from the outdoor
    # mean. The base is the lower comfort edge, so this intentionally differs
    # from the fixed 1979 Givoni/Milne comfort polygon.
    round(17.6 + 0.31 * mean_outdoor_si - 3.5, 1L)
}

# Return the SI dry-bulb anchors shared by zones and labels.
givoni__anchors <- function(strategy) {
    relhum <- strategy$relhum_range %||% c(20, 80)
    base <- givoni__base_temp(strategy)
    if (givoni__is_adaptive(strategy)) {
        return(list(
            base = base,
            comfort_right = base + 7,
            comfort_top_right = base + 5,
            comfort_mid = base + 3.5,
            relhum_low = relhum[[1L]],
            relhum_high = relhum[[2L]],
            relhum_mid = mean(relhum)
        ))
    }
    # The fixed variant uses the commonly cited Givoni/Milne 1979 comfort
    # anchor unless the user supplies a project-specific dry-bulb range. The
    # upper RH corner is clipped just before the right edge.
    tdb_range_si <- if (is.null(strategy$tdb_range)) {
        c(20, 25.5)
    } else {
        comfort__to_si_temp(strategy$tdb_range, strategy$units)
    }
    base <- tdb_range_si[[1L]]
    comfort_right <- tdb_range_si[[2L]]
    list(
        base = base,
        comfort_right = comfort_right,
        comfort_top_right = base + diff(tdb_range_si) * 0.9,
        comfort_mid = mean(tdb_range_si),
        relhum_low = relhum[[1L]],
        relhum_high = relhum[[2L]],
        relhum_mid = mean(relhum)
    )
}

# Return the fixed Givoni strategy zone metadata used for drawing and labels.
givoni__zone_specs <- function() {
    util__new_data_frame(list(
        zone = c(
            "comfort",
            "natural_ventilation",
            "internal_gains",
            "passive_solar_heating",
            "active_solar_heating",
            "evaporative_cooling",
            "mass_cooling",
            "mass_cooling_night_ventilation",
            "winter",
            "air_conditioning",
            "air_conditioning_dehumidification",
            "humidification"
        ),
        label = c(
            "COMFORT\nZONE",
            "NATURAL VENTILATION",
            "INTERNAL\nGAINS",
            "PASSIVE SOLAR\nHEATING",
            "ACTIVE\nSOLAR\nHEATING",
            "EVAPORATIVE COOLING",
            "MASS COOLING",
            "MASS COOLING &\nNIGHT VENTILATION",
            "WINTER",
            "AIR-CONDITIONING",
            "AIR-CONDITIONING &\nDEHUMIDIFICATION",
            "HUMIDIFICATION"
        ),
        fill = c(
            "#5BD96A",
            rep(NA_character_, 11L)
        ),
        linetype = c(
            "solid",
            "solid",
            "solid",
            "solid",
            "solid",
            "solid",
            "solid",
            "solid",
            "dashed",
            "longdash",
            "longdash",
            "solid"
        ),
        filled = c(TRUE, rep(FALSE, 11L)),
        draw_zone = c(
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            FALSE
        )
    ))
}

# Return the accepted style fields for Givoni zone overrides.
givoni__zone_style_fields <- function() {
    c("fill", "colour", "color", "linewidth", "linetype", "alpha", "linejoin")
}

# Test whether a Givoni zone fill override should mark the zone as filled.
givoni__zone_fill_is_set <- function(fill) {
    !is.null(fill) && length(fill) == 1L && !is.na(fill)
}

# Validate the named Givoni zone style override list.
givoni__check_zone_style <- function(zone_style, zone_names) {
    if (is.null(zone_style)) {
        return(list())
    }
    if (
        !is.list(zone_style) ||
            is.null(names(zone_style)) ||
            any(!nzchar(names(zone_style)))
    ) {
        stop(
            "`zone_style` must be a named list of comfort zone style ",
            "overrides.",
            call. = FALSE
        )
    }
    unknown <- setdiff(names(zone_style), zone_names)
    if (length(unknown)) {
        stop(
            "Unknown Givoni zone style name: ",
            paste(unknown, collapse = ", "),
            call. = FALSE
        )
    }
    zone_style
}

# Normalize one Givoni zone style override into layer parameter names.
givoni__zone_style_to_params <- function(style) {
    if (
        inherits(style, "PsyComfortZoneElement") ||
            inherits(style, "ggplot2::element_polygon")
    ) {
        out <- list(
            fill = style$fill,
            colour = style$colour,
            linewidth = style$linewidth,
            linetype = style$linetype,
            linejoin = style$linejoin
        )
        if (!is.null(style$alpha)) {
            out$alpha <- style$alpha
        }
    } else if (is.list(style)) {
        out <- style
    } else {
        stop(
            "`zone_style` values must be created by element_givoni_zone(), ",
            "ggplot2::element_polygon(), or ordinary named lists.",
            call. = FALSE
        )
    }

    if (is.null(names(out))) {
        stop("Zone style lists must be named.", call. = FALSE)
    }
    unknown <- setdiff(names(out), givoni__zone_style_fields())
    if (length(unknown)) {
        stop(
            "Unknown comfort zone style field: ",
            paste(unknown, collapse = ", "),
            call. = FALSE
        )
    }
    if (!is.null(out$color)) {
        out$colour <- out$color
        out$color <- NULL
    }
    out <- out[!vapply(out, ggplot2::is_waiver, logical(1L))]
    out <- out[!vapply(out, is.null, logical(1L))]
    out
}

# Merge layer, default, and per-zone style settings for one Givoni zone.
givoni__zone_params <- function(
    spec,
    params,
    zone_style,
    zone_alpha,
    na.rm,
    strategy
) {
    zone_is_filled <- isTRUE(spec$filled[[1L]])
    defaults <- list(
        na.rm = na.rm,
        strategy = strategy,
        zone = spec$zone[[1L]],
        fill = if (zone_is_filled) spec$fill[[1L]] else NA_character_,
        colour = "#444444",
        linewidth = 0.9,
        linetype = spec$linetype[[1L]],
        alpha = if (zone_is_filled) zone_alpha else 1
    )
    out <- utils::modifyList(defaults, params)
    style <- zone_style[[spec$zone[[1L]]]]
    if (!is.null(style)) {
        style_params <- givoni__zone_style_to_params(style)
        out <- utils::modifyList(out, style_params)
        if (
            !("alpha" %in% names(style_params)) &&
                "fill" %in% names(style_params) &&
                givoni__zone_fill_is_set(style_params$fill)
        ) {
            out$alpha <- zone_alpha
        }
    }
    out
}

# Return an empty zone data frame with the expected computed columns.
givoni__empty_zone <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        x = numeric(),
        y = numeric(),
        zone = character(),
        label = character(),
        group = integer(),
        subgroup = integer()
    ))
}

# Return an empty label data frame with the expected computed columns.
givoni__empty_label <- function() {
    util__new_data_frame(list(
        tdb = numeric(),
        humratio = numeric(),
        x = numeric(),
        y = numeric(),
        zone = character(),
        label = character(),
        angle = numeric(),
        hjust = numeric(),
        vjust = numeric(),
        group = integer()
    ))
}

# Convert a Givoni dry-bulb/RH point to SI humidity ratio.
givoni__humratio <- function(tdb_si, rh, pressure_pa) {
    psychrolib__with_units(
        "SI",
        psychrolib::GetHumRatioFromRelHum(tdb_si, rh / 100, pressure_pa)
    )
}

# Convert a Givoni dry-bulb/RH point to g/kg humidity ratio.
givoni__hum_gkg <- function(tdb_si, rh, pressure_pa) {
    givoni__humratio(tdb_si, rh, pressure_pa) * 1000
}

# Build one Givoni geometry point in SI chart coordinates.
givoni__point <- function(tdb_si, hum_gkg) {
    util__new_data_frame(list(tdb_si = tdb_si, humratio = hum_gkg / 1000))
}

# Build a curved Givoni path between two dry-bulb/RH points.
givoni__rh_path <- function(
    t0,
    rh0,
    t1,
    rh1,
    pressure_pa,
    max_gkg = Inf,
    n = NULL
) {
    if (is.null(n)) {
        n <- max(8L, ceiling(abs(t1 - t0) * 4L) + 1L)
    }
    # Curved chart edges are specified in temperature/RH space, then converted
    # to humidity ratio so they follow psychrometric curvature on the plot.
    tdb <- seq(t0, t1, length.out = n)
    rh <- seq(rh0, rh1, length.out = n)
    hum_gkg <- pmin(
        givoni__hum_gkg(tdb, rh, pressure_pa),
        max_gkg
    )
    util__new_data_frame(list(tdb_si = tdb, humratio = hum_gkg / 1000))
}

# Build the raw SI polygon path for one named Givoni strategy zone.
givoni__polygon <- function(
    zone,
    anchors,
    pressure_pa,
    tdb_max_si,
    hum_min_gkg
) {
    if (zone %in% c("air_conditioning_dehumidification", "humidification")) {
        return(givoni__point(numeric(), numeric()))
    }
    # Zone vertices follow the Givoni-Milne screening overlay in dry-bulb and
    # RH terms. Humidity caps keep upper edges from extending beyond the comfort
    # maximum, but these outlines are not a standalone comfort-hour
    # classification method.
    base <- anchors$base
    comfort_right <- anchors$comfort_right
    comfort_top_right <- anchors$comfort_top_right
    comfort_mid <- anchors$comfort_mid
    relhum_low <- anchors$relhum_low
    relhum_high <- anchors$relhum_high
    relhum_mid <- anchors$relhum_mid
    hum_low <- function(tdb) givoni__hum_gkg(tdb, relhum_low, pressure_pa)
    hum_mid <- function(tdb) givoni__hum_gkg(tdb, relhum_mid, pressure_pa)
    hum_high <- function(tdb) givoni__hum_gkg(tdb, relhum_high, pressure_pa)
    hum20 <- function(tdb) givoni__hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) givoni__hum_gkg(tdb, 30, pressure_pa)
    hum100 <- function(tdb) givoni__hum_gkg(tdb, 100, pressure_pa)
    max_comfort_gkg <- min(16, hum_high(comfort_top_right))
    comfort_bottom <- hum_low(base)
    evap_left <- base + 2.4528 * (comfort_bottom - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)

    parts <- switch(
        zone,
        comfort = list(
            givoni__rh_path(
                base,
                relhum_high,
                comfort_top_right,
                relhum_high,
                pressure_pa,
                16
            ),
            givoni__point(
                comfort_right,
                min(max_comfort_gkg, hum_mid(comfort_right))
            ),
            givoni__point(comfort_right, hum_low(comfort_right)),
            givoni__rh_path(
                comfort_right,
                relhum_low,
                base,
                relhum_low,
                pressure_pa
            ),
            givoni__point(base, hum_high(base))
        ),
        natural_ventilation = list(
            givoni__rh_path(base, 100, comfort_right, 100, pressure_pa),
            givoni__point(base + 12, hum_mid(base + 12)),
            givoni__point(base + 12, hum_low(base + 12)),
            givoni__rh_path(
                base + 12,
                relhum_low,
                base,
                relhum_low,
                pressure_pa
            ),
            givoni__point(base, hum100(base))
        ),
        internal_gains = list(
            givoni__rh_path(
                base - 2.5,
                relhum_low,
                base - 7,
                relhum_low,
                pressure_pa
            ),
            givoni__point(base - 7.5, hum_low(base - 7.5)),
            givoni__point(base - 7.5, min(hum_high(base - 7.5), 16)),
            givoni__rh_path(
                base - 7.5,
                relhum_high,
                base - 2.5,
                relhum_high,
                pressure_pa,
                16
            )
        ),
        passive_solar_heating = list(
            givoni__point(comfort_mid, 0),
            givoni__point(base - 12, 0),
            givoni__point(base - 12, hum100(base - 12)),
            givoni__rh_path(base - 12, 100, base - 1, 100, pressure_pa)
        ),
        active_solar_heating = list(
            givoni__point(base - 13, 0),
            givoni__point(base - 16, 0),
            givoni__point(base - 16, hum100(base - 16)),
            givoni__rh_path(base - 16, 100, base - 13, 100, pressure_pa)
        ),
        evaporative_cooling = list(
            givoni__point(comfort_top_right, max_comfort_gkg),
            givoni__point(
                base + 16,
                min(max_comfort_gkg, hum30(base + 16))
            ),
            givoni__point(
                base + 19,
                min(max_comfort_gkg, hum20(base + 19))
            ),
            givoni__point(
                base + 21,
                min(
                    max_comfort_gkg,
                    givoni__hum_gkg(base + 21, 10, pressure_pa)
                )
            ),
            givoni__point(base + 21, 0),
            givoni__point(evap_left, 0),
            givoni__point(base, comfort_bottom)
        ),
        mass_cooling = list(
            givoni__point(comfort_top_right, max_comfort_gkg),
            givoni__point(base + 13, max_comfort_gkg),
            givoni__point(
                base + 17,
                min(max_comfort_gkg, hum30(base + 17))
            ),
            givoni__point(base + 17, min(max_comfort_gkg, hum_low(base))),
            givoni__point(base, min(max_comfort_gkg, hum_low(base)))
        ),
        mass_cooling_night_ventilation = list(
            givoni__point(base + 13, max_comfort_gkg),
            givoni__point(base + 20, max_comfort_gkg),
            givoni__point(
                base + 24,
                min(max_comfort_gkg, hum20(base + 24))
            ),
            givoni__point(base + 24, min(max_comfort_gkg, hum_low(base))),
            givoni__point(base, min(max_comfort_gkg, hum_low(base)))
        ),
        winter = list(
            givoni__rh_path(
                base - 0.5,
                relhum_low,
                base - 2,
                relhum_low,
                pressure_pa
            ),
            givoni__point(base - 2, hum_low(base - 2)),
            givoni__point(base - 2, min(hum_high(base - 2), 16)),
            givoni__rh_path(
                base - 2,
                relhum_high,
                base - 0.5,
                relhum_high,
                pressure_pa,
                16
            )
        ),
        air_conditioning = list(
            givoni__point(base + 20, max_comfort_gkg),
            givoni__point(right_air, max_comfort_gkg),
            givoni__point(right_air, 0),
            givoni__point(base + 21, 0)
        ),
        air_conditioning_dehumidification = list(
            givoni__point(base + 20, max_comfort_gkg),
            givoni__point(right_air, max_comfort_gkg),
            givoni__point(right_air, 30),
            givoni__point(base + 20, 30)
        ),
        humidification = list(
            givoni__point(base - 12, 0),
            givoni__point(base, 0),
            givoni__point(base, hum_low(base)),
            givoni__point(base - 12, hum_low(base - 12))
        ),
        stop("Unknown Givoni zone: ", zone, call. = FALSE)
    )

    out <- do.call(rbind, parts)
    row.names(out) <- NULL
    out
}

# Convert selected Givoni zones into plot-ready chart data.
givoni__zone_data <- function(
    strategy,
    zone,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    psychro_scales = NULL
) {
    strategy <- givoni__check_strategy(strategy)
    specs <- givoni__zone_specs()
    if (is.null(zone)) {
        zone <- specs$zone[specs$draw_zone]
    }
    zone <- match.arg(zone, specs$zone, several.ok = TRUE)
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort__pressure_pa(pres, units)
    anchors <- givoni__anchors(strategy)
    tdb_max_si <- comfort__to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- unit__hum_from_chart(lim$hum[[1L]], units) * 1000

    pieces <- vector("list", length(zone))
    for (i in seq_along(zone)) {
        poly <- givoni__polygon(
            zone[[i]],
            anchors,
            pressure_pa,
            tdb_max_si,
            hum_min_gkg
        )
        if (!nrow(poly)) {
            next
        }
        spec <- specs[match(zone[[i]], specs$zone), , drop = FALSE]
        pieces[[i]] <- util__new_data_frame(list(
            tdb = comfort__from_si_temp(poly$tdb_si, units),
            humratio = poly$humratio,
            zone = spec$zone,
            label = spec$label,
            group = i,
            subgroup = 1L
        ))
    }
    pieces <- pieces[!vapply(pieces, is.null, logical(1L))]
    if (!length(pieces)) {
        return(givoni__empty_zone())
    }
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
    out <- givoni__clip_humratio(out, lim$hum, units)
    state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}

# Build one path-label entry with consistent label columns.
givoni__label_path_entry <- function(
    zone,
    label,
    path,
    hjust = 0.5,
    vjust = 0.5,
    group = 1L
) {
    if (!nrow(path)) {
        return(givoni__empty_label())
    }
    util__new_data_frame(list(
        tdb_si = path$tdb_si,
        humratio = path$humratio,
        zone = zone,
        label = label,
        angle = NA_real_,
        hjust = hjust,
        vjust = vjust,
        group = group
    ))
}

# Build path-following label specs for Givoni strategy zones.
givoni__label_path_specs <- function(
    anchors,
    pressure_pa,
    tdb_max_si,
    hum_min_gkg
) {
    # Path labels reuse the same geometric primitives as zones so text follows
    # curved RH boundaries and vertical strategy lines consistently.
    base <- anchors$base
    comfort_right <- anchors$comfort_right
    comfort_top_right <- anchors$comfort_top_right
    relhum_low <- anchors$relhum_low
    relhum_high <- anchors$relhum_high
    hum_low <- function(tdb) givoni__hum_gkg(tdb, relhum_low, pressure_pa)
    hum_high <- function(tdb) givoni__hum_gkg(tdb, relhum_high, pressure_pa)
    max_comfort_gkg <- min(
        16,
        hum_high(comfort_top_right)
    )
    hum30 <- function(tdb) givoni__hum_gkg(tdb, 30, pressure_pa)
    hum100 <- function(tdb) givoni__hum_gkg(tdb, 100, pressure_pa)
    evap_left <- base + 2.4528 * (hum_low(base) - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)
    air_label_x <- min(right_air - 1.0, tdb_max_si - 0.8)
    air_label_x <- max(air_label_x, base + 21)

    paths <- list(
        givoni__label_path_entry(
            "natural_ventilation",
            "NATURAL VENTILATION",
            givoni__rh_path(base, 100, comfort_right, 100, pressure_pa),
            hjust = 0.5,
            vjust = 1.8,
            group = 1L
        ),
        givoni__label_path_entry(
            "internal_gains",
            "INTERNAL GAINS",
            rbind(
                givoni__point(base - 7.5, min(hum_high(base - 7.5), 16)),
                givoni__point(base - 7.5, hum_low(base - 7.5))
            ),
            hjust = 0.5,
            vjust = -0.25,
            group = 2L
        ),
        givoni__label_path_entry(
            "passive_solar_heating",
            "PASSIVE SOLAR",
            rbind(
                givoni__point(base - 12, hum100(base - 12)),
                givoni__point(base - 12, 0)
            ),
            hjust = 0.5,
            vjust = -0.25,
            group = 3L
        ),
        givoni__label_path_entry(
            "active_solar_heating",
            "ACTIVE SOLAR",
            rbind(
                givoni__point(base - 16, hum100(base - 16)),
                givoni__point(base - 16, 0)
            ),
            hjust = 0.5,
            vjust = -0.25,
            group = 4L
        ),
        givoni__label_path_entry(
            "evaporative_cooling",
            "EVAPORATIVE COOLING",
            rbind(
                givoni__point(evap_left, 0),
                givoni__point(base + 21, 0)
            ),
            hjust = 0.68,
            vjust = -0.35,
            group = 5L
        ),
        givoni__label_path_entry(
            "mass_cooling",
            "MASS COOLING",
            rbind(
                givoni__point(
                    base + 17,
                    min(max_comfort_gkg, hum30(base + 17))
                ),
                givoni__point(
                    base + 17,
                    min(max_comfort_gkg, hum_low(base))
                )
            ),
            hjust = 0.48,
            vjust = -0.25,
            group = 6L
        ),
        givoni__label_path_entry(
            "mass_cooling_night_ventilation",
            "MASS COOLING &\nNIGHT VENTILATION",
            rbind(
                givoni__point(
                    base + 24,
                    min(
                        max_comfort_gkg,
                        givoni__hum_gkg(base + 24, 20, pressure_pa)
                    )
                ),
                givoni__point(
                    base + 24,
                    min(max_comfort_gkg, hum_low(base))
                )
            ),
            hjust = 0.5,
            vjust = -0.25,
            group = 7L
        ),
        givoni__label_path_entry(
            "winter",
            "WINTER",
            rbind(
                givoni__point(base - 2, min(hum_high(base - 2), 16)),
                givoni__point(base - 2, hum_low(base - 2))
            ),
            hjust = 0.5,
            vjust = -0.25,
            group = 8L
        ),
        givoni__label_path_entry(
            "air_conditioning",
            "AIR-CONDITIONING",
            rbind(
                givoni__point(air_label_x, max_comfort_gkg),
                givoni__point(air_label_x, 0)
            ),
            hjust = 0.5,
            vjust = -0.25,
            group = 9L
        ),
        givoni__label_path_entry(
            "humidification",
            "HUMIDIFICATION",
            rbind(
                givoni__point(base - 12, 0),
                givoni__point(base, 0)
            ),
            hjust = 0.5,
            vjust = -0.35,
            group = 10L
        )
    )
    out <- do.call(rbind, paths)
    row.names(out) <- NULL
    out
}

# Build point label specs for compact Givoni strategy labels.
givoni__label_point_specs <- function(
    anchors,
    pressure_pa,
    tdb_max_si,
    hum_min_gkg = 0
) {
    base <- anchors$base
    comfort_top_right <- anchors$comfort_top_right
    comfort_mid <- anchors$comfort_mid
    relhum_high <- anchors$relhum_high
    max_comfort_gkg <- min(
        16,
        givoni__hum_gkg(comfort_top_right, relhum_high, pressure_pa)
    )
    heating_x <- base - 18
    heating_hum_gkg <- mean(c(
        hum_min_gkg,
        givoni__hum_gkg(heating_x, 100, pressure_pa)
    ))
    util__new_data_frame(list(
        zone = c(
            "comfort",
            "heating",
            "air_conditioning_dehumidification"
        ),
        label = c(
            "COMFORT\nZONE",
            "HEATING",
            "AIR-CONDITIONING &\nDEHUMIDIFICATION"
        ),
        tdb_si = c(
            comfort_mid,
            base - 18,
            base + 19
        ),
        hum_gkg = c(
            max_comfort_gkg * 0.55,
            heating_hum_gkg,
            max_comfort_gkg + 6
        ),
        angle = c(0, 270, 0),
        hjust = c(0.5, 0.5, 0.5),
        vjust = c(0.5, 0.5, 0.5)
    ))
}

# Convert Givoni label specs into plot-ready chart data.
givoni__label_data <- function(
    strategy,
    label_type,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    psychro_scales = NULL
) {
    label_type <- match.arg(label_type, c("path", "point"))
    strategy <- givoni__check_strategy(strategy)
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort__pressure_pa(pres, units)
    anchors <- givoni__anchors(strategy)
    tdb_max_si <- comfort__to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- unit__hum_from_chart(lim$hum[[1L]], units) * 1000

    if (label_type == "path") {
        labels <- givoni__label_path_specs(
            anchors,
            pressure_pa,
            tdb_max_si,
            hum_min_gkg
        )
        out <- util__new_data_frame(list(
            tdb = comfort__from_si_temp(labels$tdb_si, units),
            humratio = labels$humratio,
            zone = labels$zone,
            label = labels$label,
            angle = labels$angle,
            hjust = labels$hjust,
            vjust = labels$vjust,
            group = labels$group
        ))
        out <- givoni__clip_humratio(out, lim$hum, units)
        return(state__output_xy(
            out,
            out$tdb,
            out$humratio,
            mollier,
            psychro_scales = psychro_scales,
            units = units
        ))
    }

    labels <- givoni__label_point_specs(
        anchors,
        pressure_pa,
        tdb_max_si,
        hum_min_gkg
    )
    out <- util__new_data_frame(list(
        tdb = comfort__from_si_temp(labels$tdb_si, units),
        humratio = labels$hum_gkg / 1000,
        zone = labels$zone,
        label = labels$label,
        angle = labels$angle,
        hjust = labels$hjust,
        vjust = labels$vjust,
        group = seq_len(nrow(labels))
    ))
    out <- givoni__clip_humratio(out, lim$hum, units)
    state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}

# Clamp Givoni humidity ratios to the visible chart range.
givoni__clip_humratio <- function(data, hum_lim, units) {
    hum_lim <- unit__hum_from_chart(hum_lim, units)
    data$humratio <- pmin(pmax(data$humratio, hum_lim[[1L]]), hum_lim[[2L]])
    data
}

# Return mean-outdoor label rotation for normal or Mollier orientation.
givoni__mean_outdoor_label_angle <- function(mollier) {
    if (isTRUE(mollier)) 0 else 270
}

# Return mean-outdoor label vertical adjustment for chart orientation.
givoni__mean_outdoor_label_vjust <- function(mollier) {
    if (isTRUE(mollier)) 1.25 else -0.25
}

# Compute saturation, top, and label humidity ratios for the marker.
givoni__mean_outdoor_marker <- function(
    mean_si,
    pressure_pa,
    hum_lim_narrow
) {
    # The mean-outdoor marker runs from the visible lower humidity limit to just
    # past saturation, leaving a short extension for the numeric label.
    hum_sat <- givoni__humratio(mean_si, 100, pressure_pa)
    if (!is.finite(hum_sat)) {
        return(NULL)
    }
    hum_extension <- max(0.0015, diff(hum_lim_narrow) * 0.08)
    hum_top <- min(hum_lim_narrow[[2L]], hum_sat + hum_extension)
    if (!is.finite(hum_top) || hum_top <= hum_lim_narrow[[1L]]) {
        return(NULL)
    }
    hum_label <- min(hum_top, hum_sat + hum_extension * 0.65)
    hum_label <- pmax(hum_lim_narrow[[1L]], hum_label)

    list(saturation = hum_sat, top = hum_top, label = hum_label)
}

# Convert the mean-outdoor marker line into plot-ready chart data.
givoni__mean_outdoor_data <- function(
    strategy,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    psychro_scales = NULL
) {
    strategy <- givoni__check_strategy(strategy)
    if (!givoni__is_adaptive(strategy)) {
        return(comfort_contour__empty())
    }
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort__pressure_pa(pres, units)
    mean_si <- comfort__to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- unit__hum_from_chart(lim$hum, units)
    marker <- givoni__mean_outdoor_marker(
        mean_si,
        pressure_pa,
        hum_lim_narrow
    )
    if (is.null(marker)) {
        return(comfort_contour__empty())
    }
    out <- util__new_data_frame(list(
        tdb = comfort__from_si_temp(c(mean_si, mean_si), units),
        humratio = c(hum_lim_narrow[[1L]], marker$top),
        level = mean_si,
        group = 1L,
        metric = "givoni_mean_outdoor"
    ))
    state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}

# Convert the mean-outdoor marker label into plot-ready chart data.
givoni__mean_outdoor_label_data <- function(
    strategy,
    units,
    pres,
    mollier,
    tdb_lim,
    hum_lim,
    psychro_scales = NULL
) {
    strategy <- givoni__check_strategy(strategy)
    if (!givoni__is_adaptive(strategy)) {
        return(givoni__empty_label())
    }
    lim <- comfort_grid__limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort__pressure_pa(pres, units)
    mean_si <- comfort__to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- unit__hum_from_chart(lim$hum, units)
    marker <- givoni__mean_outdoor_marker(
        mean_si,
        pressure_pa,
        hum_lim_narrow
    )
    if (is.null(marker)) {
        return(givoni__empty_label())
    }

    label_temp <- comfort__from_si_temp(mean_si, units)
    unit_label <- if (units == "IP") "\u00b0F" else "\u00b0C"
    out <- util__new_data_frame(list(
        tdb = comfort__from_si_temp(mean_si, units),
        humratio = marker$label,
        zone = "mean_outdoor",
        label = sprintf("%.1f %s", label_temp, unit_label),
        angle = givoni__mean_outdoor_label_angle(mollier),
        hjust = 0.5,
        vjust = givoni__mean_outdoor_label_vjust(mollier),
        group = 1L
    ))
    state__output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}
