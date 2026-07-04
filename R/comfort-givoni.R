#' @include comfort-core.R
NULL

# Givoni strategy geometry is fixed-shape chart construction rather than a
# continuous comfort model, so it lives outside the generic grid/contour helpers.

#' Givoni bioclimatic strategy
#'
#' `comfort_strategy_givoni()` stores the fixed inputs used by
#' `geom_comfort_givoni()`. The strategy geometry follows Marsh's Givoni
#' Bioclimatic Chart overlay: the mean outdoor temperature shifts the base
#' comfort zone, and zones are drawn in dry-bulb/relative-humidity space before
#' conversion to humidity ratio.
#'
#' @param mean_outdoor Mean outdoor temperature.
#' @param units Unit system for `mean_outdoor`, `"SI"` or `"IP"`.
#'
#' @return A Givoni comfort strategy object.
#'
#' @examples
#' # Create a Givoni strategy for a warm outdoor mean.
#' comfort_strategy_givoni(mean_outdoor = 22)
#'
#' # Draw the Givoni strategy overlay for that outdoor mean.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         strategy = comfort_strategy_givoni(mean_outdoor = 22),
#'         show_labels = FALSE
#'     )
#'
#' @export
comfort_strategy_givoni <- function(mean_outdoor = 19, units = c("SI", "IP")) {
    units <- match.arg(units)
    mean_outdoor <- as.numeric(mean_outdoor)
    if (length(mean_outdoor) != 1L || !is.finite(mean_outdoor)) {
        stop(
            "`mean_outdoor` must be a single finite temperature.",
            call. = FALSE
        )
    }
    structure(
        list(mean_outdoor = mean_outdoor, units = units),
        class = c("PsyComfortGivoniStrategy", "list")
    )
}

#' Comfort zone style element
#'
#' `element_comfort_zone()` creates a small style object for comfort strategy
#' zones. It is used by `geom_comfort_givoni()` through the `zone_style`
#' argument to override Marsh-style defaults for individual zones.
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
#'         show_labels = FALSE,
#'         zone_style = list(
#'             comfort = element_comfort_zone(
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
#'         show_labels = FALSE,
#'         zone_style = list(
#'             air_conditioning = element_comfort_zone(
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
#'         show_labels = FALSE,
#'         zone_style = list(
#'             winter = element_comfort_zone(
#'                 colour = "#C44536",
#'                 linewidth = 1.2,
#'                 linetype = "dashed"
#'             )
#'         )
#'     )
#'
#' @export
element_comfort_zone <- function(
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

# Compute the Marsh/Givoni base dry-bulb temperature in SI units.
givoni__base_temp <- function(strategy) {
    mean_outdoor_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    # Marsh's Givoni chart shifts the comfort polygon from the mean outdoor
    # temperature; geometry is encoded in SI and converted at the output edge.
    round(17.6 + 0.31 * mean_outdoor_si - 3.5, 1L)
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
            "`zone_style` values must be created by element_comfort_zone(), ",
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
    base,
    pressure_pa,
    tdb_max_si,
    hum_min_gkg
) {
    if (zone %in% c("air_conditioning_dehumidification", "humidification")) {
        return(givoni__point(numeric(), numeric()))
    }
    # Zone vertices follow the Marsh/Givoni overlay in dry-bulb and RH terms.
    # Humidity caps keep upper edges from extending beyond the comfort maximum.
    hum20 <- function(tdb) givoni__hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) givoni__hum_gkg(tdb, 30, pressure_pa)
    hum50 <- function(tdb) givoni__hum_gkg(tdb, 50, pressure_pa)
    hum80 <- function(tdb) givoni__hum_gkg(tdb, 80, pressure_pa)
    hum100 <- function(tdb) givoni__hum_gkg(tdb, 100, pressure_pa)
    max_comfort_gkg <- min(16, hum80(base + 5))
    bottom20 <- hum20(base)
    evap_left <- base + 2.4528 * (bottom20 - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)

    parts <- switch(
        zone,
        comfort = list(
            givoni__rh_path(base, 80, base + 5, 80, pressure_pa, 16),
            givoni__point(
                base + 7,
                min(max_comfort_gkg, hum50(base + 7))
            ),
            givoni__point(base + 7, hum20(base + 7)),
            givoni__rh_path(base + 7, 20, base, 20, pressure_pa),
            givoni__point(base, hum80(base))
        ),
        natural_ventilation = list(
            givoni__rh_path(base, 100, base + 7, 100, pressure_pa),
            givoni__point(base + 12, hum50(base + 12)),
            givoni__point(base + 12, hum20(base + 12)),
            givoni__rh_path(base + 12, 20, base, 20, pressure_pa),
            givoni__point(base, hum100(base))
        ),
        internal_gains = list(
            givoni__rh_path(base - 2.5, 20, base - 7, 20, pressure_pa),
            givoni__point(base - 7.5, hum20(base - 7.5)),
            givoni__point(base - 7.5, min(hum80(base - 7.5), 16)),
            givoni__rh_path(
                base - 7.5,
                80,
                base - 2.5,
                80,
                pressure_pa,
                16
            )
        ),
        passive_solar_heating = list(
            givoni__point(base + 3.5, 0),
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
            givoni__point(base + 5, max_comfort_gkg),
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
            givoni__point(base, bottom20)
        ),
        mass_cooling = list(
            givoni__point(base + 5, max_comfort_gkg),
            givoni__point(base + 13, max_comfort_gkg),
            givoni__point(
                base + 17,
                min(max_comfort_gkg, hum30(base + 17))
            ),
            givoni__point(base + 17, min(max_comfort_gkg, hum20(base))),
            givoni__point(base, min(max_comfort_gkg, hum20(base)))
        ),
        mass_cooling_night_ventilation = list(
            givoni__point(base + 13, max_comfort_gkg),
            givoni__point(base + 20, max_comfort_gkg),
            givoni__point(
                base + 24,
                min(max_comfort_gkg, hum20(base + 24))
            ),
            givoni__point(base + 24, min(max_comfort_gkg, hum20(base))),
            givoni__point(base, min(max_comfort_gkg, hum20(base)))
        ),
        winter = list(
            givoni__rh_path(base - 0.5, 20, base - 2, 20, pressure_pa),
            givoni__point(base - 2, hum20(base - 2)),
            givoni__point(base - 2, min(hum80(base - 2), 16)),
            givoni__rh_path(
                base - 2,
                80,
                base - 0.5,
                80,
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
            givoni__point(base, hum20(base)),
            givoni__point(base - 12, hum20(base - 12))
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
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    base <- givoni__base_temp(strategy)
    tdb_max_si <- comfort_to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- unit__hum_from_chart(lim$hum[[1L]], units) * 1000

    pieces <- vector("list", length(zone))
    for (i in seq_along(zone)) {
        poly <- givoni__polygon(
            zone[[i]],
            base,
            pressure_pa,
            tdb_max_si,
            hum_min_gkg
        )
        if (!nrow(poly)) {
            next
        }
        spec <- specs[match(zone[[i]], specs$zone), , drop = FALSE]
        pieces[[i]] <- util__new_data_frame(list(
            tdb = comfort_from_si_temp(poly$tdb_si, units),
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
    psychro_output_xy(
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
    base,
    pressure_pa,
    tdb_max_si,
    hum_min_gkg
) {
    # Path labels reuse the same geometric primitives as zones so text follows
    # curved RH boundaries and vertical strategy lines consistently.
    max_comfort_gkg <- min(
        16,
        givoni__hum_gkg(base + 5, 80, pressure_pa)
    )
    hum20 <- function(tdb) givoni__hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) givoni__hum_gkg(tdb, 30, pressure_pa)
    hum80 <- function(tdb) givoni__hum_gkg(tdb, 80, pressure_pa)
    hum100 <- function(tdb) givoni__hum_gkg(tdb, 100, pressure_pa)
    evap_left <- base + 2.4528 * (hum20(base) - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)
    air_label_x <- min(right_air - 1.0, tdb_max_si - 0.8)
    air_label_x <- max(air_label_x, base + 21)

    paths <- list(
        givoni__label_path_entry(
            "natural_ventilation",
            "NATURAL VENTILATION",
            givoni__rh_path(base, 100, base + 7, 100, pressure_pa),
            hjust = 0.5,
            vjust = 1.8,
            group = 1L
        ),
        givoni__label_path_entry(
            "internal_gains",
            "INTERNAL GAINS",
            rbind(
                givoni__point(base - 7.5, min(hum80(base - 7.5), 16)),
                givoni__point(base - 7.5, hum20(base - 7.5))
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
                    min(max_comfort_gkg, hum20(base))
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
                    min(max_comfort_gkg, hum20(base + 24))
                ),
                givoni__point(
                    base + 24,
                    min(max_comfort_gkg, hum20(base))
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
                givoni__point(base - 2, min(hum80(base - 2), 16)),
                givoni__point(base - 2, hum20(base - 2))
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
    base,
    pressure_pa,
    tdb_max_si,
    hum_min_gkg = 0
) {
    max_comfort_gkg <- min(
        16,
        givoni__hum_gkg(base + 5, 80, pressure_pa)
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
            base + 3.5,
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
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    base <- givoni__base_temp(strategy)
    tdb_max_si <- comfort_to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- unit__hum_from_chart(lim$hum[[1L]], units) * 1000

    if (label_type == "path") {
        labels <- givoni__label_path_specs(
            base,
            pressure_pa,
            tdb_max_si,
            hum_min_gkg
        )
        out <- util__new_data_frame(list(
            tdb = comfort_from_si_temp(labels$tdb_si, units),
            humratio = labels$humratio,
            zone = labels$zone,
            label = labels$label,
            angle = labels$angle,
            hjust = labels$hjust,
            vjust = labels$vjust,
            group = labels$group
        ))
        out <- givoni__clip_humratio(out, lim$hum, units)
        return(psychro_output_xy(
            out,
            out$tdb,
            out$humratio,
            mollier,
            psychro_scales = psychro_scales,
            units = units
        ))
    }

    labels <- givoni__label_point_specs(
        base,
        pressure_pa,
        tdb_max_si,
        hum_min_gkg
    )
    out <- util__new_data_frame(list(
        tdb = comfort_from_si_temp(labels$tdb_si, units),
        humratio = labels$hum_gkg / 1000,
        zone = labels$zone,
        label = labels$label,
        angle = labels$angle,
        hjust = labels$hjust,
        vjust = labels$vjust,
        group = seq_len(nrow(labels))
    ))
    out <- givoni__clip_humratio(out, lim$hum, units)
    psychro_output_xy(
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
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    mean_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- unit__hum_from_chart(lim$hum, units)
    marker <- givoni__mean_outdoor_marker(
        mean_si,
        pressure_pa,
        hum_lim_narrow
    )
    if (is.null(marker)) {
        return(comfort_empty_contour())
    }
    out <- util__new_data_frame(list(
        tdb = comfort_from_si_temp(c(mean_si, mean_si), units),
        humratio = c(hum_lim_narrow[[1L]], marker$top),
        level = mean_si,
        group = 1L,
        metric = "givoni_mean_outdoor"
    ))
    psychro_output_xy(
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
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    mean_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- unit__hum_from_chart(lim$hum, units)
    marker <- givoni__mean_outdoor_marker(
        mean_si,
        pressure_pa,
        hum_lim_narrow
    )
    if (is.null(marker)) {
        return(givoni__empty_label())
    }

    label_temp <- comfort_from_si_temp(mean_si, units)
    unit_label <- if (units == "IP") "\u00b0F" else "\u00b0C"
    out <- util__new_data_frame(list(
        tdb = comfort_from_si_temp(mean_si, units),
        humratio = marker$label,
        zone = "mean_outdoor",
        label = sprintf("%.1f %s", label_temp, unit_label),
        angle = givoni__mean_outdoor_label_angle(mollier),
        hjust = 0.5,
        vjust = givoni__mean_outdoor_label_vjust(mollier),
        group = 1L
    ))
    psychro_output_xy(
        out,
        out$tdb,
        out$humratio,
        mollier,
        psychro_scales = psychro_scales,
        units = units
    )
}
