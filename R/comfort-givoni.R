#' @include comfort-api.R
NULL

# Givoni strategy geometry is fixed-shape chart construction rather than a
# continuous comfort model, so it lives outside the generic grid/contour helpers.
comfort_givoni_foreground_marker <- function(strategy, show_label, colour,
                                             linewidth, linetype, label_size,
                                             fontface) {
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
comfort_givoni_base_temp <- function(strategy) {
    mean_outdoor_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    # Marsh's Givoni chart shifts the comfort polygon from the mean outdoor
    # temperature; geometry is encoded in SI and converted at the output edge.
    round(17.6 + 0.31 * mean_outdoor_si - 3.5, 1L)
}

comfort_givoni_zone_specs <- function() {
    new_data_frame(list(
        zone = c(
            "comfort", "natural_ventilation", "internal_gains",
            "passive_solar_heating", "active_solar_heating",
            "evaporative_cooling", "mass_cooling",
            "mass_cooling_night_ventilation", "winter",
            "air_conditioning", "air_conditioning_dehumidification",
            "humidification"
        ),
        label = c(
            "COMFORT\nZONE", "NATURAL VENTILATION", "INTERNAL\nGAINS",
            "PASSIVE SOLAR\nHEATING", "ACTIVE\nSOLAR\nHEATING",
            "EVAPORATIVE COOLING", "MASS COOLING",
            "MASS COOLING &\nNIGHT VENTILATION", "WINTER",
            "AIR-CONDITIONING", "AIR-CONDITIONING &\nDEHUMIDIFICATION",
            "HUMIDIFICATION"
        ),
        fill = c(
            "#5BD96A", rep(NA_character_, 11L)
        ),
        linetype = c(
            "solid", "solid", "solid", "solid", "solid", "solid",
            "solid", "solid", "dashed", "longdash", "longdash", "solid"
        ),
        filled = c(TRUE, rep(FALSE, 11L)),
        draw_zone = c(
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            FALSE, FALSE
        )
    ))
}

comfort_zone_style_fields <- function() {
    c("fill", "colour", "color", "linewidth", "linetype", "alpha", "linejoin")
}

comfort_zone_fill_is_set <- function(fill) {
    !is.null(fill) && length(fill) == 1L && !is.na(fill)
}

comfort_givoni_check_zone_style <- function(zone_style, zone_names) {
    if (is.null(zone_style)) {
        return(list())
    }
    if (!is.list(zone_style) || is.null(names(zone_style)) ||
            any(!nzchar(names(zone_style)))) {
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

comfort_zone_style_to_params <- function(style) {
    if (inherits(style, "PsyComfortZoneElement") ||
            inherits(style, "ggplot2::element_polygon")) {
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
    unknown <- setdiff(names(out), comfort_zone_style_fields())
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

comfort_givoni_zone_params <- function(spec, params, zone_style, zone_alpha,
                                       na.rm, strategy) {
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
        style_params <- comfort_zone_style_to_params(style)
        out <- utils::modifyList(out, style_params)
        if (!("alpha" %in% names(style_params)) &&
                "fill" %in% names(style_params) &&
                comfort_zone_fill_is_set(style_params$fill)) {
            out$alpha <- zone_alpha
        }
    }
    out
}

comfort_givoni_empty_zone <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        zone = character(), label = character(), group = integer(),
        subgroup = integer()
    ))
}

comfort_givoni_empty_label <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        zone = character(), label = character(), angle = numeric(),
        hjust = numeric(), vjust = numeric(), group = integer()
    ))
}

comfort_givoni_humratio <- function(tdb_si, rh, pressure_pa) {
    with_units("SI", psychrolib::GetHumRatioFromRelHum(tdb_si, rh / 100, pressure_pa))
}

comfort_givoni_hum_gkg <- function(tdb_si, rh, pressure_pa) {
    comfort_givoni_humratio(tdb_si, rh, pressure_pa) * 1000
}

comfort_givoni_point <- function(tdb_si, hum_gkg) {
    new_data_frame(list(tdb_si = tdb_si, humratio = hum_gkg / 1000))
}

comfort_givoni_rh_path <- function(t0, rh0, t1, rh1, pressure_pa,
                                   max_gkg = Inf, n = NULL) {
    if (is.null(n)) {
        n <- max(8L, ceiling(abs(t1 - t0) * 4L) + 1L)
    }
    # Curved chart edges are specified in temperature/RH space, then converted
    # to humidity ratio so they follow psychrometric curvature on the plot.
    tdb <- seq(t0, t1, length.out = n)
    rh <- seq(rh0, rh1, length.out = n)
    hum_gkg <- pmin(
        comfort_givoni_hum_gkg(tdb, rh, pressure_pa),
        max_gkg
    )
    new_data_frame(list(tdb_si = tdb, humratio = hum_gkg / 1000))
}

comfort_givoni_polygon <- function(zone, base, pressure_pa, tdb_max_si,
                                   hum_min_gkg) {
    if (zone %in% c("air_conditioning_dehumidification", "humidification")) {
        return(comfort_givoni_point(numeric(), numeric()))
    }
    # Zone vertices follow the Marsh/Givoni overlay in dry-bulb and RH terms.
    # Humidity caps keep upper edges from extending beyond the comfort maximum.
    hum20 <- function(tdb) comfort_givoni_hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) comfort_givoni_hum_gkg(tdb, 30, pressure_pa)
    hum50 <- function(tdb) comfort_givoni_hum_gkg(tdb, 50, pressure_pa)
    hum80 <- function(tdb) comfort_givoni_hum_gkg(tdb, 80, pressure_pa)
    hum100 <- function(tdb) comfort_givoni_hum_gkg(tdb, 100, pressure_pa)
    max_comfort_gkg <- min(16, hum80(base + 5))
    bottom20 <- hum20(base)
    evap_left <- base + 2.4528 * (bottom20 - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)

    parts <- switch(zone,
        comfort = list(
            comfort_givoni_rh_path(base, 80, base + 5, 80, pressure_pa, 16),
            comfort_givoni_point(base + 7, min(max_comfort_gkg, hum50(base + 7))),
            comfort_givoni_point(base + 7, hum20(base + 7)),
            comfort_givoni_rh_path(base + 7, 20, base, 20, pressure_pa),
            comfort_givoni_point(base, hum80(base))
        ),
        natural_ventilation = list(
            comfort_givoni_rh_path(base, 100, base + 7, 100, pressure_pa),
            comfort_givoni_point(base + 12, hum50(base + 12)),
            comfort_givoni_point(base + 12, hum20(base + 12)),
            comfort_givoni_rh_path(base + 12, 20, base, 20, pressure_pa),
            comfort_givoni_point(base, hum100(base))
        ),
        internal_gains = list(
            comfort_givoni_rh_path(base - 2.5, 20, base - 7, 20, pressure_pa),
            comfort_givoni_point(base - 7.5, hum20(base - 7.5)),
            comfort_givoni_point(base - 7.5, min(hum80(base - 7.5), 16)),
            comfort_givoni_rh_path(base - 7.5, 80, base - 2.5, 80,
                pressure_pa, 16)
        ),
        passive_solar_heating = list(
            comfort_givoni_point(base + 3.5, 0),
            comfort_givoni_point(base - 12, 0),
            comfort_givoni_point(base - 12, hum100(base - 12)),
            comfort_givoni_rh_path(base - 12, 100, base - 1, 100, pressure_pa)
        ),
        active_solar_heating = list(
            comfort_givoni_point(base - 13, 0),
            comfort_givoni_point(base - 16, 0),
            comfort_givoni_point(base - 16, hum100(base - 16)),
            comfort_givoni_rh_path(base - 16, 100, base - 13, 100, pressure_pa)
        ),
        evaporative_cooling = list(
            comfort_givoni_point(base + 5, max_comfort_gkg),
            comfort_givoni_point(base + 16, min(max_comfort_gkg, hum30(base + 16))),
            comfort_givoni_point(base + 19, min(max_comfort_gkg, hum20(base + 19))),
            comfort_givoni_point(base + 21, min(max_comfort_gkg,
                comfort_givoni_hum_gkg(base + 21, 10, pressure_pa))),
            comfort_givoni_point(base + 21, 0),
            comfort_givoni_point(evap_left, 0),
            comfort_givoni_point(base, bottom20)
        ),
        mass_cooling = list(
            comfort_givoni_point(base + 5, max_comfort_gkg),
            comfort_givoni_point(base + 13, max_comfort_gkg),
            comfort_givoni_point(base + 17, min(max_comfort_gkg, hum30(base + 17))),
            comfort_givoni_point(base + 17, min(max_comfort_gkg, hum20(base))),
            comfort_givoni_point(base, min(max_comfort_gkg, hum20(base)))
        ),
        mass_cooling_night_ventilation = list(
            comfort_givoni_point(base + 13, max_comfort_gkg),
            comfort_givoni_point(base + 20, max_comfort_gkg),
            comfort_givoni_point(base + 24, min(max_comfort_gkg, hum20(base + 24))),
            comfort_givoni_point(base + 24, min(max_comfort_gkg, hum20(base))),
            comfort_givoni_point(base, min(max_comfort_gkg, hum20(base)))
        ),
        winter = list(
            comfort_givoni_rh_path(base - 0.5, 20, base - 2, 20, pressure_pa),
            comfort_givoni_point(base - 2, hum20(base - 2)),
            comfort_givoni_point(base - 2, min(hum80(base - 2), 16)),
            comfort_givoni_rh_path(base - 2, 80, base - 0.5, 80,
                pressure_pa, 16)
        ),
        air_conditioning = list(
            comfort_givoni_point(base + 20, max_comfort_gkg),
            comfort_givoni_point(right_air, max_comfort_gkg),
            comfort_givoni_point(right_air, 0),
            comfort_givoni_point(base + 21, 0)
        ),
        air_conditioning_dehumidification = list(
            comfort_givoni_point(base + 20, max_comfort_gkg),
            comfort_givoni_point(right_air, max_comfort_gkg),
            comfort_givoni_point(right_air, 30),
            comfort_givoni_point(base + 20, 30)
        ),
        humidification = list(
            comfort_givoni_point(base - 12, 0),
            comfort_givoni_point(base, 0),
            comfort_givoni_point(base, hum20(base)),
            comfort_givoni_point(base - 12, hum20(base - 12))
        ),
        stop("Unknown Givoni zone: ", zone, call. = FALSE)
    )

    out <- do.call(rbind, parts)
    row.names(out) <- NULL
    out
}

comfort_givoni_zone_data <- function(strategy, zone, units, pres, mollier,
                                     tdb_lim, hum_lim) {
    strategy <- comfort_check_givoni_strategy(strategy)
    specs <- comfort_givoni_zone_specs()
    if (is.null(zone)) {
        zone <- specs$zone[specs$draw_zone]
    }
    zone <- match.arg(zone, specs$zone, several.ok = TRUE)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    base <- comfort_givoni_base_temp(strategy)
    tdb_max_si <- comfort_to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- narrow_hum(lim$hum[[1L]], units) * 1000

    pieces <- vector("list", length(zone))
    for (i in seq_along(zone)) {
        poly <- comfort_givoni_polygon(zone[[i]], base, pressure_pa,
            tdb_max_si, hum_min_gkg)
        if (!nrow(poly)) {
            next
        }
        spec <- specs[match(zone[[i]], specs$zone), , drop = FALSE]
        pieces[[i]] <- new_data_frame(list(
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
        return(comfort_givoni_empty_zone())
    }
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
    out <- comfort_givoni_clip_humratio(out, lim$hum, units)
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_givoni_label_path_entry <- function(zone, label, path,
                                            hjust = 0.5, vjust = 0.5,
                                            group = 1L) {
    if (!nrow(path)) {
        return(comfort_givoni_empty_label())
    }
    new_data_frame(list(
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

comfort_givoni_label_path_specs <- function(base, pressure_pa, tdb_max_si,
                                            hum_min_gkg) {
    # Path labels reuse the same geometric primitives as zones so text follows
    # curved RH boundaries and vertical strategy lines consistently.
    max_comfort_gkg <- min(16,
        comfort_givoni_hum_gkg(base + 5, 80, pressure_pa)
    )
    hum20 <- function(tdb) comfort_givoni_hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) comfort_givoni_hum_gkg(tdb, 30, pressure_pa)
    hum80 <- function(tdb) comfort_givoni_hum_gkg(tdb, 80, pressure_pa)
    hum100 <- function(tdb) comfort_givoni_hum_gkg(tdb, 100, pressure_pa)
    evap_left <- base + 2.4528 * (hum20(base) - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)
    air_label_x <- min(right_air - 1.0, tdb_max_si - 0.8)
    air_label_x <- max(air_label_x, base + 21)

    paths <- list(
        comfort_givoni_label_path_entry(
            "natural_ventilation", "NATURAL VENTILATION",
            comfort_givoni_rh_path(base, 100, base + 7, 100, pressure_pa),
            hjust = 0.5, vjust = 1.8, group = 1L
        ),
        comfort_givoni_label_path_entry(
            "internal_gains", "INTERNAL GAINS",
            rbind(
                comfort_givoni_point(base - 7.5,
                    min(hum80(base - 7.5), 16)),
                comfort_givoni_point(base - 7.5, hum20(base - 7.5))
            ),
            hjust = 0.5, vjust = -0.25, group = 2L
        ),
        comfort_givoni_label_path_entry(
            "passive_solar_heating", "PASSIVE SOLAR",
            rbind(
                comfort_givoni_point(base - 12, hum100(base - 12)),
                comfort_givoni_point(base - 12, 0)
            ),
            hjust = 0.5, vjust = -0.25, group = 3L
        ),
        comfort_givoni_label_path_entry(
            "active_solar_heating", "ACTIVE SOLAR",
            rbind(
                comfort_givoni_point(base - 16, hum100(base - 16)),
                comfort_givoni_point(base - 16, 0)
            ),
            hjust = 0.5, vjust = -0.25, group = 4L
        ),
        comfort_givoni_label_path_entry(
            "evaporative_cooling", "EVAPORATIVE COOLING",
            rbind(
                comfort_givoni_point(evap_left, 0),
                comfort_givoni_point(base + 21, 0)
            ),
            hjust = 0.68, vjust = -0.35, group = 5L
        ),
        comfort_givoni_label_path_entry(
            "mass_cooling", "MASS COOLING",
            rbind(
                comfort_givoni_point(base + 17,
                    min(max_comfort_gkg, hum30(base + 17))),
                comfort_givoni_point(base + 17,
                    min(max_comfort_gkg, hum20(base)))
            ),
            hjust = 0.48, vjust = -0.25, group = 6L
        ),
        comfort_givoni_label_path_entry(
            "mass_cooling_night_ventilation",
            "MASS COOLING &\nNIGHT VENTILATION",
            rbind(
                comfort_givoni_point(base + 24,
                    min(max_comfort_gkg, hum20(base + 24))),
                comfort_givoni_point(base + 24,
                    min(max_comfort_gkg, hum20(base)))
            ),
            hjust = 0.5, vjust = -0.25, group = 7L
        ),
        comfort_givoni_label_path_entry(
            "winter", "WINTER",
            rbind(
                comfort_givoni_point(base - 2, min(hum80(base - 2), 16)),
                comfort_givoni_point(base - 2, hum20(base - 2))
            ),
            hjust = 0.5, vjust = -0.25, group = 8L
        ),
        comfort_givoni_label_path_entry(
            "air_conditioning", "AIR-CONDITIONING",
            rbind(
                comfort_givoni_point(air_label_x, max_comfort_gkg),
                comfort_givoni_point(air_label_x, 0)
            ),
            hjust = 0.5, vjust = -0.25, group = 9L
        ),
        comfort_givoni_label_path_entry(
            "humidification", "HUMIDIFICATION",
            rbind(
                comfort_givoni_point(base - 12, 0),
                comfort_givoni_point(base, 0)
            ),
            hjust = 0.5, vjust = -0.35, group = 10L
        )
    )
    out <- do.call(rbind, paths)
    row.names(out) <- NULL
    out
}

comfort_givoni_label_point_specs <- function(base, pressure_pa, tdb_max_si,
                                             hum_min_gkg = 0) {
    max_comfort_gkg <- min(16,
        comfort_givoni_hum_gkg(base + 5, 80, pressure_pa)
    )
    heating_x <- base - 18
    heating_hum_gkg <- mean(c(
        hum_min_gkg,
        comfort_givoni_hum_gkg(heating_x, 100, pressure_pa)
    ))
    new_data_frame(list(
        zone = c(
            "comfort", "heating", "air_conditioning_dehumidification"
        ),
        label = c(
            "COMFORT\nZONE", "HEATING",
            "AIR-CONDITIONING &\nDEHUMIDIFICATION"
        ),
        tdb_si = c(
            base + 3.5, base - 18, base + 19
        ),
        hum_gkg = c(
            max_comfort_gkg * 0.55, heating_hum_gkg, max_comfort_gkg + 6
        ),
        angle = c(0, 270, 0),
        hjust = c(0.5, 0.5, 0.5),
        vjust = c(0.5, 0.5, 0.5)
    ))
}

comfort_givoni_label_data <- function(strategy, label_type, units, pres,
                                      mollier, tdb_lim, hum_lim) {
    label_type <- match.arg(label_type, c("path", "point"))
    strategy <- comfort_check_givoni_strategy(strategy)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    base <- comfort_givoni_base_temp(strategy)
    tdb_max_si <- comfort_to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- narrow_hum(lim$hum[[1L]], units) * 1000

    if (label_type == "path") {
        labels <- comfort_givoni_label_path_specs(
            base, pressure_pa, tdb_max_si, hum_min_gkg
        )
        out <- new_data_frame(list(
            tdb = comfort_from_si_temp(labels$tdb_si, units),
            humratio = labels$humratio,
            zone = labels$zone,
            label = labels$label,
            angle = labels$angle,
            hjust = labels$hjust,
            vjust = labels$vjust,
            group = labels$group
        ))
        out <- comfort_givoni_clip_humratio(out, lim$hum, units)
        return(psychro_output_xy(out, out$tdb, out$humratio, mollier))
    }

    labels <- comfort_givoni_label_point_specs(
        base, pressure_pa, tdb_max_si, hum_min_gkg
    )
    out <- new_data_frame(list(
        tdb = comfort_from_si_temp(labels$tdb_si, units),
        humratio = labels$hum_gkg / 1000,
        zone = labels$zone,
        label = labels$label,
        angle = labels$angle,
        hjust = labels$hjust,
        vjust = labels$vjust,
        group = seq_len(nrow(labels))
    ))
    out <- comfort_givoni_clip_humratio(out, lim$hum, units)
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_givoni_clip_humratio <- function(data, hum_lim, units) {
    hum_lim <- narrow_hum(hum_lim, units)
    data$humratio <- pmin(pmax(data$humratio, hum_lim[[1L]]), hum_lim[[2L]])
    data
}

comfort_givoni_mean_outdoor_label_angle <- function(mollier) {
    if (isTRUE(mollier)) 0 else 270
}

comfort_givoni_mean_outdoor_label_vjust <- function(mollier) {
    if (isTRUE(mollier)) 1.25 else -0.25
}

comfort_givoni_mean_outdoor_marker <- function(mean_si, pressure_pa,
                                               hum_lim_narrow) {
    # The mean-outdoor marker runs from the visible lower humidity limit to just
    # past saturation, leaving a short extension for the numeric label.
    hum_sat <- comfort_givoni_humratio(mean_si, 100, pressure_pa)
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

comfort_givoni_mean_outdoor_data <- function(strategy, units, pres, mollier,
                                             tdb_lim, hum_lim) {
    strategy <- comfort_check_givoni_strategy(strategy)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    mean_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- narrow_hum(lim$hum, units)
    marker <- comfort_givoni_mean_outdoor_marker(
        mean_si, pressure_pa, hum_lim_narrow
    )
    if (is.null(marker)) {
        return(comfort_empty_contour())
    }
    out <- new_data_frame(list(
        tdb = comfort_from_si_temp(c(mean_si, mean_si), units),
        humratio = c(hum_lim_narrow[[1L]], marker$top),
        level = mean_si,
        group = 1L,
        metric = "givoni_mean_outdoor"
    ))
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_givoni_mean_outdoor_label_data <- function(strategy, units, pres,
                                                   mollier, tdb_lim,
                                                   hum_lim) {
    strategy <- comfort_check_givoni_strategy(strategy)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    mean_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- narrow_hum(lim$hum, units)
    marker <- comfort_givoni_mean_outdoor_marker(
        mean_si, pressure_pa, hum_lim_narrow
    )
    if (is.null(marker)) {
        return(comfort_givoni_empty_label())
    }

    label_temp <- comfort_from_si_temp(mean_si, units)
    unit_label <- if (units == "IP") "\u00b0F" else "\u00b0C"
    out <- new_data_frame(list(
        tdb = comfort_from_si_temp(mean_si, units),
        humratio = marker$label,
        zone = "mean_outdoor",
        label = sprintf("%.1f %s", label_temp, unit_label),
        angle = comfort_givoni_mean_outdoor_label_angle(mollier),
        hjust = 0.5,
        vjust = comfort_givoni_mean_outdoor_label_vjust(mollier),
        group = 1L
    ))
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}
