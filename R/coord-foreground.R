#' @include coord-psychro.R
NULL

# Render coord-owned foreground annotations after panel data and saturation.
coord_fg__extra_foreground <- function(coord, panel_params, theme) {
    foreground <- coord$comfort_foreground
    if (!length(foreground)) {
        return(grid::nullGrob())
    }

    grobs <- lapply(foreground, function(spec) {
        switch(
            spec$type,
            givoni_mean_outdoor = coord_fg__givoni_mean_outdoor_grob(
                coord,
                panel_params,
                spec
            ),
            heat_index_labels = coord_fg__heat_index_label_grob(
                coord,
                panel_params,
                spec
            ),
            grid::nullGrob()
        )
    })
    do.call(grid::grobTree, grobs)
}

# Draw the Givoni mean-outdoor marker in normalized panel coordinates.
coord_fg__givoni_mean_outdoor_grob <- function(coord, panel_params, spec) {
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)
    range_tdb_physical <- coord$range_tdb_physical(panel_params)
    range_hum_physical <- coord$range_hum_physical(panel_params)
    mean_si <- comfort_to_si_temp(
        spec$strategy$mean_outdoor,
        spec$strategy$units
    )
    tdb <- comfort_from_si_temp(mean_si, coord$units)
    if (
        !is.finite(tdb) ||
            tdb < range_tdb_physical[[1L]] ||
            tdb > range_tdb_physical[[2L]]
    ) {
        return(grid::nullGrob())
    }

    hum_sat <- psychrolib__with_units(
        coord$units,
        psychrolib::GetHumRatioFromRelHum(tdb, 1, coord$pressure)
    )
    if (!is.finite(hum_sat) || hum_sat >= range_hum_physical[[2L]]) {
        return(grid::nullGrob())
    }
    hum_extension <- max(
        diff(range_hum_physical) * 0.08,
        diff(range_hum_physical) / 25
    )
    hum_top <- min(range_hum_physical[[2L]], hum_sat + hum_extension)
    if (!is.finite(hum_top) || hum_top <= hum_sat) {
        return(grid::nullGrob())
    }
    hum_label <- min(hum_top, hum_sat + (hum_top - hum_sat) * 0.65)
    tdb_scaled <- coord$scale_tdb(panel_params, tdb)
    hum_scaled <- coord$scale_hum(panel_params, c(hum_sat, hum_top, hum_label))
    hum_sat_scaled <- hum_scaled[[1L]]
    hum_top_scaled <- hum_scaled[[2L]]
    hum_label_scaled <- hum_scaled[[3L]]

    if (coord$mollier) {
        line_x <- util__rescale01(c(hum_sat_scaled, hum_top_scaled), range_hum)
        line_y <- rep(util__rescale01(tdb_scaled, range_tdb), 2L)
        label_x <- util__rescale01(hum_label_scaled, range_hum)
        label_y <- line_y[[1L]]
        label_rot <- givoni__mean_outdoor_label_angle(TRUE)
        label_vjust <- givoni__mean_outdoor_label_vjust(TRUE)
    } else {
        line_x <- rep(util__rescale01(tdb_scaled, range_tdb), 2L)
        line_y <- util__rescale01(c(hum_sat_scaled, hum_top_scaled), range_hum)
        label_x <- line_x[[1L]]
        label_y <- util__rescale01(hum_label_scaled, range_hum)
        label_rot <- givoni__mean_outdoor_label_angle(FALSE)
        label_vjust <- givoni__mean_outdoor_label_vjust(FALSE)
    }

    label_temp <- comfort_from_si_temp(mean_si, coord$units)
    unit_label <- if (coord$units == "IP") "\u00b0F" else "\u00b0C"
    label <- sprintf("%.1f %s", label_temp, unit_label)
    colour <- spec$colour %||% "#444444"
    linewidth <- spec$linewidth %||% 0.8
    label_size <- spec$label_size %||% 2.7

    grid::grobTree(
        grid::linesGrob(
            x = line_x,
            y = line_y,
            gp = grid::gpar(
                col = colour,
                lwd = linewidth * ggplot2::.pt,
                lty = spec$linetype %||% "dotted"
            )
        ),
        if (isTRUE(spec$show_label)) {
            grid::textGrob(
                label,
                x = label_x,
                y = label_y,
                rot = label_rot,
                hjust = 0.5,
                vjust = label_vjust,
                gp = grid::gpar(
                    col = colour,
                    fontsize = label_size * ggplot2::.pt,
                    fontface = spec$fontface %||% "bold"
                )
            )
        } else {
            grid::nullGrob()
        }
    )
}

# Draw heat-index category labels after converting comfort data through coord.
coord_fg__heat_index_label_grob <- function(coord, panel_params, spec) {
    range_tdb <- coord$range_tdb_physical(panel_params)
    range_hum <- coord$range_hum_physical(panel_params)
    data <- heat_index__label_data(
        spec$model,
        comfort_grid_n(spec$n),
        coord$units,
        coord$pressure,
        coord$mollier,
        range_tdb,
        unit__hum_to_chart(range_hum, coord$units)
    )
    if (!nrow(data)) {
        return(grid::nullGrob())
    }

    data <- coord_psy__scale_xy(coord, panel_params, data)
    data <- coord$transform(data, panel_params)
    colour <- util__apply_alpha(spec$colour %||% "#444444", spec$alpha)
    grid::textGrob(
        data$label,
        x = data$x,
        y = data$y,
        rot = data$angle,
        hjust = spec$hjust %||% 0.5,
        vjust = spec$vjust %||% 0.5,
        gp = grid::gpar(
            col = colour,
            fontsize = (spec$size %||% 3) * ggplot2::.pt,
            fontfamily = spec$family %||% "",
            fontface = spec$fontface %||% "bold",
            lineheight = spec$lineheight %||% 1.2
        ),
        name = "psychro-heat-index-labels"
    )
}
