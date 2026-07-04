#' @include utils.R
NULL

# Tile-local cell grid helpers draw optional grid lines inside
# geom_psychro_tile() output.
# Draw the tile-local cell grid as ggplot2 segment grobs.
psychro_tile_cell_grid_grob <- function(
    data,
    panel_params,
    coord,
    theme,
    colour,
    linewidth,
    linetype,
    alpha,
    lineend,
    linejoin
) {
    segments <- psychro_tile_cell_grid_data(
        data,
        panel_params,
        coord,
        theme,
        colour,
        linewidth,
        linetype,
        alpha
    )
    if (!nrow(segments)) {
        return(grid::nullGrob())
    }

    ggplot2::GeomSegment$draw_panel(
        segments,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        na.rm = TRUE
    )
}

# Build styled cell-grid segment data for the current panel.
psychro_tile_cell_grid_data <- function(
    data,
    panel_params,
    coord,
    theme,
    colour,
    linewidth,
    linetype,
    alpha
) {
    segments <- psychro_tile_cell_segments(data, panel_params, coord)
    if (!nrow(segments)) {
        return(segments)
    }

    theme <- theme %||% coord$psychro_theme %||% ggplot2::theme_get()
    styles <- lapply(seq_len(nrow(segments)), function(i) {
        psychro_tile_cell_grid_style(
            theme,
            segments$axis[[i]],
            segments$grid_type[[i]],
            colour,
            linewidth,
            linetype,
            alpha
        )
    })
    keep <- vapply(styles, `[[`, logical(1L), "visible")
    segments <- segments[keep, , drop = FALSE]
    if (!nrow(segments)) {
        return(segments)
    }
    styles <- styles[keep]

    segments$colour <- vapply(styles, `[[`, character(1L), "colour")
    segments$linewidth <- vapply(styles, `[[`, numeric(1L), "linewidth")
    segments$linetype <- unlist(
        lapply(styles, `[[`, "linetype"),
        use.names = FALSE
    )
    segments$alpha <- vapply(styles, `[[`, numeric(1L), "alpha")
    segments$group <- seq_len(nrow(segments))
    segments
}

# Resolve one tile grid style from theme inheritance and explicit overrides.
psychro_tile_cell_grid_style <- function(
    theme,
    axis,
    type,
    colour,
    linewidth,
    linetype,
    alpha
) {
    element <- ggplot2::calc_element(
        paste("panel.grid", type, axis, sep = "."),
        theme
    )
    has_override <- !util__is_waive(colour) ||
        !util__is_waive(linewidth) ||
        !util__is_waive(linetype) ||
        !util__is_waive(alpha)
    is_blank <- is.null(element) || inherits(element, "element_blank")
    defaults <- list(
        colour = "grey78",
        linewidth = 0.25,
        linetype = 1,
        alpha = NA_real_
    )

    list(
        visible = !is_blank || has_override,
        colour = psychro_tile_cell_grid_value(
            colour,
            if (!is_blank) element$colour else NULL,
            defaults$colour
        ),
        linewidth = psychro_tile_cell_grid_value(
            linewidth,
            if (!is_blank) element$linewidth else NULL,
            defaults$linewidth
        ),
        linetype = psychro_tile_cell_grid_value(
            linetype,
            if (!is_blank) element$linetype else NULL,
            defaults$linetype
        ),
        alpha = psychro_tile_cell_grid_value(
            alpha,
            if (!is_blank) element$alpha else NULL,
            defaults$alpha
        )
    )
}

# Prefer explicit grid style values, then inherited theme values, then defaults.
psychro_tile_cell_grid_value <- function(value, inherited, default) {
    if (!util__is_waive(value)) {
        return(value)
    }

    inherited %||% default
}

# Build tile-local grid segments from cell bounds and panel breaks.
psychro_tile_cell_segments <- function(data, panel_params, coord) {
    needed <- c("cell_xmin", "cell_xmax", "cell_ymin", "cell_ymax")
    if (!nrow(data) || !all(needed %in% names(data)) || is.null(panel_params)) {
        return(util__new_data_frame(list(
            x = numeric(),
            y = numeric(),
            xend = numeric(),
            yend = numeric()
        )))
    }

    x_spacing <- psychro_tile_cell_spacing(data$cell_xmin, data$cell_xmax)
    y_spacing <- psychro_tile_cell_spacing(data$cell_ymin, data$cell_ymax)
    x_breaks <- psychro_tile_cell_grid_breaks(panel_params, "x", x_spacing)
    y_breaks <- psychro_tile_cell_grid_breaks(panel_params, "y", y_spacing)
    if (
        (!nrow(x_breaks) && is.null(x_spacing)) ||
            (!nrow(y_breaks) && is.null(y_spacing))
    ) {
        return(util__new_data_frame(list(
            x = numeric(),
            y = numeric(),
            xend = numeric(),
            yend = numeric()
        )))
    }

    ranges <- psychro_tile_panel_ranges(panel_params)
    if (!nrow(x_breaks)) {
        x_breaks <- psychro_tile_spacing_breaks(
            ranges$x,
            x_spacing$width,
            x_spacing$anchor
        )
    }
    if (!nrow(y_breaks)) {
        y_breaks <- psychro_tile_spacing_breaks(
            ranges$y,
            y_spacing$width,
            y_spacing$anchor
        )
    }

    psychro_tile_chart_segments(x_breaks, y_breaks, ranges$x, ranges$y, coord)
}

# Extract trained major/minor grid breaks for one panel axis.
psychro_tile_panel_grid_breaks <- function(
    panel_params,
    axis,
    tolerance = 1e-8
) {
    scale <- panel_params[[axis]]
    if (is.null(scale)) {
        return(psychro_tile_break_data(numeric(), character()))
    }

    ranges <- psychro_tile_panel_ranges(panel_params)[[axis]]
    major <- psychro_tile_break_values(scale$breaks)
    minor <- psychro_tile_break_values(scale$minor_breaks)
    major <- major[
        major >= ranges[[1L]] - tolerance &
            major <= ranges[[2L]] + tolerance
    ]
    minor <- minor[
        minor >= ranges[[1L]] - tolerance &
            minor <= ranges[[2L]] + tolerance
    ]

    major_key <- round(major, 12L)
    minor <- minor[!round(minor, 12L) %in% major_key]

    psychro_tile_break_data(
        c(minor, major),
        c(
            rep("minor", length(minor)),
            rep("major", length(major))
        )
    )
}

# Merge tile spacing breaks with existing panel grid breaks when aligned.
psychro_tile_cell_grid_breaks <- function(
    panel_params,
    axis,
    spacing,
    tolerance = 1e-8
) {
    breaks <- psychro_tile_panel_grid_breaks(panel_params, axis)
    if (is.null(spacing)) {
        return(breaks)
    }

    ranges <- psychro_tile_panel_ranges(panel_params)[[axis]]
    if (!nrow(breaks)) {
        return(psychro_tile_spacing_breaks(
            ranges,
            spacing$width,
            spacing$anchor
        ))
    }

    values <- unique(breaks$value)
    if (length(values) < 2L) {
        return(psychro_tile_merge_breaks(
            psychro_tile_grid_breaks(ranges, spacing$width, min(values)),
            breaks
        ))
    }

    step <- min(diff(sort(values)), na.rm = TRUE)
    if (!is.finite(step) || spacing$width >= step - tolerance) {
        return(breaks)
    }

    anchor <- min(values)
    if (!psychro_tile_breaks_are_aligned(values, spacing$width, anchor)) {
        return(breaks)
    }

    psychro_tile_merge_breaks(
        psychro_tile_grid_breaks(ranges, spacing$width, anchor),
        breaks
    )
}

# Check whether existing breaks align with tile spacing.
psychro_tile_breaks_are_aligned <- function(
    values,
    width,
    anchor,
    tolerance = 1e-8
) {
    steps <- (values - anchor) / width
    all(abs(steps - round(steps)) <= tolerance)
}

# Merge generated tile breaks with existing major/minor break metadata.
psychro_tile_merge_breaks <- function(values, existing) {
    out <- psychro_tile_break_data(values, rep("minor", length(values)))
    if (!nrow(out) || !nrow(existing)) {
        return(out)
    }

    out_key <- round(out$value, 12L)
    existing_key <- round(existing$value, 12L)
    minor <- match(existing_key[existing$type == "minor"], out_key)
    major <- match(existing_key[existing$type == "major"], out_key)
    out$type[minor[!is.na(minor)]] <- "minor"
    out$type[major[!is.na(major)]] <- "major"
    out
}

# Extract finite numeric break values from ggplot2 scale break containers.
psychro_tile_break_values <- function(x) {
    x <- unlist(x, use.names = FALSE)
    x[is.finite(x)]
}

# Build sorted break metadata used by cell-grid segment construction.
psychro_tile_break_data <- function(value, type) {
    if (!length(value)) {
        return(util__new_data_frame(list(
            value = numeric(),
            type = character()
        )))
    }

    order <- order(value)
    util__new_data_frame(list(value = value[order], type = type[order]))
}

# Infer a single tile spacing and anchor from cell bounds.
psychro_tile_cell_spacing <- function(lower, upper, tolerance = 1e-8) {
    widths <- unique(round(upper - lower, 12L))
    widths <- widths[is.finite(widths) & widths > tolerance]
    if (!length(widths)) {
        return(NULL)
    }

    list(width = widths[[1L]], anchor = min(c(lower, upper), na.rm = TRUE))
}

# Resolve panel x/y ranges across ggplot2 panel-params variants.
psychro_tile_panel_ranges <- function(panel_params) {
    x_range <- panel_params$x.range
    y_range <- panel_params$y.range

    if (is.null(x_range)) {
        x_range <- panel_params$x$continuous_range
    }
    if (is.null(y_range)) {
        y_range <- panel_params$y$continuous_range
    }

    list(x = x_range, y = y_range)
}

# Generate evenly spaced breaks inside a panel range.
psychro_tile_grid_breaks <- function(range, width, anchor, tolerance = 1e-8) {
    lower <- ceiling((range[[1L]] - anchor) / width - tolerance) *
        width +
        anchor
    upper <- floor((range[[2L]] - anchor) / width + tolerance) * width + anchor

    if (lower > upper) {
        return(numeric())
    }

    seq(lower, upper, by = width)
}

# Generate major breaks when no trained panel breaks are available.
psychro_tile_spacing_breaks <- function(range, width, anchor) {
    breaks <- psychro_tile_grid_breaks(range, width, anchor)
    psychro_tile_break_data(breaks, rep("major", length(breaks)))
}

# Convert x/y break metadata into psychrometric chart grid segments.
psychro_tile_chart_segments <- function(
    x_breaks,
    y_breaks,
    x_range,
    y_range,
    coord
) {
    if (!nrow(x_breaks) && !nrow(y_breaks)) {
        return(util__new_data_frame(list(
            x = numeric(),
            y = numeric(),
            xend = numeric(),
            yend = numeric()
        )))
    }

    if (
        isTRUE(coord$mollier) || is.null(coord$units) || is.null(coord$pressure)
    ) {
        return(psychro_tile_rectangular_segments(
            x_breaks,
            y_breaks,
            x_range,
            y_range
        ))
    }

    x_sat <- psychrolib__with_units(
        coord$units,
        psychrolib::GetHumRatioFromRelHum(x_breaks$value, 1.0, coord$pressure)
    )
    vertical <- util__new_data_frame(list(
        x = x_breaks$value,
        y = y_range[[1L]],
        xend = x_breaks$value,
        yend = pmin(y_range[[2L]], x_sat),
        axis = "x",
        grid_type = x_breaks$type
    ))
    vertical <- vertical[vertical$yend >= vertical$y, , drop = FALSE]

    y_dew <- rep(x_range[[1L]], nrow(y_breaks))
    positive <- y_breaks$value > 0
    if (any(positive)) {
        y_dew[positive] <- psychrolib__with_units(
            coord$units,
            GetTDewPointFromHumRatioOnly(
                y_breaks$value[positive],
                coord$pressure
            )
        )
    }
    horizontal <- util__new_data_frame(list(
        x = pmax(x_range[[1L]], y_dew),
        y = y_breaks$value,
        xend = x_range[[2L]],
        yend = y_breaks$value,
        axis = "y",
        grid_type = y_breaks$type
    ))
    horizontal <- horizontal[horizontal$x <= horizontal$xend, , drop = FALSE]

    unique(rbind(vertical, horizontal))
}

# Convert x/y break metadata into rectangular fallback grid segments.
psychro_tile_rectangular_segments <- function(
    x_breaks,
    y_breaks,
    x_range,
    y_range
) {
    vertical <- util__new_data_frame(list(
        x = x_breaks$value,
        y = y_range[[1L]],
        xend = x_breaks$value,
        yend = y_range[[2L]],
        axis = "x",
        grid_type = x_breaks$type
    ))
    horizontal <- util__new_data_frame(list(
        x = x_range[[1L]],
        y = y_breaks$value,
        xend = x_range[[2L]],
        yend = y_breaks$value,
        axis = "y",
        grid_type = y_breaks$type
    ))

    unique(rbind(vertical, horizontal))
}
