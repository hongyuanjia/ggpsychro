#' @include utils.R ggproto-classes.R stat.R
NULL

#' Bin data on psychrometric chart coordinates
#'
#' `stat_psychro_bin()` bins observations on dry-bulb temperature and humidity
#' ratio coordinates. `geom_psychro_tile()` draws the result as tiles, which is
#' useful for weather-hour distributions and gridded simulation summaries.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @param bins Number of bins in the dry-bulb and humidity-ratio directions.
#'   A single number is recycled to both directions. Ignored when `binwidth` is
#'   supplied.
#' @param binwidth Width of bins in chart display units. The first value is in
#'   dry-bulb temperature units, and the second value is in humidity-ratio units
#'   (`g/kg` for SI and `gr/lb` for IP). A single number is recycled to both
#'   directions.
#' @param boundary Bin boundary in chart display units. The first value is a
#'   dry-bulb temperature boundary, and the second value is a humidity-ratio
#'   boundary (`g/kg` for SI and `gr/lb` for IP). A single number is recycled
#'   to both directions. Only used when `binwidth` is supplied.
#' @param drop If `TRUE`, the default, omit empty bins.
#' @param fun Summary function used when the `value` aesthetic is supplied.
#'   One of `"sum"`, `"mean"`, `"median"`, `"min"`, or `"max"`.
#' @param gap Relative gap between adjacent tiles. The default, `0.08`, draws
#'   tiles at 92% of the bin width and height. Use `gap = 0` for full-size
#'   tiles. Must be a single finite number greater than or equal to 0 and less
#'   than 1.
#' @param cell.grid If `TRUE`, the default, draw a tile-local grid across the
#'   chart area. The grid uses the current x/y scale major and minor breaks by
#'   default. If `binwidth` is finer than, and aligned with, those scale breaks,
#'   the cell grid uses the finer bin spacing while preserving the existing x/y
#'   breaks as grid lines. If scale breaks are unavailable, the grid falls back
#'   to the computed bin spacing.
#' @param cell.grid.colour,cell.grid.linewidth,cell.grid.linetype,cell.grid.alpha
#'   Appearance of the tile-local cell grid. The default, [ggplot2::waiver()],
#'   inherits from the current `panel.grid.*.x` and `panel.grid.*.y` theme
#'   elements. Explicit values override the inherited theme style.
#'
#' @details
#' The stat accepts either `x` and `y` aesthetics, where `y` is humidity ratio
#' in chart display units, or `x` and `relhum`, where `relhum` is relative
#' humidity in percent. Relative humidity inputs inherit the plot unit system
#' and pressure from [ggpsychro()]. Tiles default to a small gap and `alpha =
#' 0.85` so psychrometric grid lines remain visible. Tile bodies are clipped to
#' the saturation line in psychrometric coordinates. When `binwidth` is used,
#' each tile represents one dry-bulb and humidity-ratio cell aligned to
#' `boundary`. The optional cell grid follows the chart's x/y breaks so it stays
#' aligned with the visible dry-bulb and humidity-ratio grid. Choose a
#' `binwidth` that evenly subdivides those breaks when a denser Marsh-style cell
#' grid should still coincide with the existing x/y grid.
#'
#' @section Computed variables:
#' * `count`: number of observations in each tile.
#' * `hours`: same as `count`, named for hourly weather data.
#' * `value`: aggregated `value` aesthetic when supplied.
#' * `width`, `height`: tile dimensions after applying `gap`.
#' * `cell_xmin`, `cell_xmax`, `cell_ymin`, `cell_ymax`: full bin boundaries.
#'
#' @examples
#' d <- data.frame(
#'     dry_bulb = c(20.1, 20.4, 22.2, 22.5),
#'     relative_humidity = c(50, 52, 60, 62),
#'     cooling_load = c(1.2, 1.6, 3.4, 4.2)
#' )
#'
#' ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
#'     geom_psychro_tile(
#'         aes(dry_bulb, relhum = relative_humidity, fill = after_stat(hours)),
#'         binwidth = c(2, 2)
#'     )
#'
#' ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
#'     stat_psychro_bin(
#'         aes(dry_bulb, relhum = relative_humidity, fill = after_stat(hours)),
#'         binwidth = c(2, 2)
#'     )
#'
#' ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
#'     geom_psychro_tile(
#'         aes(dry_bulb, relhum = relative_humidity, value = cooling_load,
#'             fill = after_stat(value)),
#'         binwidth = c(2, 2),
#'         fun = "mean"
#'     )
#'
#' @rdname stat_psychro_bin
#' @importFrom ggplot2 ggproto Stat after_stat
#' @export
stat_psychro_bin <- function(mapping = NULL, data = NULL, geom = "tile",
                             position = "identity", ..., bins = 30,
                             binwidth = NULL, boundary = c(0, 0),
                             drop = TRUE, fun = "sum",
                             gap = 0.08,
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {
    if (identical(geom, "tile")) {
        geom <- GeomPsychroTile
    }

    psychro_layer(
        stat = StatPsychroBin, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm, bins = bins, binwidth = binwidth,
            boundary = boundary, drop = drop, fun = fun, gap = gap, ...
        )
    )
}

#' @rdname stat_psychro_bin
#' @export
geom_psychro_tile <- function(mapping = NULL, data = NULL, stat = "psychro_bin",
                              position = "identity", ..., gap = 0.08,
                              boundary = c(0, 0), cell.grid = TRUE,
                              cell.grid.colour = ggplot2::waiver(),
                              cell.grid.linewidth = ggplot2::waiver(),
                              cell.grid.linetype = ggplot2::waiver(),
                              cell.grid.alpha = ggplot2::waiver(),
                              na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
    params <- list(
        na.rm = na.rm,
        cell.grid = cell.grid,
        cell.grid.colour = cell.grid.colour,
        cell.grid.linewidth = cell.grid.linewidth,
        cell.grid.linetype = cell.grid.linetype,
        cell.grid.alpha = cell.grid.alpha,
        ...
    )
    if (identical(stat, "psychro_bin") || identical(stat, StatPsychroBin)) {
        params$gap <- gap
        params$boundary <- boundary
    }

    psychro_layer(
        data = data, mapping = mapping, stat = stat, geom = GeomPsychroTile,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = params
    )
}

GeomPsychroTile <- ggproto(
    "GeomPsychroTile", ggplot2::GeomTile,
    extra_params = c(
        "na.rm", "cell.grid", "cell.grid.colour", "cell.grid.linewidth",
        "cell.grid.linetype", "cell.grid.alpha", "psychro.theme"
    ),
    default_aes = utils::modifyList(
        ggplot2::GeomTile$default_aes,
        ggplot2::aes(alpha = 0.85)
    ),
    draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                          linejoin = "mitre", cell.grid = TRUE,
                          cell.grid.colour = ggplot2::waiver(),
                          cell.grid.linewidth = ggplot2::waiver(),
                          cell.grid.linetype = ggplot2::waiver(),
                          cell.grid.alpha = ggplot2::waiver(),
                          psychro.theme = NULL) {
        tiles <- psychro_tile_grob(
            data, panel_params, coord, lineend = lineend, linejoin = linejoin
        )

        if (!isTRUE(cell.grid)) {
            return(tiles)
        }

        cell_grid <- psychro_tile_cell_grid_grob(
            data, panel_params, coord,
            theme = psychro.theme,
            colour = cell.grid.colour,
            linewidth = cell.grid.linewidth,
            linetype = cell.grid.linetype,
            alpha = cell.grid.alpha,
            lineend = lineend,
            linejoin = linejoin
        )

        grid::grobTree(tiles, cell_grid)
    }
)

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
StatPsychroBin <- ggproto(
    "StatPsychroBin", Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = ggplot2::after_stat(hours),
        width = ggplot2::after_stat(width),
        height = ggplot2::after_stat(height),
        alpha = 0.85
    ),

    dropped_aes = c("relhum", "value", "pres", "units"),

    extra_params = c(
        "na.rm", "bins", "binwidth", "boundary", "drop", "fun", "gap",
        "units", "pres"
    ),

    required_aes = c("x", "y|relhum"),

    optional_aes = "value",

    compute_group = function(self, data, scales, bins = 30,
                             binwidth = NULL, boundary = c(0, 0),
                             drop = TRUE, fun = "sum",
                             gap = 0.08, na.rm = FALSE) {
        units <- get_units(data)
        gap <- psychro_bin_gap(gap)
        data <- psychro_bin_humidity(data, units)
        data <- psychro_bin_drop_missing(data, na.rm = na.rm)
        if (!nrow(data)) {
            return(psychro_bin_empty())
        }

        bins <- psychro_bin_bins(bins)
        binwidth <- psychro_bin_binwidth(binwidth, units)
        boundary <- psychro_bin_boundary(boundary, units, binwidth)
        fun <- match.arg(fun, c("sum", "mean", "median", "min", "max"))

        x_breaks <- psychro_bin_breaks(
            data$x, bins[[1L]], binwidth[[1L]], boundary[[1L]]
        )
        y_breaks <- psychro_bin_breaks(
            data$y, bins[[2L]], binwidth[[2L]], boundary[[2L]]
        )

        x_bin <- psychro_bin_find(data$x, x_breaks)
        y_bin <- psychro_bin_find(data$y, y_breaks)

        nx <- length(x_breaks) - 1L
        ny <- length(y_breaks) - 1L
        bin_id <- (y_bin - 1L) * nx + x_bin
        n_bins <- nx * ny

        counts <- tabulate(bin_id, nbins = n_bins)
        values <- psychro_bin_values(data, bin_id, n_bins, fun)

        grid <- expand.grid(x_bin = seq_len(nx), y_bin = seq_len(ny))
        keep <- if (drop) counts > 0L else rep(TRUE, n_bins)
        grid <- grid[keep, , drop = FALSE]

        x_width <- diff(x_breaks)
        y_width <- diff(y_breaks)
        x_center <- x_breaks[-length(x_breaks)] + x_width / 2
        y_center <- y_breaks[-length(y_breaks)] + y_width / 2
        tile_scale <- 1 - gap

        new_data_frame(list(
            x = x_center[grid$x_bin],
            y = y_center[grid$y_bin],
            width = x_width[grid$x_bin] * tile_scale,
            height = y_width[grid$y_bin] * tile_scale,
            cell_xmin = x_breaks[grid$x_bin],
            cell_xmax = x_breaks[grid$x_bin + 1L],
            cell_ymin = y_breaks[grid$y_bin],
            cell_ymax = y_breaks[grid$y_bin + 1L],
            count = as.numeric(counts[keep]),
            hours = as.numeric(counts[keep]),
            value = values[keep]
        ))
    }
)

psychro_bin_humidity <- function(data, units) {
    if ("y" %in% names(data)) {
        return(data)
    }

    pres <- unique(data$pres)
    if (length(pres) != 1L) {
        stop("`pres` must resolve to a single pressure value.", call. = FALSE)
    }

    data$y <- with_units(
        units,
        psychrolib::GetHumRatioFromRelHum(data$x, data$relhum, pres)
    )
    data
}

psychro_bin_drop_missing <- function(data, na.rm = FALSE) {
    vars <- c("x", "y", "value"["value" %in% names(data)])
    keep <- stats::complete.cases(data[vars])

    if (all(keep)) {
        return(data)
    }

    if (!na.rm) {
        warning(
            sprintf("Removed %d rows containing missing values.", sum(!keep)),
            call. = FALSE
        )
    }
    data[keep, , drop = FALSE]
}

psychro_bin_empty <- function() {
    new_data_frame(list(
        x = numeric(),
        y = numeric(),
        width = numeric(),
        height = numeric(),
        cell_xmin = numeric(),
        cell_xmax = numeric(),
        cell_ymin = numeric(),
        cell_ymax = numeric(),
        count = numeric(),
        hours = numeric(),
        value = numeric()
    ))
}

psychro_bin_bins <- function(bins) {
    if (!is.numeric(bins) || length(bins) < 1L || length(bins) > 2L ||
            any(!is.finite(bins)) || any(bins < 1)) {
        stop("`bins` must be one or two positive finite numbers.", call. = FALSE)
    }

    as.integer(rep(bins, length.out = 2L))
}

psychro_bin_gap <- function(gap) {
    if (!is.numeric(gap) || length(gap) != 1L || !is.finite(gap) ||
            gap < 0 || gap >= 1) {
        stop("`gap` must be a single finite number in [0, 1).", call. = FALSE)
    }

    gap
}

psychro_bin_boundary <- function(boundary, units, binwidth) {
    if (all(vapply(binwidth, is.null, logical(1L)))) {
        return(list(0, 0))
    }

    if (!is.numeric(boundary) || length(boundary) < 1L ||
            length(boundary) > 2L || any(!is.finite(boundary))) {
        stop("`boundary` must be one or two finite numbers.", call. = FALSE)
    }

    boundary <- rep(boundary, length.out = 2L)
    list(boundary[[1L]], narrow_hum(boundary[[2L]], units))
}

psychro_bin_binwidth <- function(binwidth, units) {
    if (is.null(binwidth)) {
        return(list(NULL, NULL))
    }

    if (!is.numeric(binwidth) || length(binwidth) < 1L ||
            length(binwidth) > 2L || any(!is.finite(binwidth)) ||
            any(binwidth <= 0)) {
        stop("`binwidth` must be one or two positive finite numbers.", call. = FALSE)
    }

    binwidth <- rep(binwidth, length.out = 2L)
    list(binwidth[[1L]], narrow_hum(binwidth[[2L]], units))
}

psychro_bin_breaks <- function(x, bins, binwidth, boundary = 0) {
    rng <- range(x, finite = TRUE)

    if (is.null(binwidth)) {
        if (rng[[1L]] == rng[[2L]]) {
            rng <- rng + c(-0.5, 0.5)
        }
        return(seq(rng[[1L]], rng[[2L]], length.out = bins + 1L))
    }

    lower <- floor((rng[[1L]] - boundary) / binwidth) * binwidth + boundary
    upper <- ceiling((rng[[2L]] - boundary) / binwidth) * binwidth + boundary
    if (lower == upper) {
        upper <- lower + binwidth
    }

    breaks <- seq(lower, upper, by = binwidth)
    if (utils::tail(breaks, 1L) < rng[[2L]]) {
        breaks <- c(breaks, utils::tail(breaks, 1L) + binwidth)
    }
    breaks
}

psychro_bin_find <- function(x, breaks) {
    findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
}

psychro_bin_values <- function(data, bin_id, n_bins, fun) {
    if (!"value" %in% names(data)) {
        return(rep(NA_real_, n_bins))
    }

    fun <- switch(fun,
        sum = sum,
        mean = mean,
        median = stats::median,
        min = min,
        max = max
    )

    values <- rep(NA_real_, n_bins)
    split_values <- split(data$value, bin_id)
    values[as.integer(names(split_values))] <- vapply(
        split_values, fun, numeric(1L)
    )
    values
}

psychro_tile_grob <- function(data, panel_params, coord, lineend, linejoin) {
    polygons <- psychro_tile_polygon_data(data, coord)
    if (!nrow(polygons)) {
        return(grid::nullGrob())
    }

    ggplot2::GeomPolygon$draw_panel(
        polygons, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
}

psychro_tile_polygon_data <- function(data, coord, n = 16L) {
    if (!nrow(data)) {
        return(data[0, , drop = FALSE])
    }

    data <- psychro_tile_bounds(data)
    draw_rectangular <- isTRUE(coord$mollier) ||
        is.null(coord$units) || is.null(coord$pressure)

    polygons <- lapply(seq_len(nrow(data)), function(i) {
        if (draw_rectangular) {
            psychro_tile_rectangle_polygon(data[i, , drop = FALSE], i)
        } else {
            psychro_tile_saturation_polygon(
                data[i, , drop = FALSE], coord$units, coord$pressure, i, n = n
            )
        }
    })
    polygons <- Filter(nrow, polygons)
    if (!length(polygons)) {
        return(data[0, , drop = FALSE])
    }

    out <- do.call(rbind, polygons)
    row.names(out) <- NULL
    out
}

psychro_tile_bounds <- function(data) {
    if (!"xmin" %in% names(data)) {
        data$xmin <- data$x - data$width / 2
    }
    if (!"xmax" %in% names(data)) {
        data$xmax <- data$x + data$width / 2
    }
    if (!"ymin" %in% names(data)) {
        data$ymin <- data$y - data$height / 2
    }
    if (!"ymax" %in% names(data)) {
        data$ymax <- data$y + data$height / 2
    }
    data
}

psychro_tile_rectangle_polygon <- function(row, group) {
    if (!psychro_tile_has_area(row)) {
        return(row[0, , drop = FALSE])
    }

    out <- row[rep(1L, 4L), , drop = FALSE]
    out$x <- c(row$xmin, row$xmax, row$xmax, row$xmin)
    out$y <- c(row$ymin, row$ymin, row$ymax, row$ymax)
    out$group <- group
    out
}

psychro_tile_saturation_polygon <- function(row, units, pres, group, n = 16L,
                                            tolerance = 1e-10) {
    if (!psychro_tile_has_area(row)) {
        return(row[0, , drop = FALSE])
    }

    saturation_min <- psychro_saturation_humratio(row$xmin, units, pres)
    saturation_max <- psychro_saturation_humratio(row$xmax, units, pres)
    if (is.finite(saturation_min) && row$ymax <= saturation_min + tolerance) {
        return(psychro_tile_rectangle_polygon(row, group))
    }
    if (is.finite(saturation_max) && row$ymin >= saturation_max - tolerance) {
        return(row[0, , drop = FALSE])
    }

    x <- psychro_tile_saturation_x(row, units, pres, n = n)
    saturation <- psychro_saturation_humratio(x, units, pres)
    upper <- pmin(row$ymax, saturation)
    keep <- is.finite(x) & is.finite(upper) & upper >= row$ymin - tolerance
    x <- x[keep]
    upper <- pmax(upper[keep], row$ymin)

    if (length(x) < 2L || all(upper <= row$ymin + tolerance)) {
        return(row[0, , drop = FALSE])
    }

    out <- row[rep(1L, length(x) * 2L), , drop = FALSE]
    out$x <- c(x, rev(x))
    out$y <- c(rep(row$ymin, length(x)), rev(upper))
    out$group <- group
    out
}

psychro_tile_has_area <- function(row) {
    vals <- unlist(row[c("xmin", "xmax", "ymin", "ymax")], use.names = FALSE)
    all(is.finite(vals)) && row$xmin < row$xmax && row$ymin < row$ymax
}

psychro_tile_saturation_x <- function(row, units, pres, n = 16L,
                                      tolerance = 1e-10) {
    x <- c(
        seq(row$xmin, row$xmax, length.out = max(2L, n)),
        row$xmin,
        row$xmax,
        psychro_tile_tdb_at_hum(c(row$ymin, row$ymax), units, pres)
    )
    x <- x[is.finite(x) & x >= row$xmin - tolerance & x <= row$xmax + tolerance]
    x <- sort(pmin(pmax(x, row$xmin), row$xmax))
    x[c(TRUE, diff(x) > tolerance)]
}

psychro_tile_tdb_at_hum <- function(hum, units, pres) {
    out <- rep(NA_real_, length(hum))
    keep <- is.finite(hum) & hum > 0
    if (any(keep)) {
        out[keep] <- with_units(
            units,
            GetTDewPointFromHumRatioOnly(hum[keep], pres)
        )
    }
    out
}

psychro_tile_cell_grid_grob <- function(data, panel_params, coord, theme, colour,
                                        linewidth, linetype, alpha,
                                        lineend, linejoin) {
    segments <- psychro_tile_cell_grid_data(
        data, panel_params, coord, theme, colour, linewidth, linetype, alpha
    )
    if (!nrow(segments)) {
        return(grid::nullGrob())
    }

    ggplot2::GeomSegment$draw_panel(
        segments, panel_params, coord, lineend = lineend,
        linejoin = linejoin, na.rm = TRUE
    )
}

psychro_tile_cell_grid_data <- function(data, panel_params, coord, theme,
                                        colour, linewidth, linetype, alpha) {
    segments <- psychro_tile_cell_segments(data, panel_params, coord)
    if (!nrow(segments)) {
        return(segments)
    }

    theme <- theme %||% coord$psychro_theme %||% ggplot2::theme_get()
    styles <- lapply(seq_len(nrow(segments)), function(i) {
        psychro_tile_cell_grid_style(
            theme, segments$axis[[i]], segments$grid_type[[i]],
            colour, linewidth, linetype, alpha
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

psychro_tile_cell_grid_style <- function(theme, axis, type, colour, linewidth,
                                         linetype, alpha) {
    element <- ggplot2::calc_element(
        paste("panel.grid", type, axis, sep = "."),
        theme
    )
    has_override <- !is.waive(colour) || !is.waive(linewidth) ||
        !is.waive(linetype) || !is.waive(alpha)
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
            colour, if (!is_blank) element$colour else NULL, defaults$colour
        ),
        linewidth = psychro_tile_cell_grid_value(
            linewidth,
            if (!is_blank) element$linewidth else NULL,
            defaults$linewidth
        ),
        linetype = psychro_tile_cell_grid_value(
            linetype, if (!is_blank) element$linetype else NULL,
            defaults$linetype
        ),
        alpha = psychro_tile_cell_grid_value(
            alpha, if (!is_blank) element$alpha else NULL, defaults$alpha
        )
    )
}

psychro_tile_cell_grid_value <- function(value, inherited, default) {
    if (!is.waive(value)) {
        return(value)
    }

    inherited %||% default
}

psychro_tile_cell_segments <- function(data, panel_params, coord) {
    needed <- c("cell_xmin", "cell_xmax", "cell_ymin", "cell_ymax")
    if (!nrow(data) || !all(needed %in% names(data)) || is.null(panel_params)) {
        return(new_data_frame(list(
            x = numeric(), y = numeric(), xend = numeric(), yend = numeric()
        )))
    }

    x_spacing <- psychro_tile_cell_spacing(data$cell_xmin, data$cell_xmax)
    y_spacing <- psychro_tile_cell_spacing(data$cell_ymin, data$cell_ymax)
    x_breaks <- psychro_tile_cell_grid_breaks(panel_params, "x", x_spacing)
    y_breaks <- psychro_tile_cell_grid_breaks(panel_params, "y", y_spacing)
    if ((!nrow(x_breaks) && is.null(x_spacing)) ||
        (!nrow(y_breaks) && is.null(y_spacing))) {
        return(new_data_frame(list(
            x = numeric(), y = numeric(), xend = numeric(), yend = numeric()
        )))
    }

    ranges <- psychro_tile_panel_ranges(panel_params)
    if (!nrow(x_breaks)) {
        x_breaks <- psychro_tile_spacing_breaks(
            ranges$x, x_spacing$width, x_spacing$anchor
        )
    }
    if (!nrow(y_breaks)) {
        y_breaks <- psychro_tile_spacing_breaks(
            ranges$y, y_spacing$width, y_spacing$anchor
        )
    }

    psychro_tile_chart_segments(x_breaks, y_breaks, ranges$x, ranges$y, coord)
}

psychro_tile_panel_grid_breaks <- function(panel_params, axis,
                                           tolerance = 1e-8) {
    scale <- panel_params[[axis]]
    if (is.null(scale)) {
        return(psychro_tile_break_data(numeric(), character()))
    }

    ranges <- psychro_tile_panel_ranges(panel_params)[[axis]]
    major <- psychro_tile_break_values(scale$breaks)
    minor <- psychro_tile_break_values(scale$minor_breaks)
    major <- major[major >= ranges[[1L]] - tolerance &
        major <= ranges[[2L]] + tolerance]
    minor <- minor[minor >= ranges[[1L]] - tolerance &
        minor <= ranges[[2L]] + tolerance]

    major_key <- round(major, 12L)
    minor <- minor[!round(minor, 12L) %in% major_key]

    psychro_tile_break_data(c(minor, major), c(
        rep("minor", length(minor)),
        rep("major", length(major))
    ))
}

psychro_tile_cell_grid_breaks <- function(panel_params, axis, spacing,
                                          tolerance = 1e-8) {
    breaks <- psychro_tile_panel_grid_breaks(panel_params, axis)
    if (is.null(spacing)) {
        return(breaks)
    }

    ranges <- psychro_tile_panel_ranges(panel_params)[[axis]]
    if (!nrow(breaks)) {
        return(psychro_tile_spacing_breaks(
            ranges, spacing$width, spacing$anchor
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

psychro_tile_breaks_are_aligned <- function(values, width, anchor,
                                            tolerance = 1e-8) {
    steps <- (values - anchor) / width
    all(abs(steps - round(steps)) <= tolerance)
}

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

psychro_tile_break_values <- function(x) {
    x <- unlist(x, use.names = FALSE)
    x[is.finite(x)]
}

psychro_tile_break_data <- function(value, type) {
    if (!length(value)) {
        return(new_data_frame(list(value = numeric(), type = character())))
    }

    order <- order(value)
    new_data_frame(list(value = value[order], type = type[order]))
}

psychro_tile_cell_spacing <- function(lower, upper, tolerance = 1e-8) {
    widths <- unique(round(upper - lower, 12L))
    widths <- widths[is.finite(widths) & widths > tolerance]
    if (!length(widths)) {
        return(NULL)
    }

    list(width = widths[[1L]], anchor = min(c(lower, upper), na.rm = TRUE))
}

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

psychro_tile_grid_breaks <- function(range, width, anchor, tolerance = 1e-8) {
    lower <- ceiling((range[[1L]] - anchor) / width - tolerance) * width + anchor
    upper <- floor((range[[2L]] - anchor) / width + tolerance) * width + anchor

    if (lower > upper) {
        return(numeric())
    }

    seq(lower, upper, by = width)
}

psychro_tile_spacing_breaks <- function(range, width, anchor) {
    breaks <- psychro_tile_grid_breaks(range, width, anchor)
    psychro_tile_break_data(breaks, rep("major", length(breaks)))
}

psychro_tile_chart_segments <- function(x_breaks, y_breaks, x_range, y_range,
                                        coord) {
    if (!nrow(x_breaks) && !nrow(y_breaks)) {
        return(new_data_frame(list(
            x = numeric(), y = numeric(), xend = numeric(), yend = numeric()
        )))
    }

    if (isTRUE(coord$mollier) || is.null(coord$units) || is.null(coord$pressure)) {
        return(psychro_tile_rectangular_segments(x_breaks, y_breaks, x_range, y_range))
    }

    x_sat <- with_units(
        coord$units,
        psychrolib::GetHumRatioFromRelHum(x_breaks$value, 1.0, coord$pressure)
    )
    vertical <- new_data_frame(list(
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
        y_dew[positive] <- with_units(
            coord$units,
            GetTDewPointFromHumRatioOnly(y_breaks$value[positive], coord$pressure)
        )
    }
    horizontal <- new_data_frame(list(
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

psychro_tile_rectangular_segments <- function(x_breaks, y_breaks, x_range, y_range) {
    vertical <- new_data_frame(list(
        x = x_breaks$value,
        y = y_range[[1L]],
        xend = x_breaks$value,
        yend = y_range[[2L]],
        axis = "x",
        grid_type = x_breaks$type
    ))
    horizontal <- new_data_frame(list(
        x = x_range[[1L]],
        y = y_breaks$value,
        xend = x_range[[2L]],
        yend = y_breaks$value,
        axis = "y",
        grid_type = y_breaks$type
    ))

    unique(rbind(vertical, horizontal))
}
