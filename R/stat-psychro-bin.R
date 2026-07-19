#' @include utils.R stat.R
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
#' `binwidth` that evenly subdivides those breaks when a denser cell grid should
#' still coincide with the existing x/y grid.
#'
#' @section Computed variables:
#' * `count`: number of observations in each tile.
#' * `hours`: same as `count`, named for hourly weather data.
#' * `value`: aggregated `value` aesthetic when supplied.
#' * `width`, `height`: tile dimensions after applying `gap`.
#' * `cell_xmin`, `cell_xmax`, `cell_ymin`, `cell_ymax`: full bin boundaries.
#'
#' @return A ggplot layer.
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
stat_psychro_bin <- function(
    mapping = NULL,
    data = NULL,
    geom = "tile",
    position = "identity",
    ...,
    bins = 30,
    binwidth = NULL,
    boundary = c(0, 0),
    drop = TRUE,
    fun = "sum",
    gap = 0.08,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    if (identical(geom, "tile")) {
        geom <- GeomPsychroTile
    }

    psychro_layer(
        stat = StatPsychroBin,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            bins = bins,
            binwidth = binwidth,
            boundary = boundary,
            drop = drop,
            fun = fun,
            gap = gap,
            ...
        )
    )
}

# Internal ggproto backing stat_psychro_bin() and geom_psychro_tile().
StatPsychroBin <- ggproto(
    "StatPsychroBin",
    Stat,

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
        "na.rm",
        "bins",
        "binwidth",
        "boundary",
        "drop",
        "fun",
        "gap",
        "units",
        "pres"
    ),

    required_aes = c("x", "y|relhum"),

    optional_aes = "value",

    compute_group = function(
        self,
        data,
        scales,
        bins = 30,
        binwidth = NULL,
        boundary = c(0, 0),
        drop = TRUE,
        fun = "sum",
        gap = 0.08,
        na.rm = FALSE
    ) {
        units <- unit__from_data(data)
        gap <- bin__gap(gap)
        data <- bin__humidity(data, units)
        data <- bin__drop_missing(data, na.rm = na.rm)
        if (!nrow(data)) {
            return(bin__empty())
        }

        bins <- bin__bins(bins)
        binwidth <- bin__binwidth(binwidth, units)
        boundary <- bin__boundary(boundary, units, binwidth)
        fun <- match.arg(fun, c("sum", "mean", "median", "min", "max"))

        x_breaks <- bin__breaks(
            data$x,
            bins[[1L]],
            binwidth[[1L]],
            boundary[[1L]]
        )
        y_breaks <- bin__breaks(
            data$y,
            bins[[2L]],
            binwidth[[2L]],
            boundary[[2L]]
        )

        x_bin <- bin__find(data$x, x_breaks)
        y_bin <- bin__find(data$y, y_breaks)

        nx <- length(x_breaks) - 1L
        ny <- length(y_breaks) - 1L
        bin_id <- (y_bin - 1L) * nx + x_bin
        n_bins <- nx * ny

        counts <- tabulate(bin_id, nbins = n_bins)
        values <- bin__values(data, bin_id, n_bins, fun)

        if (isTRUE(drop)) {
            # Sparse weather or simulation datasets can request dense bin grids;
            # build only observed cells instead of allocating every empty bin.
            keep <- which(counts > 0L)
            grid <- util__new_data_frame(list(
                x_bin = (keep - 1L) %% nx + 1L,
                y_bin = (keep - 1L) %/% nx + 1L
            ))
        } else {
            keep <- seq_len(n_bins)
            grid <- expand.grid(x_bin = seq_len(nx), y_bin = seq_len(ny))
        }

        x_width <- diff(x_breaks)
        y_width <- diff(y_breaks)
        x_center <- x_breaks[-length(x_breaks)] + x_width / 2
        y_center <- y_breaks[-length(y_breaks)] + y_width / 2
        tile_scale <- 1 - gap

        util__new_data_frame(list(
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

# Convert relative-humidity input into humidity-ratio bin coordinates.
bin__humidity <- function(data, units) {
    if ("y" %in% names(data)) {
        return(data)
    }

    pres <- unique(data$pres)
    if (length(pres) != 1L) {
        stop("`pres` must resolve to a single pressure value.", call. = FALSE)
    }

    data$y <- psychrolib__with_units(
        units,
        psychrolib::GetHumRatioFromRelHum(data$x, data$relhum, pres)
    )
    data
}

# Drop incomplete binning rows while preserving ggplot-style warnings.
bin__drop_missing <- function(data, na.rm = FALSE) {
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

# Return an empty stat output with stable computed columns.
bin__empty <- function() {
    util__new_data_frame(list(
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

# Validate and recycle the requested bin count.
bin__bins <- function(bins) {
    bins <- util__check_whole_count(bins, "`bins`", min = 1L, max_len = 2L)
    rep(bins, length.out = 2L)
}

# Validate the visible gap between adjacent psychrometric tiles.
bin__gap <- function(gap) {
    if (
        !is.numeric(gap) ||
            length(gap) != 1L ||
            !is.finite(gap) ||
            gap < 0 ||
            gap >= 1
    ) {
        stop("`gap` must be a single finite number in [0, 1).", call. = FALSE)
    }

    gap
}

# Convert user-facing bin boundary input into native chart coordinates.
bin__boundary <- function(boundary, units, binwidth) {
    if (all(vapply(binwidth, is.null, logical(1L)))) {
        return(list(0, 0))
    }

    if (
        !is.numeric(boundary) ||
            length(boundary) < 1L ||
            length(boundary) > 2L ||
            any(!is.finite(boundary))
    ) {
        stop("`boundary` must be one or two finite numbers.", call. = FALSE)
    }

    boundary <- rep(boundary, length.out = 2L)
    list(boundary[[1L]], unit__hum_from_chart(boundary[[2L]], units))
}

# Convert user-facing binwidth input into native chart coordinates.
bin__binwidth <- function(binwidth, units) {
    if (is.null(binwidth)) {
        return(list(NULL, NULL))
    }

    if (
        !is.numeric(binwidth) ||
            length(binwidth) < 1L ||
            length(binwidth) > 2L ||
            any(!is.finite(binwidth)) ||
            any(binwidth <= 0)
    ) {
        stop(
            "`binwidth` must be one or two positive finite numbers.",
            call. = FALSE
        )
    }

    binwidth <- rep(binwidth, length.out = 2L)
    list(binwidth[[1L]], unit__hum_from_chart(binwidth[[2L]], units))
}

# Build breakpoints from either a target bin count or explicit bin width.
bin__breaks <- function(x, bins, binwidth, boundary = 0) {
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

# Map observations to bin indices using closed endpoint handling.
bin__find <- function(x, breaks) {
    findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
}

# Summarise optional value aesthetics for each computed bin.
bin__values <- function(data, bin_id, n_bins, fun) {
    if (!"value" %in% names(data)) {
        return(rep(NA_real_, n_bins))
    }

    fun <- switch(
        fun,
        sum = sum,
        mean = mean,
        median = stats::median,
        min = min,
        max = max
    )

    values <- rep(NA_real_, n_bins)
    split_values <- split(data$value, bin_id)
    values[as.integer(names(split_values))] <- vapply(
        split_values,
        fun,
        numeric(1L)
    )
    values
}
