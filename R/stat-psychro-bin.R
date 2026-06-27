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
#' @param drop If `TRUE`, the default, omit empty bins.
#' @param fun Summary function used when the `value` aesthetic is supplied.
#'   One of `"sum"`, `"mean"`, `"median"`, `"min"`, or `"max"`.
#'
#' @details
#' The stat accepts either `x` and `y` aesthetics, where `y` is humidity ratio
#' in chart display units, or `x` and `relhum`, where `relhum` is relative
#' humidity in percent. Relative humidity inputs inherit the plot unit system
#' and pressure from [ggpsychro()].
#'
#' @section Computed variables:
#' * `count`: number of observations in each tile.
#' * `hours`: same as `count`, named for hourly weather data.
#' * `value`: aggregated `value` aesthetic when supplied.
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
                             binwidth = NULL, drop = TRUE, fun = "sum",
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {
    psychro_layer(
        stat = StatPsychroBin, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm, bins = bins, binwidth = binwidth,
            drop = drop, fun = fun, ...
        )
    )
}

#' @rdname stat_psychro_bin
#' @export
geom_psychro_tile <- function(mapping = NULL, data = NULL, stat = "psychro_bin",
                              position = "identity", ..., na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        data = data, mapping = mapping, stat = stat, geom = ggplot2::GeomTile,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

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
        height = ggplot2::after_stat(height)
    ),

    dropped_aes = c("relhum", "value", "pres", "units"),

    extra_params = c(
        "na.rm", "bins", "binwidth", "drop", "fun", "units", "pres"
    ),

    required_aes = c("x", "y|relhum"),

    optional_aes = "value",

    compute_group = function(self, data, scales, bins = 30,
                             binwidth = NULL, drop = TRUE, fun = "sum",
                             na.rm = FALSE) {
        units <- get_units(data)
        data <- psychro_bin_humidity(data, units)
        data <- psychro_bin_drop_missing(data, na.rm = na.rm)
        if (!nrow(data)) {
            return(psychro_bin_empty())
        }

        bins <- psychro_bin_bins(bins)
        binwidth <- psychro_bin_binwidth(binwidth, units)
        fun <- match.arg(fun, c("sum", "mean", "median", "min", "max"))

        x_breaks <- psychro_bin_breaks(data$x, bins[[1L]], binwidth[[1L]])
        y_breaks <- psychro_bin_breaks(data$y, bins[[2L]], binwidth[[2L]])

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

        new_data_frame(list(
            x = x_center[grid$x_bin],
            y = y_center[grid$y_bin],
            width = x_width[grid$x_bin],
            height = y_width[grid$y_bin],
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

psychro_bin_breaks <- function(x, bins, binwidth) {
    rng <- range(x, finite = TRUE)

    if (is.null(binwidth)) {
        if (rng[[1L]] == rng[[2L]]) {
            rng <- rng + c(-0.5, 0.5)
        }
        return(seq(rng[[1L]], rng[[2L]], length.out = bins + 1L))
    }

    lower <- floor(rng[[1L]] / binwidth) * binwidth
    upper <- ceiling(rng[[2L]] / binwidth) * binwidth
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
