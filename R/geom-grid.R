#' Draw constant psychrometric properties grids
#'
#' @details
#'
#' * `geom_satline` for saturation line.
#' * `geom_grid_relhum` for relative humidity grid in range [0, 100] in %
#' * `geom_grid_wetbulb` for wet-bulb temperature grid in °F [IP] or °C [SI]
#' * `geom_grid_vappres` for partial pressure grid of water vapor in Psi [IP]
#'   or Pa [SI]
#' * `geom_grid_specvol` for specific volume grid in ft3 lb-1 of dry air [IP] or
#'    in m3 kg-1 of dry air [SI]
#' * `geom_grid_enthalpy` for moist air enthalpy grid in Btu lb-1 [IP] or kJ kg-1
#'
#' What these [ggplot2::ggproto()] objects do are to take input values,
#' calculate the corresponding humidity ratio and replace the `y` aesthetic
#' values in each group.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @export
#' @importFrom ggplot2 layer GeomLine
#' @rdname geom_grid
# geom_satline {{{
geom_satline <- function (mapping = NULL, data = NULL, n = 201, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomSatLine,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_relhum {{{
#' @export
geom_grid_relhum <- function (mapping = NULL, data = NULL, n = 201, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridRelHum,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_wetbulb {{{
#' @export
geom_grid_wetbulb <- function (mapping = NULL, data = NULL, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridWetBulb,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = 50, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_vappres {{{
#' @export
geom_grid_vappres <- function (mapping = NULL, data = NULL, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridVapPres,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = 2, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_specvol {{{
#' @export
geom_grid_specvol <- function (mapping = NULL, data = NULL, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridSpecVol,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = 50, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_enthalpy {{{
#' @export
geom_grid_enthalpy <- function (mapping = NULL, data = NULL, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridEnthalpy,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = 50, na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @export
# GeomSatLine {{{
GeomSatLine <- ggproto("GeomSatLine", GeomLine,
    required_aes = c("n", "pres", "units"),

    setup_data = function (data, params) {
        GeomLine$setup_data(init_grid_data(data), params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        units <- get_units(data)

        data <- compute_grid_panel_data(data, panel_params, coord,
            vlim = c(1.0, 1.0), step = 1L)

        # calculate hum ratio at each relative humidity
        data$y <- amplify_hum(with_units(units, GetHumRatioFromRelHum(data$x, data$var, data$pres)), units)

        data <- clean_grid_panel_data(data)

        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#DA251D", size = 1, linetype = 1, alpha = NA)
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @export
# GeomGridRelHum {{{
GeomGridRelHum <- ggproto("GeomGridRelHum", GeomLine,
    required_aes = c("step", "n", "pres", "units"),

    setup_data = function (data, params) {
        GeomLine$setup_data(init_grid_data(data), params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        units <- get_units(data)

        data <- compute_grid_panel_data(data, panel_params, coord,
            vlim = c(0.0, 1.0), step = unique(data$step)/100)

        # exclude zero line and saturatio line
        data <- data[data$var > 0.0 & data$var < 1.0, ]

        # calculate hum ratio at each relative humidity
        data$y <- amplify_hum(with_units(units, GetHumRatioFromRelHum(data$x, data$var, data$pres)), units)

        data <- clean_grid_panel_data(data)

        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#00126B", size = 0.5, linetype = "twodash", alpha = NA)
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom psychrolib GetTWetBulbFromHumRatio
#' @export
# GeomGridWetBulb {{{
GeomGridWetBulb <- ggproto("GeomGridWetBulb", GeomLine,
    required_aes = c("n", "step", "pres", "units"),

    setup_data = function (data, params) {
        GeomLine$setup_data(init_grid_data(data), params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        units <- get_units(data)
        pres <- get_pres(data)
        ranges <- coord$limits

        # calculate wetbulb at the min drybulb at the origin
        vmin <- with_units(units, GetTWetBulbFromHumRatio(ranges$x[[1L]], ranges$y[[1L]], pres))

        # get the break step of drybulb
        xstep <- diff(panel_params$x.minor_source)[[1L]]

        # calculate min wetbulb with whole x steps away
        vmin <- ranges$x[[1L]] - ceiling((ranges$x[[1L]] - vmin) / xstep) * xstep
        vlim <- c(vmin, ranges$x[[2L]])

        # wet-bulb range is the same as drybulb
        data <- compute_grid_panel_data(data, panel_params, coord, vlim = vlim)

        # make sure there is a point on the saturation line
        data <- do.call(rbind, lapply(split(data, data$group), function (d) {
            if (any(d$x == min(d$var))) return(d)
            sat <- d[1L, ]
            sat$x <- min(d$var)
            rbind(sat, d)
        }))

        # only keep rows where drybulb is larger than wetbulb
        data <- data[data$x >= data$var, ]

        # calculate hum ratio
        data$y <- amplify_hum(with_units(units, GetHumRatioFromTWetBulb(data$x, data$var, data$pres)), units)

        # only keep rows where hum ratio is larger than 0.01
        data <- data[data$y > 0.01, ]

        # extend curve to the bottom axis
        data <- add_x_intercept(data)

        data <- clean_grid_panel_data(data)

        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#015756", size = 0.5, linetype = "twodash", alpha = NA)
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom psychrolib GetVapPresFromHumRatio
#' @export
# GeomGridVapPres {{{
GeomGridVapPres <- ggproto("GeomGridVapPres", GeomLine,
    required_aes = c("step", "n", "pres", "units"),

    setup_data = function (data, params) {
        GeomLine$setup_data(init_grid_data(data), params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        units <- get_units(data)
        pres <- get_pres(data)

        # calculate vapor pressure range at the y axis range
        yrange <- narrow_hum(coord$limits$y, units)
        vrange <- round(GetVapPresFromHumRatio(yrange, pres))

        data <- compute_grid_panel_data(data, panel_params, coord, vlim = vrange)

        # calculate hum ratio at each vapor pressure
        data$y <- amplify_hum(with_units(units, GetHumRatioFromVapPres(data$var, data$pres)), units)

        data <- clean_grid_panel_data(data)

        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#941919", size = 0.5, linetype = 2, alpha = NA)
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom psychrolib GetMoistAirVolume
#' @export
# GeomGridSpecVol {{{
GeomGridSpecVol <- ggproto("GeomGridSpecVol", GeomLine,
    required_aes = c("step", "n", "pres", "units"),

    setup_data = function (data, params) {
        GeomLine$setup_data(init_grid_data(data), params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        units <- get_units(data)
        pres <- get_pres(data)
        ranges <- coord$limits

        # calculate spec vol range at the x and y axis range
        vrange <- with_units(units, GetMoistAirVolume(ranges$x, narrow_hum(ranges$y, units), pres))

        data <- compute_grid_panel_data(data, panel_params, coord, vlim = vrange)

        # calculate hum ratio at each vapor pressure
        data$y <- amplify_hum(with_units(units, GetHumRatioFromAirVolume(data$x, data$var, data$pres)), units)

        # only keep rows where hum ratio is larger than 0.01
        data <- data[data$y > 0.01, ]

        # extend line to the bottom axis
        data <- add_x_intercept(data)

        data <- clean_grid_panel_data(data)

        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#108860", size = 0.5, linetype = 1, alpha = NA)
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom psychrolib GetHumRatioFromEnthalpyAndTDryBulb GetMoistAirEnthalpy
#' @export
# GeomGridEnthalpy {{{
GeomGridEnthalpy <- ggproto("GeomGridEnthalpy", GeomLine,
    required_aes = c("step", "n", "pres", "units"),

    setup_data = function (data, params) {
        GeomLine$setup_data(init_grid_data(data), params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        units <- get_units(data)
        ranges <- coord$limits

        # calculate spec vol range at the x and y axis range
        vrange <- narrow_enth(with_units(units, GetMoistAirEnthalpy(ranges$x, narrow_hum(ranges$y, units))), units)
        vrange <- round(vrange)

        data <- compute_grid_panel_data(data, panel_params, coord, vlim = vrange)

        # calculate hum ratio at each enthalpy
        data$y <- amplify_hum(with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(amplify_enth(data$var, units), data$x)), units)

        # only keep rows where hum ratio is larger than 0.01
        data <- data[data$y > 0.01, ]

        # extend line to the bottom axis
        data <- add_x_intercept(data)

        data <- clean_grid_panel_data(data)

        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#633F87", size = 0.5, linetype = 2, alpha = NA)
)
# }}}

# init_grid_data {{{
init_grid_data <- function (data) {
    # reset group
    data$group <- -1L

    # only keep unique
    data <- unique(data[c("PANEL", "group", "pres", "units")])

    # temporarily set x and y
    data$x <- 0
    data$y <- 0

    data
}
# }}}
# compute_grid_panel_data {{{
compute_grid_panel_data <- function (data, panel_params, coord,
                                     xlim = NULL, vlim = NULL, step = NULL) {
    if (is.null(xlim)) xlim <- coord$limits$x
    if (is.null(vlim)) vlim <- xlim
    if (is.null(step)) step <- data$step

    # get x based on axis x limits
    # check n
    n <- unique(data$n)
    assert_count(n, positive = TRUE)
    x <- seq(xlim[[1L]], xlim[[2L]], length.out = n)

    # get target variable values
    var <- seq(vlim[[1L]], vlim[[2L]], unique(step))

    # create new data
    d <- do.call(rbind, replicate(length(var), data, simplify = FALSE))

    d$var <- var

    # each relative humidity is a single group
    d$group <- seq_along(var)

    # create new data
    data <- do.call(rbind, replicate(length(x), d, simplify = FALSE))

    data$x <- rep(x, each = length(var))

    # now can safely remove helpers
    data$units <- NULL
    data$step <- NULL
    data$n <- NULL

    data
}
# }}}
# clean_grid_panel_data {{{
clean_grid_panel_data <- function (data) {
    data$var <- NULL
    data$press <- NULL

    data
}
# }}}
# add_x_intercept {{{
add_x_intercept <- function (data) {
    # extend curve to the bottom axis
    data <- do.call(rbind, lapply(split(data, data$group), function (d) {
        zero <- d[1L, ]

        # calculate slope
        slope <- (d$y[[nrow(d)]] - d$y[[1L]]) / (d$x[[nrow(d)]] - d$x[[1L]])
        intercept <- zero$y - slope * zero$x
        zero$x <- (0.0 - intercept) / slope
        zero$y <- 0.0

        rbind(d, zero)
    }))
}
# }}}
