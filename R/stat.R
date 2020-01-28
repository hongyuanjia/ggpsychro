#' Calculate psychrometric properties of moist air
#'
#' @details
#'
#' * `stat_relhum` requires an extra `relhum` aesthetics for relative humidity
#'   in range \\[0, 100\\] in %
#' * `stat_wetbulb` requires an extra `wetbulb` aesthetics for wet-bulb
#'   temperature in °F \\[IP\\] or °C \\[SI\\]
#' * `stat_vappres` requires an extra `vappres` aesthetics for partial pressure
#'   of water vapor in moist air in Psi \\[IP\\] or Pa \\[SI\\]
#' * `stat_specvol` requires an extra `specvol` aesthetics for specific volume
#'   of moist air in ft3 lb-1 of dry air \\[IP\\] or in m3 kg-1 of dry air \\[SI\\]
#' * `stat_enthalpy` requires an extra `enthalpy` aesthetics for moist air
#'   enthalpy in Btu lb-1 \\[IP\\] or J kg-1
#'
#' What these [ggplot2::ggproto()] objects do are to take input values,
#' calculate the corresponding humidity ratio and replace the `y` aesthetic
#' values in each group.
#'
#' All of stats above requires two additional aesthetics:
#'
#' * `units`: A single string indicating the units sytem to use. Should be
#'   either `"SI"` or `"IP" or `waiver()` which uses the value from the parent
#'   plot. Default: `waiver()`
#'
#' * `pres`: A single number indicating the atmosphere pressure in Pa \\[SI\\] or
#'   Psi \\[IP\\]. If `waiver()`, the pressure calculated from the parent plot's
#'   altitude value will be used. Default: `waiver()`
#'
#' However, when these stats are used inside a ggplot `geom_*` as the `stat`
#' argument, both `units` and `pres` have to be specified.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname stat
#'
#' @examples
#' p <- ggpsychro() + geom_grid_relhum() # add relative humidity grid lines
#'
#' # draw a point with dry-bulb at 30C and relative humidity at 60%
#' p + geom_point(aes(x = 30, relhum = 0.6), stat = "relhum", size = 5)
#'
#' # draw a constant relative humidity line of 60% with dry-bulb from 20C to 30C
#' ## use stat_* directly
#' p + stat_relhum(geom = "line", aes(x = 20:30, relhum = 0.6), size = 2)
#' ## OR
#' ## use as thet `stat` argument inside an ggplot `geom_*` function
#' p + geom_line(aes(x = 20:30, relhum = 0.6), stat = "relhum")
#'
#' @export
# stat_relhum {{{
stat_relhum <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatRelhum, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname stat
# stat_wetbulb {{{
stat_wetbulb <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatWetbulb, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname stat
# stat_vappres {{{
stat_vappres <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatVappres, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname stat
# stat_specvol {{{
stat_specvol <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatSpecvol, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname stat
# stat_enthalpy {{{
stat_enthalpy <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatEnthalpy, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

# init_stat_data {{{
init_stat_data <- function (data, params) {
    if (!"units" %in% names(data)) {
        data$units <- encode_units(params$units)
    } else {
        data$units <- encode_units(unique(data$units))
    }

    if (!"pres" %in% names(data)) {
        data$pres <- params$pres
    }

    data
}
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @export
# StatRelhum {{{
StatRelhum <- ggproto(
    "StatRelhum", Stat,

    setup_data = function (self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label"),

    required_aes = c("x", "relhum", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]

        if (!length(ys)) ys <- "y"

        # get input pair
        for (var in ys) {
            data[[var]] <- with_units(units, GetHumRatioFromRelHum(data$x, data$relhum, data$pres))
        }

        xs <- names(data)[names(data) %in% GGPSY_OPT$x_aes]

        cols <- c(xs, ys, "relhum", "label"["label" %in% names(data)])
        data[cols]
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromTWetBulb
#' @export
# StatWetbulb {{{
StatWetbulb <- ggproto(
    "StatWetbulb", Stat,

    setup_data = function (self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label"),

    required_aes = c("x", "wetbulb", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]

        if (!length(ys)) ys <- "y"

        # get input pair
        for (var in ys) {
            data[[var]] <- with_units(units, GetHumRatioFromTWetBulb(data$x, data$wetbulb, data$pres))
        }

        xs <- names(data)[names(data) %in% GGPSY_OPT$x_aes]

        cols <- c(xs, ys, "wetbulb", "label"["label" %in% names(data)])
        data[cols]
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromVapPres
#' @export
# StatVappres {{{
StatVappres <- ggproto(
    "StatVappres", Stat,

    setup_data = function (self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label"),

    required_aes = c("x", "vappres", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]

        if (!length(ys)) ys <- "y"

        for (var in ys) {
            data[[var]] <- with_units(units, GetHumRatioFromVapPres(data$vappres, data$pres))
        }

        xs <- names(data)[names(data) %in% GGPSY_OPT$x_aes]

        cols <- c(xs, ys, "vappres", "label"["label" %in% names(data)])
        data[cols]
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# StatSpecvol {{{
StatSpecvol <- ggproto(
    "StatSpecvol", Stat,

    setup_data = function (self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label"),

    required_aes = c("x", "specvol", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]

        if (!length(ys)) ys <- "y"

        for (var in ys) {
            data[[var]] <- with_units(units, GetHumRatioFromAirVolume(data$x, data$specvol, data$pres))
        }

        xs <- names(data)[names(data) %in% GGPSY_OPT$x_aes]

        cols <- c(xs, ys, "specvol", "label"["label" %in% names(data)])
        data[cols]
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromEnthalpyAndTDryBulb
#' @export
# StatEnthalpy {{{
StatEnthalpy <- ggproto(
    "StatEnthalpy", Stat,

    setup_data = function (self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label"),

    required_aes = c("x", "enthalpy", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]

        for (var in ys) {
            data[[var]] <- with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(data$enthalpy, data$x))
        }

        xs <- names(data)[names(data) %in% GGPSY_OPT$x_aes]

        cols <- c(xs, ys, "enthalpy", "label"["label" %in% names(data)])
        data[cols]
    }
)
# }}}
