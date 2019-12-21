#' Calculate psychrometric properties of moist air
#'
#' @details
#'
#' * `stat_relhum` for relative humidity in range [0, 100] in %
#' * `stat_wetbulb` for wet-bulb temperature in °F [IP] or °C [SI]
#' * `stat_vappres` for partial pressure of water vapor in moist air in Psi [IP]
#'   or Pa [SI]
#' * `stat_specvol` for specific volume of moist air in ft3 lb-1 of dry air [IP]
#'   or in m3 kg-1 of dry air [SI]
#' * `stat_enthalpy` for moist air enthalpy in Btu lb-1 [IP] or J kg-1
#'
#' What these [ggplot2::ggproto()] objects do are to take input values,
#' calculate the corresponding humidity ratio and replace the `y` aesthetic
#' values in each group.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 ggproto Stat Geom
#' @export
#' @rdname stat
# stat_relhum {{{
stat_relhum <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
        stat = StatRelHum, data = data, mapping = mapping, geom = geom,
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
    layer(
        stat = StatWetBulb, data = data, mapping = mapping, geom = geom,
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
    layer(
        stat = StatVapPres, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname stat
# stat_specheat {{{
stat_specheat <- function (mapping = NULL, data = NULL, geom = "point", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
        stat = StatSpecVol, data = data, mapping = mapping, geom = geom,
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
    layer(
        stat = StatEnthalpy, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @export
# StatRelHum {{{
StatRelHum <- ggproto(
    "StatRelHum", Stat,

    extra_params = c("na.rm"),

    required_aes = c("x", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]
        for (var in ys) {
            data[[var]] <- amplify_hum(with_units(units, GetHumRatioFromRelHum(data$x, data[[var]]/100, data$pres)), units)
        }
        data[c("x", ys)]
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromTWetBulb
#' @export
# StatWetBulb {{{
StatWetBulb <- ggproto(
    "StatWetBulb", Stat,

    extra_params = c("na.rm"),

    required_aes = c("x", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]
        for (var in ys) {
            data[[var]] <- amplify_hum(with_units(units, GetHumRatioFromTWetBulb(data$x, data[[var]], data$pres)), units)
        }
        data[c("x", ys)]
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromVapPres
#' @export
# StatVapPres {{{
StatVapPres <- ggproto(
    "StatVapPres", Stat,

    extra_params = c("na.rm"),

    required_aes = c("x", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]
        for (var in ys) {
            data[[var]] <- amplify_hum(with_units(units, GetHumRatioFromVapPres(data[[var]], data$pres)), units)
        }

        if ("x" %in% names(data)) {
            data[c("x", ys)]
        } else {
            data[ys]
        }
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# StatSpecVol {{{
StatSpecVol <- ggproto(
    "StatSpecVol", Stat,

    extra_params = c("na.rm"),

    required_aes = c("x", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]
        for (var in ys) {
            data[[var]] <- amplify_hum(with_units(units, GetHumRatioFromAirVolume(data$x, data[[var]], data$pres)), units)
        }
        data[c("x", ys)]
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

    extra_params = c("na.rm"),

    required_aes = c("x", "pres", "units"),

    compute_group = function (self, data, scales) {
        units <- get_units(data)
        ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]
        for (var in ys) {
            data[[var]] <- amplify_hum(with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(data[[var]], data$x)), units)
        }

        data[c("x", ys)]
    }
)
# }}}
