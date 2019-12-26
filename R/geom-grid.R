#' Draw saturation line
#'
#' `geom_line_sat()` draws a saturation line based on current psychrometric
#' chart's dry-bulb temperature (x axis) range and humidity ratio (y axis)
#' range.
#'
#' `geom_line_sat()` is based on [ggplot2::geom_line()], so you can further
#' customize the line style in the same way.
#'
#' Normally there is no need to add another saturation line since [ggpsychro()]
#' calls `geom_line_sat()` internally and makes sure that it is always rendered
#' at the last.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#'
#' @param units A single string indicating the units sytem to use. Should be
#'        either `"SI"` or `"IP" or `waiver()` which uses the value from the
#'        parent plot. Default: `waiver()`
#' @param pres A single number indicating the atmosphere pressure in Pa [SI] or
#'        Psi [IP]. If `waiver()`, the pressure calculated from the parent
#'        plot's altitude value will be used. Default: `waiver()`
#' @param n Number of points to interpolate along
#'
#' @section Aesthetics:
#'
#' `geom_line_sat()` is drawing using [ggplot2::geom_line()] so support the
#' same aesthetics: `alpha`, `color`, `linetype` and `size`. It also has
#' aesthetics that control the calculation of the saturation line points
#' (required aesthetics are in bold):
#'
#' @seealso [ggpsychro()]
#' @examples
#' # by default, a saturation line is automatically added when calling 'ggpsychro()' function
#' ggpsychro()
#'
#' # you can add another saturation line
#' ggpsychro() + geom_line_sat(units = "SI", pres = 101325, color = "blue", size = 2)
#'
#' @export
# geom_line_sat {{{
geom_line_sat <- function (mapping = NULL, data = NULL, units = waiver(), pres = waiver(),
                           n = 201, ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_count(n, positive = TRUE)

    psychro_layer("PsyLayerLineSat",
        data = data, mapping = mapping, stat = "relhum", geom = GeomLineSat,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = n, units = units, pres = pres, na.rm = na.rm, ...)
    )
}
# }}}

#' Draw grid lines of constant psychrometric properties
#'
#' `geom_grid_*()` geoms draw grid line of constant psychrometric properties,
#' including relative humidity, wet-bulb temperature, water vapor pressure,
#' specific volume and enthalpy, based on current psychrometric chart's dry-bulb
#' temperature range and humidity ratio range.
#'
#' * `geom_grid_relhum()` for relative humidity grid in range [0, 100] in %
#' * `geom_grid_wetbulb()` for wet-bulb temperature grid in °F [IP] or °C [SI]
#' * `geom_grid_vappres()` for partial pressure grid of water vapor in Psi [IP]
#'   or Pa [SI]
#' * `geom_grid_specvol()` for specific volume grid in ft3 lb-1 of dry air [IP] or
#'    in m3 kg-1 of dry air [SI]
#' * `geom_grid_enthalpy()` for moist air enthalpy grid in Btu lb-1 [IP] or kJ kg-1
#'
#' Each `geom_grid_*()` comes along with a corresponding
#' [`scale_*()`][scale_relhum()] function for customizing scale
#' properties, including breaks, labels and etc.
#'
#' For each psychrometric properties, the maximum and minimum value is
#' calculated based on the ranges of dry-bulb temperature and humidity ratio in
#' the coordinate system.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param n Number of points to interpolate along. Only used in
#'        `geom_grid_relhum()`.
#' @param label_loc A single number in range (0, 1) indicating label position
#'        relative to the line. The default values aim to reduce overlappings
#'        among differnent psychrometric property lines, but you can change them
#'        if needed:
#' * `0.95` for constant relative humidity lines
#' * `0.10` for constant wet-bulb temperature lines
#' * `0.50` for constant water vapor pressure lines
#' * `0.95` for constant specific volume lines
#' * `0.95` for constant enthalpy lines
#'
#' @param label_parse If `TRUE`, the labels will be parsed into expressions and
#'        displayed as described in `?plotmath`.
#'
#' @section Alignment:
#'
#' You can modify text alignment with the `vjust` and `hjust`
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character (`"left"`, `"middle"`, `"right"`, `"bottom"`,
#' `"center"`, `"top"`). There are two special alignments: `"inward"` and
#' `"outward"`. Inward always aligns text towards the center, and outward
#' aligns it away from the center.
#'
#' @importFrom ggplot2 layer GeomLine
#'
#' @section Aesthetics:
#'
#' `geom_grid_*()` understands the following aesthetics.
#'
#' - `color`
#' - `size`
#' - `linetype`
#' - `alpha`
#' - `label.colour`
#' - `label.size`
#' - `label.angle`
#' - `label.hjust`
#' - `label.vjust`
#' - `label.alpha`
#' - `label.family`
#' - `label.fontface`
#' - `label.lineheight`
#'
#' @rdname geom_grid
#' @export
# geom_grid_relhum {{{
geom_grid_relhum <- function(mapping = NULL, data = NULL, n = 201, label_loc = 0.95,
                             label_parse = FALSE, units = waiver(), pres = waiver(),
                             ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_count(n, positive = TRUE)
    assert_number(label_loc, lower = 0.0, upper = 1.0, na.ok = TRUE)

    psychro_layer("PsyLayerGrid",
        data = data, mapping = mapping, stat = "relhum", geom = GeomGridRelHum,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = n, label_loc = label_loc, label_parse = label_parse,
            units = units, na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname geom_grid
#' @export
# geom_grid_wetbulb {{{
geom_grid_wetbulb <- function (mapping = NULL, data = NULL, label_loc = 0.1,
                               label_parse = TRUE, units = waiver(), pres = waiver(),
                               ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_number(label_loc, lower = 0.0, upper = 1.0, na.ok = TRUE)

    psychro_layer("PsyLayerGrid",
        data = data, mapping = mapping, stat = "wetbulb", geom = GeomGridWetBulb,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = 50, label_loc = label_loc, label_parse = label_parse,
            units = units, pres = pres, na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname geom_grid
#' @export
# geom_grid_vappres {{{
geom_grid_vappres <- function (mapping = NULL, data = NULL, label_loc = 0.5,
                               label_parse = FALSE, units = waiver(), pres = waiver(),
                               ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_number(label_loc, lower = 0.0, upper = 1.0, na.ok = TRUE)

    psychro_layer("PsyLayerGrid",
        data = data, mapping = mapping, stat = "vappres", geom = GeomGridVapPres,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = 2, label_loc = label_loc, label_parse = label_parse,
            units = units, pres = pres, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_specvol {{{
geom_grid_specvol <- function (mapping = NULL, data = NULL, label_loc = 0.95,
                               label_parse = TRUE, units = waiver(), pres = waiver(),
                               ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_number(label_loc, lower = 0.0, upper = 1.0, na.ok = TRUE)

    psychro_layer("PsyLayerGrid",
        data = data, mapping = mapping, stat = "specvol", geom = GeomGridSpecVol,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = 50, label_loc = label_loc, label_parse = label_parse,
            units = units, pres = pres, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_enthalpy {{{
geom_grid_enthalpy <- function (mapping = NULL, data = NULL, label_loc = 0.95,
                                label_parse = TRUE, units = waiver(), pres = waiver(),
                                ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_number(label_loc, lower = 0.0, upper = 1.0, na.ok = TRUE)

    psychro_layer("PsyLayerGrid",
        data = data, mapping = mapping, stat = "enthalpy", geom = GeomGridEnthalpy,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = 50, label_loc = label_loc, label_parse = label_parse,
            units = units, pres = pres, na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# GeomLineSat {{{
GeomLineSat <- ggproto("GeomLineSat", GeomLine,
    required_aes = c("x", "y", "units", "pres", "n"),

    draw_panel = function(self, data, panel_params, coord) {
        # clean
        data$n <- NULL
        data$pres <- NULL
        data$units <- NULL
        data$relhum <- NULL

        # str(coord$transform(data$y))
        GeomLine$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "#DA251D", size = 1, linetype = 1, alpha = NA)
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @export
# GeomGridRelHum {{{
GeomGridRelHum <- ggproto("GeomGridRelHum", GeomPath,
    required_aes = c("x", "y", "n", "pres", "units", "label_loc"),

    extra_params = c("label", "na.rm"),

    default_aes = aes(
        colour = "#00126B", size = 0.5, linetype = "twodash", alpha = 0.85,
        label.colour = "#00126B", label.size = 3.2, label.angle = 0, label.hjust = 1.0,
        label.vjust = -0.3, label.alpha = 0.85, label.family = "", label.fontface = 1, label.lineheight = 1.2
    ),
    setup_data = function (self, data, params) {
        data[order(data$PANEL, data$group, data$relhum), ]
    },

    draw_panel = function(self, data, panel_params, coord, label_loc = NULL,
                          label_parse = FALSE, label.color = "#00126B",
                          label.size = 3.2, label.angle = 0, label.hjust = 1.0,
                          label.vjust = -0.3, label.alpha = 0.85, label.family = "",
                          label.fontface = 1, label.lineheight = 1.2) {
        build_grid_panel_grob("relhum", self, data, panel_params, coord, label_loc, label_parse)
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetTWetBulbFromHumRatio
#' @export
# GeomGridWetBulb {{{
GeomGridWetBulb <- ggproto("GeomGridWetBulb", GeomLine,
    required_aes = c("x", "y", "wetbulb", "n", "pres", "units", "label_loc"),

    extra_params = c("label", "na.rm"),

    default_aes = aes(
        colour = "#015756", size = 0.5, linetype = "twodash", alpha = 0.85,
        label.colour = "#015756", label.size = 3.2, label.angle = 0, label.hjust = 0.0,
        label.vjust = -0.3, label.alpha = 0.85, label.family = "", label.fontface = 1,
        label.lineheight = 1.2
    ),

    setup_data = function (self, data, params) {
        data[order(data$PANEL, data$group, data$wetbulb), ]
    },

    draw_panel = function(self, data, panel_params, coord, label_loc = NULL,
                          label_parse = TRUE, label.color = "#015756",
                          label.size = 3.2, label.angle = 0, label.hjust = 0.0,
                          label.vjust = -0.3, label.alpha = 0.85, label.family = "",
                          label.fontface = 1, label.lineheight = 1.2) {
        build_grid_panel_grob("wetbulb", self, data, panel_params, coord, label_loc, label_parse)
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetVapPresFromHumRatio
#' @export
# GeomGridVapPres {{{
GeomGridVapPres <- ggproto("GeomGridVapPres", GeomLine,
    required_aes = c("x", "y", "vappres", "n", "pres", "units", "label_loc"),

    extra_params = c("label", "na.rm"),

    default_aes = aes(
        colour = "#941919", size = 0.5, linetype = 2, alpha = 0.85,
        label.colour = "#941919", label.size = 3.2, label.angle = 0, label.hjust = 0.0,
        label.vjust = -0.3, label.alpha = 0.85, label.family = "", label.fontface = 1,
        label.lineheight = 1.2
    ),

    setup_data = function (data, params) {
        data[order(data$PANEL, data$group, data$vappres), ]
    },

    draw_panel = function(self, data, panel_params, coord, label_loc = NULL,
                          label_parse = FALSE, label.color = "#941919",
                          label.size = 3.2, label.angle = 0, label.hjust = 0.0,
                          label.vjust = -0.3, label.alpha = 0.85, label.family = "",
                          label.fontface = 1, label.lineheight = 1.2) {
        build_grid_panel_grob("vappres", self, data, panel_params, coord, label_loc, label_parse)
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetMoistAirVolume
#' @export
# GeomGridSpecVol {{{
GeomGridSpecVol <- ggproto("GeomGridSpecVol", GeomLine,
    required_aes = c("x", "y", "specvol", "n", "pres", "units", "label_loc"),

    extra_params = c("label", "na.rm"),

    default_aes = aes(
        colour = "#108860", size = 0.5, linetype = 1, alpha = 0.85,
        label.colour = "#108860", label.size = 3.2, label.angle = 0, label.hjust = 1.0,
        label.vjust = -0.3, label.alpha = 0.85, label.family = "", label.fontface = 1,
        label.lineheight = 1.2
    ),

    setup_data = function (self, data, params) {
        data[order(data$PANEL, data$group, data$specvol), ]
    },

    draw_panel = function(self, data, panel_params, coord, label_loc = NULL,
                          label_parse = FALSE, label.color = "#108860",
                          label.size = 3.2, label.angle = 0, label.hjust = 0.0,
                          label.vjust = -0.3, label.alpha = 0.85, label.family = "",
                          label.fontface = 1, label.lineheight = 1.2) {
        build_grid_panel_grob("specvol", self, data, panel_params, coord, label_loc, label_parse)
    }
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromEnthalpyAndTDryBulb GetMoistAirEnthalpy
#' @export
# GeomGridEnthalpy {{{
GeomGridEnthalpy <- ggproto("GeomGridEnthalpy", GeomLine,
    required_aes = c("x", "y", "enthalpy", "n", "pres", "units", "label_loc"),

    extra_params = c("label", "na.rm"),

    default_aes = aes(
        colour = "#633F87", size = 0.5, linetype = "twodash", alpha = 0.85,
        label.colour = "#633F87", label.size = 3.2, label.angle = 0, label.hjust = 1.0,
        label.vjust = -0.3, label.alpha = 0.85, label.family = "", label.fontface = 1,
        label.lineheight = 1.2
    ),

    setup_data = function (self, data, params) {
        data[order(data$PANEL, data$group, data$enthalpy), ]
    },

    draw_panel = function(self, data, panel_params, coord, label_loc = NULL,
                          label_parse = TRUE, label.color = "#633F87",
                          label.size = 3.2, label.angle = 0, label.hjust = 1.0,
                          label.vjust = -0.3, label.alpha = 0.85, label.family = "",
                          label.fontface = 1, label.lineheight = 1.2) {
        build_grid_panel_grob("enthalpy", self, data, panel_params, coord, label_loc, label_parse)
    }
)
# }}}

# init_grid_data {{{
init_grid_data <- function (data) {
    # temporarily set x and y
    data$x <- 0
    data$y <- 0

    data
}
# }}}

#' @importFrom checkmate assert_count
#' @importFrom grid gList nullGrob
# add_x_intercept {{{
add_x_intercept <- function (data) {
    # extend curve to the bottom axis
    data <- do.call(rbind, lapply(split(data, data$group), function (d) {
        zero <- d[1L, ]
        # calculate slope
        slp <- slope(d$x[[1L]], d$y[[1L]], d$x[[nrow(d)]], d$y[[nrow(d)]])
        intercept <- zero$y - slp * zero$x
        zero$x <- (0.0 - intercept) / slp
        zero$y <- 0.0

        rbind(d, zero)
    }))
}
# }}}

#' @importFrom checkmate assert_number
#' @importFrom grid gpar textGrob
#' @importFrom psychrolib GetTDryBulbFromMoistAirVolumeAndHumRatio GetTDryBulbFromEnthalpyAndHumRatio
# build_grid_panel_grob {{{
build_grid_panel_grob <- function (type, self, data, panel_params, coord,
                                   label_loc = NULL, label_parse = FALSE) {
    # specify group per constant line
    rleid <- rle(data[[type]])
    data$group <- unlist(Map(rep, seq_len(length(rleid$lengths)), rleid$lengths))

    # only keep rows where hum ratio is larger than 0.01
    data <- data[data$y > 0.01 / 1000, ]

    # extend line to the bottom axis
    if (type %in% c("wetbulb", "specvol", "enthalpy")) data <- add_x_intercept(data)

    # create lines
    line_grob <- GeomLine$draw_panel(data, panel_params, coord)

    # escape if no labels
    if (is.na(label_loc)) return(line_grob)
    if (all(is.na(data$label))) return(line_grob)

    units <- get_units(data)
    pres <- get_pres(data)

    # calculate label location
    data <- cal_label_loc(type, coord, panel_params, data, label_loc, pres, units)

    # parse label necessary
    if (!label_parse) {
        label <- data$label
    } else {
        label <- vector("expression", length(data$label))
        for (i in seq_along(data$label)) {
            expr <- parse(text = data$label[[i]])
            label[[i]] <- if (length(expr) == 0L) NA else expr[[1L]]
        }
    }

    label_grob <- textAlongGrob(
        label, data$x, data$y, default.units = "native",
        hjust = data$label.hjust, vjust = data$label.vjust, rot = data$angle,
        gp = grid::gpar(
            col = alpha(data$label.colour, data$label.alpha),
            fontsize = data$label.size * .pt,
            fontfamily = data$label.family,
            fontface = data$label.fontface,
            lineheight = data$label.lineheight
        ),
        check.overlap = FALSE, rot.type = "along",
        x0 = data$x0, x1 = data$x1, y0 = data$y0, y1 = data$y1
    )

    gList(line_grob, label_grob)
}
# }}}
# cal_text_loc {{{
# cal_label_loc <- function (type, ranges, label, label_loc, pres, units, offset = 0.05) {
cal_label_loc <- function (type, coord, panel_params, data, label_loc, pres, units, offset = 0.05) {
    label_data <- unique(data[setdiff(names(data), c("x", "y"))])
    ranges <- coord$limits

    ab_line <- function (value, slope, intercept, reverse = FALSE) {
        if (!reverse) {
            slope * value + intercept
        } else {
            (value - intercept) / slope
        }
    }

    if (type == "relhum") {
        # relhum {{{
        # calculate dry-bulb value based on label loc
        x <- ranges$x[[1L]] + diff(ranges$x) * label_loc

        x <- rep(x, nrow(label_data))

        x0 <- x * (1 - offset)
        x1 <- x * (1 + offset)

        y <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x, label_data$relhum, pres)), units)
        y0 <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x0, label_data$relhum, pres)), units)
        y1 <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x1, label_data$relhum, pres)), units)

        angle <- line_angle(x0, y0, x1, y1)
        # }}}
    } else if (type == "wetbulb") {
        # wetbulb {{{
        label_data <- label_data[label_data$wetbulb > 0.0, ]
        # start from the saturation line
        x0 <- label_data$wetbulb
        x1 <- x0
        x1[x0 > 0] <- x0[x0 > 0] * (1 + 2 * offset)
        x1[x0 <= 0] <- x0[x0 <= 0] * (1 - 2 * offset)

        y0 <- amplify_hum(with_units(units, GetHumRatioFromTWetBulb(x0, x0, pres)), units)
        y1 <- amplify_hum(with_units(units, GetHumRatioFromTWetBulb(x1, x0, pres)), units)

        # calculate slop
        slp <- slope(x0, y0, x1, y1)
        # calculate intercept
        intercept <- y0 - slp * x0

        # check if start point y0 is out of hum-ratio axis, i.e. out of panel
        out <- y0 > ranges$y[[2L]]
        if (any(out)) {
            y0[out] <- ranges$y[[1L]]
            x0[out] <- ab_line(y0[out], slp[out], intercept[out], TRUE)
        }

        # get the intercept point on the dry-bulb axis
        y1 <- rep(ranges$y[[1L]], length(y1))
        x1 <- ab_line(ranges$y[[1L]], slp, intercept, TRUE)
        # if the x is outside of dry-bulb axis, set the point on the hum-ratio axis
        out <- x1 > ranges$x[[2L]]
        if (any(out)) {
            x1[out] <- ranges$x[[2L]]
            y1[out] <- ab_line(x1[out], slp[out], intercept[out])
        }

        angle <- line_angle(x0, y0, x1, y1)

        # calculate the length
        dis <- dist_euclid(x0, y0, x1, y1) * label_loc

        # calculate the label location
        x <- x0 + dis * cos(angle * pi / 180)
        y <- y0 + dis * sin(angle * pi / 180)
        # }}}
    } else if (type == "vappres") {
        # vappres {{{
        # start from max tdb
        x0 <- rep(ranges$x[[2L]], length(label_data$label))
        x1 <- x0

        # the vapor pressure line is horizontal
        y0 <- amplify_hum(with_units(units, GetHumRatioFromVapPres(label_data$vappres, pres)), units)
        y1 <- y0

        y <- y0

        # get the intercept of saturation line
        x0 <- cal_sat_intercept(ranges, pres, units, y)

        dis <- dist_euclid(x0, y0, x1, y1)
        x <- x0 + dis * label_loc

        angle <- rep(0.0, length(label_data$label))
        # }}}
    } else if (type == "specvol") {
        # specvol {{{
        # start from the bottom lowest hum
        y1 <- rep(ranges$y[[1L]], length(label_data$specvol))
        x1 <- with_units(units, GetTDryBulbFromMoistAirVolumeAndHumRatio(label_data$specvol, y1, pres))
        # relocate point to the y axis
        if (any(out <- x1 > ranges$x[[2L]])) {
            x1[out] <- ranges$x[[2L]]
            y1[out] <- amplify_hum(with_units(units, GetHumRatioFromAirVolume(x1[out], label_data$specvol[out], pres)), units)
        }

        # offset a little bit
        x0 <- x1 * (1 - offset)
        y0 <- amplify_hum(with_units(units, GetHumRatioFromAirVolume(x0, label_data$specvol, pres)), units)

        # calculate angle
        angle <- line_angle(x0, y0, x1, y1)
        # calculate slop
        slp <- slope(x0, y0, x1, y1)
        # calculate intercept
        intercept <- y0 - slp * x0

        # find the intercept on the saturation line
        # no need to be super accurate since this value is only used to locate
        # the label
        x0 <- root_sat_intercept(ranges, pres, units, slp, intercept)
        x0[is.na(x0)] <- 0.0
        y0 <- ab_line(x0, slp, intercept)

        # calculate the length
        dis <- dist_euclid(x0, y0, x1, y1) * label_loc

        # calculate the label location
        x <- x0 + dis * cos(angle * pi / 180)
        y <- y0 + dis * sin(angle * pi / 180)
        # }}}
    } else if (type == "enthalpy") {
        # enthalpy {{{
        # start from the bottom lowest hum
        y1 <- rep(ranges$y[[1L]], length(label_data$enthalpy))
        x1 <- with_units(units, GetTDryBulbFromEnthalpyAndHumRatio(label_data$enthalpy, y1))
        # relocate point to the y axis
        if (any(out <- x1 > ranges$x[[2L]])) {
            x1[out] <- ranges$x[[2L]]
            y1[out] <- amplify_hum(with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(label_data$enthalpy[out], x1[out])), units)
        }

        # offset a little bit
        x0 <- x1 * (1 - offset)
        y0 <- amplify_hum(with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(label_data$enthalpy, x0)), units)

        # calculate angle
        angle <- line_angle(x0, y0, x1, y1)
        # calculate slop
        slp <- slope(x0, y0, x1, y1)
        # calculate intercept
        intercept <- y0 - slp * x0

        # find the intercept on the saturation line
        # no need to be super accurate since this value is only used to locate
        # the label
        x0 <- root_sat_intercept(ranges, pres, units, slp, intercept)
        x0[is.na(x0)] <- 0.0
        y0 <- ab_line(x0, slp, intercept)

        # calculate the length
        dis <- dist_euclid(x0, y0, x1, y1) * label_loc

        # calculate the label location
        x <- x0 + dis * cos(angle * pi / 180)
        y <- y0 + dis * sin(angle * pi / 180)
        # }}}
    }

    y <- narrow_hum(y, units)
    y0 <- narrow_hum(y0, units)
    y1 <- narrow_hum(y1, units)

    # transfrom data into [0,1] range
    data <- coord$transform(new_data_frame(list(x = x, y = y)), panel_params)
    data0 <- coord$transform(new_data_frame(list(x = x0, y = y0)), panel_params)
    names(data0) <- c("x0", "y0")
    data1 <- coord$transform(new_data_frame(list(x = x1, y = y1)), panel_params)
    names(data1) <- c("x1", "y1")

    if (is.character(label_data$label.vjust)) {
        label_data$label.vjust <- compute_just(label_data$label.vjust, data$y)
    }
    if (is.character(label_data$label.hjust)) {
        label_data$label.hjust <- compute_just(label_data$label.hjust, data$x)
    }

    cbind(data, data0, data1, new_data_frame(list(angle = angle)), label_data)
}
# }}}
# compute_just {{{
# copied from tidyverse/ggplot2/R/geom-text.r
compute_just <- function(just, x) {
    inward <- just == "inward"
    just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
    outward <- just == "outward"
    just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

    unname(c(left = 0, center = 0.5, right = 1,
            bottom = 0, middle = 0.5, top = 1)[just])
}
# }}}
# cal_sat_intercept {{{
cal_sat_intercept <- function (ranges, pres, units, y = NULL, slope, intercept, num = 500L) {
    x <- seq(ranges$x[[1L]], ranges$x[[2L]], length.out = num)
    sat_hum <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x, 1.0, pres)), units)

    # remove points out of axis range
    x <- x[sat_hum <= max(y)]
    sat_hum <- sat_hum[sat_hum <= max(y)]

    vapply(y, function (y0) x[which.min(abs(sat_hum - y0))], numeric(1))
}
# }}}
# root_sat_intercept {{{
root_sat_intercept <- function (ranges, pres, units, slope, intercept, tol = 0.01) {
    find_root <- function (slope, intercept) {
        fun <- function (x) {
            slope * x + intercept - amplify_hum(with_units(units, GetHumRatioFromRelHum(x, 1.0, pres)), units)
        }

        # uniroot(fun, ranges$x, tol = tol)
        tryCatch(uniroot(fun, ranges$x, tol = tol),
            error = function (e) list(root = NA_real_))
    }
    Map(find_root, slope = slope, intercept = intercept)
    vapply(Map(find_root, slope = slope, intercept = intercept), "[[", numeric(1), "root")
}
# }}}

# adopted from thomasp85/ggraph/R/textAlong.R
#' @importFrom grid unit textGrob grob is.unit
# textAlongGrob {{{
textAlongGrob <- function(label, x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                          just = 'centre', hjust = NULL, vjust = NULL, rot = 0,
                          check.overlap = FALSE, rot.type = 'rot', x0 = 0,
                          y0 = 0, x1 = 0, y1 = 0, force.rot = TRUE, dodge = NULL,
                          push = NULL, default.units = 'npc', name = NULL, gp = gpar(),
                          vp = NULL) {
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (rot.type == 'rot') {
        textGrob(
            label = label, x = x, y = y, just = just, hjust = hjust,
            vjust = vjust, rot = rot, check.overlap = check.overlap,
            default.units = default.units, name = name, gp = gp, vp = vp
        )
    } else {
        if (!rot.type %in% c('along', 'across')) {
            stop('rot.type must be either `rot`, `along`, or `across`', call. = FALSE)
        }
        if (!is.unit(x0)) {
            x0 <- unit(x0, default.units)
        }
        if (!is.unit(y0)) {
            y0 <- unit(y0, default.units)
        }
        if (!is.unit(x1)) {
            x1 <- unit(x1, default.units)
        }
        if (!is.unit(y1)) {
            y1 <- unit(y1, default.units)
        }
        grob(
            label = label, x = x, y = y, just = just, hjust = hjust,
            vjust = vjust, rot.type = rot.type, x0 = x0, y0 = y0, x1 = x1,
            y1 = y1, force.rot = force.rot, dodge = dodge, push = push,
            check.overlap = check.overlap, name = name, gp = gp, vp = vp,
            cl = 'textalong'
        )
    }
}
# }}}

#' @importFrom grid makeContent
#' @importFrom grid convertX convertY convertHeight grob
#' @export
# makeContent.textalong {{{
makeContent.textalong <- function(x) {
    x0 <- convertX(x$x0, 'mm', TRUE)
    y0 <- convertY(x$y0, 'mm', TRUE)
    x1 <- convertX(x$x1, 'mm', TRUE)
    y1 <- convertY(x$y1, 'mm', TRUE)
    xpos <- convertX(x$x, 'mm', TRUE)
    ypos <- convertY(x$y, 'mm', TRUE)
    angle <- line_angle(x0, y0, x1, y1)
    if (x$rot.type == 'across') {
        angle <- angle - 90
    }
    if (!is.null(x$dodge)) {
        dodge <- convertHeight(x$dodge, 'mm', TRUE)
        dodge_angle <- (angle + 90) / 360 * 2 * pi
        dodge_x <- cos(dodge_angle) * dodge
        dodge_y <- sin(dodge_angle) * dodge
        xpos <- xpos + dodge_x
        ypos <- ypos + dodge_y
    }
    if (!is.null(x$push)) {
        push <- convertHeight(x$push, 'mm', TRUE)
        push_angle <- angle / 360 * 2 * pi
        push_x <- cos(push_angle) * push
        push_y <- sin(push_angle) * push
        xpos <- xpos + push_x
        ypos <- ypos + push_y
    }
    if (x$force.rot) {
        fix <- angle > 90 & angle < 270
        angle[fix] <- angle[fix] + 180
    }
    grob(
        label = x$label, x = unit(xpos, 'mm'), y = unit(ypos, 'mm'),
        just = rep(0, length(x$just)), hjust = x$hjust, vjust = x$vjust, rot = angle,
        # just = x$just, hjust = x$hjust, vjust = x$vjust, rot = angle,
        check.overlap = x$check.overlap, name = x$name, gp = x$gp, vp = x$vp,
        cl = 'text'
    )
}
# }}}

# warn_has_input {{{
warn_has_input <- function (mapping, data) {
    name <- deparse(sys.call(-1))
    if (!is.null(mapping)) {
        warning(sprintf("'%s': 'mapping' is not needed and is ignored for this geom.", name), call. = FALSE)
    }
    if (!is.null(data)) {
        warning(sprintf("'%s': 'data' is not needed and is ignored for this geom.", name), call. = FALSE)
    }
}
# }}}
# assert_unit {{{
assert_unit <- function (units) {
    if (!is.waive(units)) units <- match.arg(units, c("SI", "IP"))
}
# }}}
# assert_pressure {{{
assert_pressure <- function (pres) {
    if (!is.waive(pres)) assert_number(pres, finite = TRUE)
}
# }}}
