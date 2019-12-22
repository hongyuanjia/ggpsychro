#' Draw constant psychrometric properties grids
#'
#' @details
#'
#' * `geom_line_sat` for saturation line.
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
# geom_line_sat {{{
geom_line_sat <- function (mapping = NULL, data = NULL, n = 201, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomLineSat,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...)
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_relhum {{{
#' @export
geom_grid_relhum <- function (mapping = NULL, data = NULL, step = 10, n = 201, label = NULL, label_loc = NULL,
                              label_fun = NULL, label_parse = FALSE, label_style = NULL,
                              check_overlap = FALSE, ..., na.rm = FALSE, inherit.aes = TRUE) {
    list(
        geom = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridLineRelHum,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(step = step, n = n, na.rm = na.rm, ...)
        ),
        text = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridTextRelHum,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(step = step, label = label, label_loc = label_loc, label_fun = label_fun,
                label_parse = label_parse, label_style = label_style,
                check_overlap = check_overlap, na.rm = na.rm, ...)
        )
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_wetbulb {{{
#' @export
geom_grid_wetbulb <- function (mapping = NULL, data = NULL, step = 10, label = NULL, label_loc = NULL,
                               label_fun = NULL, label_parse = FALSE, label_style = NULL,
                               check_overlap = FALSE, ..., na.rm = FALSE, inherit.aes = TRUE) {
    list(
        geom = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridLineWetBulb,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(n = 50, step = step, na.rm = na.rm, ...)
        ),
        text = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridTextWetBulb,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(step = step, label = label, label_loc = label_loc, label_fun = label_fun,
                label_parse = label_parse, label_style = label_style,
                check_overlap = check_overlap, na.rm = na.rm, ...)
        )
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_vappres {{{
#' @export
geom_grid_vappres <- function (mapping = NULL, data = NULL, step = 500, label = NULL, label_loc = NULL,
                               label_fun = NULL, label_parse = FALSE, label_style = NULL,
                               check_overlap = FALSE, ..., na.rm = FALSE, inherit.aes = TRUE) {
    list(
        geom = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridLineVapPres,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(step = step, n = 2, na.rm = na.rm, ...),
        ),
        text = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridTextVapPres,
              position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
              params = list(step = step, label = label, label_loc = label_loc, label_fun = label_fun,
                  label_parse = label_parse, label_style = label_style,
                  check_overlap = check_overlap, na.rm = na.rm, ...)
        )
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_specvol {{{
#' @export
geom_grid_specvol <- function (mapping = NULL, data = NULL, step = 0.01, label = NULL, label_loc = NULL,
                               label_fun = NULL, label_parse = FALSE, label_style = NULL,
                               check_overlap = FALSE, ..., na.rm = FALSE, inherit.aes = TRUE) {
    list(
        geom = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridLineSpecVol,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(step = step, n = 50, na.rm = na.rm, ...),
        ),
        text = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridTextSpecVol,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(step = step, label = label, label_loc = label_loc, label_fun = label_fun,
                label_parse = label_parse, label_style = label_style,
                check_overlap = check_overlap, na.rm = na.rm, ...)
        )
    )
}
# }}}

#' @export
#' @rdname geom_grid
# geom_grid_enthalpy {{{
#' @export
geom_grid_enthalpy <- function (mapping = NULL, data = NULL, step = 20, label = NULL, label_loc = NULL,
                                label_fun = NULL, label_parse = FALSE, label_style = NULL,
                                check_overlap = FALSE, ..., na.rm = FALSE, inherit.aes = TRUE) {
    list(
        geom = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridLineEnthalpy,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(step = step, n = 50, na.rm = na.rm, ...)
        ),
        text = layer(data = data, mapping = mapping, stat = "identity", geom = GeomGridTextEnthalpy,
            position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
            params = list(step = step, label = label, label_loc = label_loc, label_fun = label_fun,
                label_parse = label_parse, label_style = label_style,
                check_overlap = check_overlap, na.rm = na.rm, ...)
        )
    )
}
# }}}
