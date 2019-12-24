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
#' @param n Number of points to interpolate along
#'
#' @section Aesthetics:
#'
#' `geom_line_sat()` is drawing using [ggplot2::geom_line()] so support the
#' same aesthetics: `alpha`, `color`, `linetype` and `size`. It also has
#' aesthetics that control the calculation of the saturation line points
#' (required aesthetics are in bold):
#'
#' - **`units`**: the unit system to be used. Should be either `"SI"` or `"IP"`.
#' - **`pres`**: the atmosphere pressure in Psi [IP] or Pa [SI]
#'
#' @seealso [ggpsychro()]
#' @examples
#' # by default, a saturation line is automatically added when calling 'ggpsychro()' function
#' ggpsychro()
#'
#' # it can also be used in a normal ggplot2 object once the coordinate is set
#' ggplot() +
#'     geom_line_sat(aes(units = "SI", pres = 101325))
#'
#' # the line style can be further customized like 'ggplot2::geom_line()'
#' ggplot() +
#'     geom_line_sat(aes(units = "SI", pres = 101325), color = "blue", size = 2)
#'
#' @export
# geom_line_sat {{{
geom_line_sat <- function (mapping = NULL, data = NULL, n = 201, ..., na.rm = FALSE, inherit.aes = TRUE) {
    psychro_layer(data = data, mapping = mapping, stat = "identity", geom = GeomLineSat,
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

# get_grid_type {{{
get_grid_type <- function (layer) {
    nm <- names(.grid_type)[.grid_type %in% class(layer$geom)]
    if (!length(nm)) return(NA_character_)
    paste0("grid_", nm)
}
# }}}

# .grid_type {{{
.grid_type <- c(
    relhum = "GeomGridRelHum",
    wetbulb = "GeomGridWetBulb",
    vappres = "GeomGridVapPres",
    specvol = "GeomGridSpecVol",
    enthalpy = "GeomGridEnthalpy"
)
# }}}
