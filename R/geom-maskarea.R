#' Mask all area outside of saturation line
#'
#' `geom_maskarea()` draws a polygon to mask all area outside of the saturation
#' line. The vertices of polygon are based on current psychrometric chart's
#' dry-bulb temperature (x axis) range and humidity ratio (y axis) range.
#'
#' `geom_maskarea()` is based on [ggplot2::geom_polygon()], so you can further
#' customize the area style in the same way.
#'
#' Normally there is no need to add another mask since [ggpsychro()]
#' calls `geom_maskarea()` internally and makes sure that it is always rendered
#' after other layers.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#'
#' @param units A single string indicating the units sytem to use. Should be
#'        either `"SI"` or `"IP" or `waiver()` which uses the value from the
#'        parent plot. Default: `waiver()`.
#' @param pres A single number indicating the atmosphere pressure in Pa \\[SI\\] or
#'        Psi \\[IP\\]. If `waiver()`, the pressure calculated from the parent
#'        plot's altitude value will be used. Default: `waiver()`.
#' @param n Number of points to interpolate along
#'
#' @section Aesthetics:
#'
#' `geom_maskarea()` understands the following aesthetics (required aesthetics
#' are in bold).
#'
#' - `color`
#' - `size`
#' - `linetype`
#'
#' @examples
#' # by default, a mask is automatically added when calling 'ggpsychro()' function
#' ggpsychro()
#'
#' # replace with another mask area for pressure at 102000
#' ggpsychro() +
#'     geom_maskarea(units = "SI", pres = 102000)
#'
#' # the line style can be further customized like 'ggplot2::geom_line()'
#' ggpsychro() +
#'     geom_maskarea(units = "SI", pres = 101325, color = "blue", fill = "green")
#'
#' @export
# geom_maskarea {{{
geom_maskarea <- function (mapping = NULL, data = NULL, units = waiver(), pres = waiver(),
                           n = 201, ..., na.rm = FALSE) {
    warn_has_input(mapping, data)
    assert_unit(units)
    assert_pressure(pres)
    assert_count(n, positive = TRUE)

    psychro_layer("PsyLayerMaskArea",
        data = data, mapping = mapping, stat = "relhum", geom = GeomMaskArea,
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(n = n, units = units, pres = pres, na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @importFrom ggplot2 GeomPolygon
#' @export
# GeomMaskArea {{{
GeomMaskArea <- ggproto("GeomMaskArea", GeomPolygon,
    required_aes = c("x", "y", "units", "pres", "n"),

    draw_panel = function(self, data, panel_params, coord) {
        # clean
        data$n <- NULL
        data$pres <- NULL
        data$units <- NULL
        data$relhum <- NULL

        # get coord ranges
        ranges <- coord$backtransform_range(panel_params)

        # include the first and the last point
        data0 <- data[1, ]
        data0$x <- ranges$x[[1L]]
        data0$y <- ranges$y[[2L]]

        data1 <- data[nrow(data), ]
        data1$x <- ranges$x[[2L]]
        data1$y <- ranges$y[[2L]]

        data <- rbind(data0, data, data1)

        coord$tranform

        GeomPolygon$draw_panel(data, panel_params, coord)
    },

    default_aes = aes(colour = "white", fill = "white", size = 0.5, linetype = 1,
        alpha = 1.0
    )
)
# }}}
