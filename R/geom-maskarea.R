# geom_maskarea {{{
#' Mask all area outside of saturation line
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#'
#' @section Aesthetics:
#' `geom_maskarea` understands the following aesthetics (required aesthetics are
#' in bold).
#'
#' - **`n`**
#' - `color`
#' - `size`
#' - `linetype`
#'
#' @export
geom_maskarea <- function (mapping = NULL, data = NULL, n = 201, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomMaskArea,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...)
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
    draw_panel = function(self, data, panel_params, coord) {
        # always set alpha to 1.0
        data$alpha <- 1.0

        # get units
        units <- get_units(data)

        # check pressure
        pres <- get_pres(data)

        # check n
        n <- unique(data$n)
        assert_count(n, positive = TRUE)

        # get coord ranges
        ranges <- coord$backtransform_range(panel_params)

        # create tdb seqs
        tdb <- seq(ranges$x[[1L]], ranges$x[[2L]], length.out = data$n[[1L]])
        # calculate hum ratio at saturation
        hum <- amplify_hum(with_units(units, GetHumRatioFromRelHum(tdb, 1.0, pres)), units)
        # include the first and the last
        tdb <- c(ranges$x[[1L]], tdb, ranges$x[[2L]])
        hum <- c(ranges$y[[2L]], hum, ranges$y[[2L]])

        # combine data
        data <- unique(data[c("PANEL", "group", names(self$default_aes))])
        d <- lapply(data$PANEL, function (pnl) new_data_frame(list(x = tdb, y = hum, PANEL = rep(pnl, length(tdb)))))
        d <- do.call(rbind, d)
        data <- merge(d, data, by = "PANEL")

        GeomPolygon$draw_panel(data, panel_params, coord)
    },

    required_aes = c("pres", "n", "units"),

    default_aes = aes(colour = "NA", fill = "white", size = 0.5, linetype = 1,
        alpha = 1.0
    )
)
# }}}
