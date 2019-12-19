#' Compute relative humidity
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @examples
#' ggpsychro(data.frame(xmin = 20, xmax = 40, relhum = 0.6)) +
#'     stat_relhum(aes(xmin = xmin, xmax = xmax, relhum = relhum),
#'                 geom = "point", n = 20)
#'
#' ggpsychro(data.frame(index = 1:11, xmin = 20, xmax = 40, relhum = (0:10)/10)) +
#'     stat_relhum(aes(xmin = xmin, xmax = xmax, relhum = relhum, group = index),
#'                 color = "red")
#'
#' @export
# stat_relhum {{{
stat_relhum <- function (mapping = NULL, data = NULL, n = 201, geom = "line",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
    layer(
        stat = StatRelHum, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, na.rm = na.rm, ...)
    )
}
# }}}

# StatRelHum {{{
#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @export
StatRelHum <- ggproto(
    "StatRelHum", Stat,

    extra_params = c("n", "na.rm"),

    required_aes = c("xmin", "xmax", "relhum", "pres", "units"),

    compute_group = function(data, scales, n) {
        # get units
        units <- get_units(data)

        # check pressure
        pres <- get_pres(data)

        # check n
        assert_count(n, positive = TRUE)

        # keep unique
        d <- unique(data[c("xmin", "xmax", "relhum")])

        # tdb is the same accoss all relhum
        tdb <- seq(d$xmin[[1L]], d$xmax[[1L]], length.out = n)

        # calcualte hum ratio
        hum <- with_units(units, GetHumRatioFromRelHum(tdb, d$relhum, pres))

        new_data_frame(list(x = tdb, y = amplify_hum(hum, units)))
    }
)
# }}}
