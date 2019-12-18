# add_relhum {{{
add_relhum <- function (min = 0.0, max = 1.0, step = 0.1, n = 101, max.color = "red", ...) {
    rh <- seq(min, max, step)
    l <- vector("list", length(rh))
    l[[1]] <- geom_relhum(aes(relhum = min), n = n, size = theme_get()$line$size * 3, ...)
    l[[length(l)]] <- geom_relhum(aes(relhum = max), n = n, size = theme_get()$line$size * 3, color = max.color, ...)
    l[-c(1, length(l))] <- lapply(rh[-c(1, length(l))], function (x) geom_relhum(aes(relhum = x), n = n, ...))
    l
}
# }}}

# geom_relhum {{{
#' @export
geom_relhum <- function (mapping = aes(), data = NULL, stat = StatRelHum,
                         position = "identity", n = 101, ..., na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE) {

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRelHum,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            n = n,
            na.rm = na.rm,
            ...
        )
    )
}
# }}}
