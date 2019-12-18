# add_humratio {{{
add_humratio <- function (hum_lim = c(0.0, 0.05), hum_step = waiver(), ...) {
    hum_lim <- check_hum_limits(hum_lim)
    yb <- check_hum_step(hum_lim, hum_step)

    l <- vector("list", length(yb$breaks))
    l[[1]] <- geom_humratio(hum = hum_lim[[1L]], size = theme_get()$line$size * 3, ...)
    l[[length(l)]] <- geom_humratio(hum = hum_lim[[2L]], size = theme_get()$line$size * 3, ...)
    l[-c(1, length(l))] <- lapply(yb$breaks[-c(1, length(l))], function (x) geom_humratio(hum = x, ...))
    l
}
# }}}

# geom_humratio {{{
#' @export
geom_humratio <- function (mapping = aes(), data = NULL, ..., hum, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {

    layer(
        data = data,
        mapping = mapping,
        stat = StatHumRatio,
        geom = "segment",
        position = "identity",
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            hum = hum,
            na.rm = na.rm,
            ...
        )
    )
}
# }}}
