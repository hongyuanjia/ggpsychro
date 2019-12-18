# stat_humratio {{{
#' @export
stat_humratio <- function (mapping = NULL, data = NULL, hum, geom = "segment",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
    layer(
        stat = StatHumRatio, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(hum = hum, na.rm = na.rm, ...)
    )
}
# }}}

# StatHumRatio {{{
StatHumRatio <- ggproto(
    "Stathumratio", Stat,

    extra_params = c("hum", "na.rm"),

    required_aes = c("tdb_max", "pres"),

    compute_group = function (data, scales, hum) {
        assert_number(hum, lower = 0.0, upper = 0.05)

        new_data_frame(list(
            x = data$tdb_max[[1L]],
            xend = with_units(GetTDewPointFromHumRatio(data$tdb_max[[1L]], hum, data$pres[[1L]])),
            y = hum, yend = hum
        ))
    }
)
# }}}
