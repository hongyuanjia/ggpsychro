# stat_relhum {{{
#' @export
stat_relhum <- function (mapping = NULL, data = NULL, relhum, n = 101, geom = "line",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
    layer(
        stat = StatRelHum, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(relhum = relhum, n = n, na.rm = na.rm, ...)
    )
}
# }}}

# StatRelHum {{{
#' @importFrom psychrolib GetRelHumFromHumRatio
#' @importFrom checkmate assert_count assert_number
StatRelHum <- ggproto(
    "StatRelHum", Stat,

    extra_params = c("relhum", "n", "na.rm"),

    required_aes = c("tdb_min", "tdb_max", "tdb_step", "pres"),

    setup_data = function (data, params) {
        assert_number(params$relhum, lower = 0.0, upper = 1.0, .var.name = "relhum")
        assert_count(params$n, positive = TRUE)

        # keep unique
        du <- unique(data[c("tdb_min", "tdb_max", "tdb_step", "pres", "PANEL", "group")])

        # tdb is the same accoss all relhum
        tdb <- seq(du$tdb_min[[1L]], du$tdb_max[[1L]], length.out = params$n)

        # merge
        data <- lapply(seq_len(nrow(du)), function (i) new_data_frame(c(du[i, ], list(tdb = tdb))))
        data <- do.call(rbind, data)

        # calcualte hum ratio
        data$hum <- with_units(GetHumRatioFromRelHum(data$tdb, params$relhum, data$pres))

        data
    },

    compute_group = function(self, data, scales) {
        data <- data[c("tdb", "hum")]
        colnames(data) <- c("x", "y")
        data
    }
)
# }}}
