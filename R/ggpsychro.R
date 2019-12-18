# ggpsychro {{{
#' @import ggplot2
#' @importFrom checkmate assert_numeric
#' @importFrom psychrolib GetStandardAtmPressure
#' @export
ggpsychro <- function (data = NULL, mapping = aes(),
                       tdb_lim = c(0, 50), tdb_step = waiver(),
                       hum_lim = c(0, 0.05), hum_step = waiver(),
                       pres = waiver(), units = "SI") {
    init_units(units)

    assert_numeric(tdb_lim, any.missing = FALSE, all.missing = FALSE, len = 2,
        unique = TRUE, sorted = TRUE, lower = -50.0, upper = 100.0
    )
    assert_numeric(hum_lim, any.missing = FALSE, all.missing = FALSE, len = 2,
        unique = TRUE, sorted = TRUE, lower = 0.0, upper = 0.05
    )

    # calculate breaks
    if (is.waive(tdb_step)) {
        xb <- scales::pretty_breaks()(tdb_lim)
        tdb_step <- diff(xb)[[1L]]
    } else {
        assert_number(tdb_step, lower = 0.0, upper = diff(tdb_lim))
        xb <- seq(tdb_lim[[1L]], tdb_lim[[2L]], tdb_step)
    }

    if (is.waive(hum_step)) {
        yb <- scales::pretty_breaks()(hum_lim)
        hum_step <- diff(yb)[[1L]]
    } else {
        assert_number(hum_step, lower = 0.0, upper = diff(hum_lim))
        yb <- seq(hum_lim[[1L]], hum_lim[[2L]], hum_step)
    }

    # calculate pressure
    if (is.waive(pres)) pres <- with_units(GetStandardAtmPressure(0))
    assert_number(pres, lower = 0.0)

    # add axis labels
    lbs <- with_units({
        if (psychrolib::isIP()) {
            list(xlab(expression("Dry-bulb temperature ("*degree*F*")")),
                 ylab(expression("Humidity ratio ("*lb[m]*"/"*lb[da]*")"))
            )
        } else {
            list(xlab(expression("Dry-bulb temperature ("*degree*C*")")),
                 ylab(expression("Humidity ratio ("*kg[m]*"/"*kg[da]*")"))
            )
        }
    })

    # add dry-bulb and humdity ratio range and pressure as aes
    more_aes <- list(tdb_min = tdb_lim[[1L]], tdb_max = tdb_lim[[2L]], tdb_step = tdb_step,
                     hum_min = hum_lim[[1L]], hum_max = hum_lim[[2L]], hum_step = hum_step,
                     pres = pres)

    if ("relhum" %in% names(mapping)) {
        stop("Aesthetics 'relhum' can only be specified using 'geom_relhum()' or 'stat_relhum()'.")
    }

    mapping <- do.call(aes, c(mapping, more_aes))

    ggplot(data = data, mapping = mapping, environment = parent.frame()) + lbs +
        # apply theme
        theme_psychro() +

        # set plot axes limit
        coord_cartesian(
            xlim = c(tdb_lim[[1L]] * (1 - 0.005), tdb_lim[[2L]] * (1 + 0.005)),
            ylim = c(hum_lim[[1L]] * (1 - 0.005), hum_lim[[2L]] * (1 + 0.005)),
            expand = FALSE, default = TRUE
        ) +

        # move y label to the right
        scale_y_continuous(position = "right") +

        # saturation line
        stat_relhum(relhum = 1, size = theme_get()$line$size * 3)
}
# }}}

# package option
GGPSYCHRO_OPT <- new.env(parent = emptyenv())
GGPSYCHRO_OPT$units <- getOption("ggpsychro.units", NA_character_)

# init_units {{{
#' @importFrom psychrolib SetUnitSystem
init_units <- function (units) {
    psy_op <- psychrolib:::PSYCHRO_OPT
    # for validation
    psychrolib::SetUnitSystem(units)
    # reset
    psy_op$units <- NA_character_

    GGPSYCHRO_OPT$units <- psychrolib::GetUnitSystem()
}
# }}}

# with_units {{{
#' @importFrom psychrolib SetUnitSystem
with_units <- function (expr) {
    psychrolib::SetUnitSystem(GGPSYCHRO_OPT$units)
    psy_op <- psychrolib:::PSYCHRO_OPT
    on.exit(psy_op$UNITS <- NA_character_, add = TRUE)

    force(expr)
}
# }}}
