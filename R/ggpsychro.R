# ggpsychro {{{
#' @import ggplot2
#' @importFrom checkmate assert_numeric
#' @export
ggpsychro <- function (data = NULL, mapping = aes(), tdb_lim = c(0, 50), hum_lim = c(0, 0.05), pres = waiver(), unit = "SI") {
    assert_numeric(tdb_lim, any.missing = FALSE, all.missing = FALSE, len = 2,
        unique = TRUE, sorted = TRUE, lower = -50.0, upper = 100.0
    )
    assert_numeric(hum_lim, any.missing = FALSE, all.missing = FALSE, len = 2,
        unique = TRUE, sorted = TRUE, lower = 0.0, upper = 0.05
    )

    # calculate pressure
    if (is.waive(pres)) pres <- with_units(unit, GetStandardAtmPressure(0))
    assert_number(pres, lower = 0.0)

    # add axis labels
    lbs <- with_units(unit, {
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
    more_aes <- list(xmin = tdb_lim[[1]], xmax = tdb_lim[2], ymin = hum_lim[1], ymax = hum_lim[2], pres = pres)
    mapping <- do.call(aes, c(mapping, more_aes))

    ggplot(data = data, mapping = mapping, environment = parent.frame()) +
        lbs +
        coord_cartesian(
            xlim = c(tdb_lim[[1L]] * (1 - 0.005), tdb_lim[[2L]] * (1 + 0.005)),
            ylim = c(hum_lim[[1L]] * (1 - 0.005), hum_lim[[2L]] * (1 + 0.005)),
            expand = FALSE, default = TRUE
        ) +
        scale_y_continuous(position = "right") +
        theme(axis.title.y.right = element_text(angle = 90))
}
# }}}

# with_units {{{
#' @importFrom psychrolib GetUnitSystem SetUnitSystem
with_units <- function (units, expr) {
    u <- psychrolib::GetUnitSystem()
    psychrolib::SetUnitSystem(units)
    psy_op <- psychrolib:::PSYCHRO_OPT
    on.exit(psy_op$UNITS <- u, add = TRUE)

    force(expr)
}
# }}}
