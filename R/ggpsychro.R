# ggpsychro {{{
#' Create a ggpsychro plot
#'
#' This function is the equivalent of [ggplot2::ggplot()] in ggplot2.
#' It takes care of setting up the plot object along with creating the layout
#' for the plot based on the graph and the specification passed in.
#' Alternatively a layout can be prepared in advance using
#' `create_layout` and passed as the data argument. See *Details* for
#' a description of all available layouts.
#'
#' @param tdb_lim A numeric vector of length-2 indicating the dry-bulb
#'        temperature limits. Should be in range
#'        `[\Sexpr[results=text]{get_tdb_limits("SI")[1]},
#'          \Sexpr[results=text]{get_tdb_limits("SI")[2]},]` °C \\[SI\\] or
#'        `[\Sexpr[results=text]{get_tdb_limits("IP")[1]},
#'          \Sexpr[results=text]{get_tdb_limits("IP")[2]},]` °F \\[IP\\].
#'        If `waiver()`, default values will be
#'        `\\[0, 50\\]` °C \\[SI\\] or
#'        `\\[30, 120\\]` °F \\[IP\\]. Default: `waiver()`.
#'
#' @param hum_lim A numeric vector of length-2 indicating the humidity ratio
#'        limits. Should be in range
#'        `[\Sexpr[results=text]{get_hum_limits("SI")[1]},
#'          \Sexpr[results=text]{get_hum_limits("SI")[2]},]` °C \\[SI\\] or
#'        `[\Sexpr[results=text]{get_hum_limits("IP")[1]},
#'          \Sexpr[results=text]{get_hum_limits("IP")[2]},]` °F \\[IP\\].
#'        If `waiver()`, default values will be `[0, 30]` °C \\[SI\\]
#'        or `\\[0, 210\\]` °F \\[IP\\]. Default: `waiver()`.
#'
#' @param altitude A single number of altitude in ft \\[IP\\] or m \\[SI\\].
#'
#' @param mask_style A list containg settings to format mask area. Will be
#'        directly passed to [ggplot2::geom_polygon()]. If `waiver()`, defaults
#'        below will be used:
#'
#' * `alpha`: `NA`
#' * `color`: `NA`
#' * `fill`: `white`
#' * `linetype`: `1`
#' * `size`: `0.5`
#'
#' Learn more about setting these aesthetics in `vignette("ggplot2-specs")`.
#'
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#'
#' @return An object of class `gg` onto which layers, scales, etc. can be added.
#'
#' @keywords psychrometric
#'
#' @examples
#' ggpsychro()
#'
#' @importFrom checkmate assert_number assert_numeric
#' @importFrom psychrolib GetStandardAtmPressure
#' @importFrom ggplot2 ggplot aes coord_cartesian xlab ylab scale_y_continuous waiver
#' @export
ggpsychro <- function (data = NULL, mapping = aes(),
                       tdb_lim = c(0, 50), hum_lim = c(0, 50), altitude = 0L,
                       mask_style = waiver(), sat_style = waiver(), aspect_ratio = 9/16, units = "SI") {
    units <- match.arg(units, c("SI", "IP"))

    assert_numeric(tdb_lim, any.missing = FALSE, all.missing = FALSE, len = 2,
        unique = TRUE, sorted = TRUE,
        lower = get_tdb_limits(units)[1], upper = get_tdb_limits(units)[2]
    )
    assert_numeric(hum_lim, any.missing = FALSE, all.missing = FALSE, len = 2,
        unique = TRUE, sorted = TRUE,
        lower = get_hum_limits(units)[1], upper = get_hum_limits(units)[2]
    )
    assert_number(altitude)
    pres <- with_units(units, GetStandardAtmPressure(altitude))

    # add pressure and units as aes
    more_aes <- list(pres = pres, units = encode_units(units))
    mapping <- do.call(aes, c(mapping, more_aes))

    # base
    base <- ggplot(data = data, mapping = mapping, environment = parent.frame())

    # set plot axes limit
    coord <- coord_cartesian(xlim = tdb_lim, ylim = hum_lim, expand = FALSE, default = TRUE)

    # set default axis label
    lab <- with_units(units, {
        if (psychrolib::isIP()) {
            list(xlab(expression("Dry-bulb temperature ("*degree*F*")")),
                 ylab(expression("Humidity ratio ("*gr[m]*"/"*lb[da]*")"))
            )
        } else {
            list(xlab(expression("Dry-bulb temperature ("*degree*C*")")),
                 ylab(expression("Humidity ratio ("*g[m]*"/"*kg[da]*")"))
            )
        }
    })

    # combine
    p <- base + coord + lab + scale_y_continuous(position = "right") + theme_psychro()

    # add mask
    mask <- do.call(geom_maskarea, mask_style)

    # add saturation line
    sat <- do.call(geom_satline, sat_style)

    structure(p + mask + sat, class = c("ggpsychro", "gg", "ggplot"))
}
# }}}

# package options {{{
GGPSY_OPT <- new.env(parent = emptyenv())
# dry-bulb temp limit in Celsius [SI]
GGPSY_OPT$tdb_min <- -50.0
GGPSY_OPT$tdb_max <- 100.0
# humidity ratio limit in g_H2O kg_Air-1 [SI]
GGPSY_OPT$hum_min <- 0.0
GGPSY_OPT$hum_max <- 60.0
# all known ggplot x aes
GGPSY_OPT$x_aes <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
    "xmax_final", "xlower", "xmiddle", "xupper", "x0")
# all known ggplot y aes
GGPSY_OPT$y_aes <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
    "ymax_final", "lower", "middle", "upper", "y0")
# }}}

# get_tdb_limits {{{
get_tdb_limits <- function (units) {
    if (units == "SI") {
        c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max), "F")
    }
}
# }}}
# get_hum_limits {{{
get_hum_limits <- function (units) {
    if (units == "SI") {
        c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max), "Gr")
    }
}
# }}}
# get_units {{{
get_units <- function (data) {
    u <- unique(data$units)

    if (is.integer(u)) u <- decode_units(u)

    if (length(u) > 1L || (!u %in% c("SI", "IP"))) {
        stop("The system of units has to be either SI or IP.")
    }

    u
}
# }}}
# get_pres {{{
get_pres <- function (data) {
    # check pressure
    pres <- unique(data$pres)

    if (length(pres) > 1L) {
        warning(sprintf("Multiple atmosphere pressure value found. Only the first one will be used (%.f).", pres))
        pres <- pres[[1L]]
    }

    pres
}
# }}}
