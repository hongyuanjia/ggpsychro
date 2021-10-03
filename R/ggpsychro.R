#' Create a ggpsychro plot
#'
#' This function is the equivalent of [ggplot2::ggplot()] in ggplot2.
#' It takes care of setting up the plot object along with creating the layout
#' for the plot based on the graph and the specification passed in.
#' Alternatively a layout can be prepared in advance using
#' `create_layout` and passed as the data argument. See *Details* for
#' a description of all available layouts.
#'
#' @param data Default dataset to use for plot. If not already a data.frame,
#'        will be converted to one by [ggplot2::fortify()]. If not specified,
#'        must be supplied in each layer added to the plot.
#'
#' @param mapping Default list of aesthetic mappings to use for plot. If not
#'        specified, must be supplied in each layer added to the plot.
#'
#' @param tdb_lim A numeric vector of length-2 indicating the dry-bulb
#'        temperature limits. Should be in range
#'        `[-50, 100]` degree_C \[SI\] or
#'        `[-58, 212]` degree_F \[IP\].
#'        If `NULL`, no grid lines will be plotted when no data and aesthetics
#'        are added, just like normal ggplot behaviour. Default: `NULL`.
#'
#' @param hum_lim A numeric vector of length-2 indicating the humidity ratio
#'        limits. Should be in range
#'        `[0, 60]` g_H20 kg_Air-1 \[SI\] or
#'        `[0, 350]` gr_H20 lb_Air-1 \[IP\].
#'        If `NULL`, no grid lines will be plotted when no data and aesthetics
#'        are added, just like normal ggplot behaviour. Default: `NULL`.
#'
#' @param altitude A single number of altitude in m \[SI\] or ft \[IP\]. Default:
#'        `0`.
#'
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#'
#' @param mollier If `TRUE`, a Mollier chart will be created instead of a
#'        psychrometric chart. Default: `FALSE`.
#'
#' @return An object of class `gg` onto which layers, scales, etc. can be added.
#'
#' @keywords psychrometric
#'
#' @examples
#' ggpsychro()
#'
#' @importFrom checkmate assert_number assert_numeric assert_flag assert_choice
#' @importFrom psychrolib GetStandardAtmPressure
#' @importFrom ggplot2 ggplot aes waiver
#' @author Hongyuan Jia
#' @export
ggpsychro <- function (data = NULL, mapping = aes(), tdb_lim = NULL, hum_lim = NULL,
                       altitude = 0L, units = "SI", mollier = FALSE) {
    assert_flag(mollier)
    assert_number(altitude)
    assert_choice(units, c("SI", "IP"))

    # base
    p <- ggplot(data = data, mapping = mapping, environment = parent.frame())

    # store meta data
    p$psychro$mollier <- mollier
    p$psychro$units <- units
    p$psychro$altitude <- altitude
    p$psychro$tdb_lim <- tdb_lim
    p$psychro$hum_lim <- hum_lim

    # set class
    class(p) <- c("ggpsychro", class(p))

    # set coordinate system
    p$coordinates <- coord_psychro(
        tdb_lim = tdb_lim, hum_lim = hum_lim,
        altitude = altitude, units = units, mollier = mollier
    )

    # set default axis label
    p$labels <- default_labs(units = units, mollier = mollier)

    p + theme_grey_psychro()
}

#' Reports whether x is a ggplot object
#' @param x An object to test
#' @keywords internal
#' @export
is.ggpsychro <- function (x) {
    inherits(x, "ggpsychro")
}
