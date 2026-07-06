# Comfort-specific scales stay outside layer modules so scale helpers follow
# the package-wide scale file naming convention.
#' Comfort PMV fill scale
#'
#' A diverging blue-white-red fill scale centered on PMV 0.
#'
#' @param ... Passed to [ggplot2::scale_fill_gradient2()].
#' @param limits Scale limits.
#' @param low,mid,high Endpoint and midpoint colours.
#' @param midpoint Scale midpoint.
#' @param oob Out-of-bounds handler.
#' @return A ggplot2 fill scale.
#'
#' @examples
#' # Use the default PMV colour scale.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(contours = FALSE, labels = FALSE, n = c(45, 30)) +
#'     scale_fill_comfort_pmv(name = "PMV")
#'
#' # Focus the legend on the usual comfort range.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(contours = FALSE, labels = FALSE, n = c(45, 30)) +
#'     scale_fill_comfort_pmv(limits = c(-1.5, 1.5), name = "PMV")
#'
#' # Use a custom diverging palette.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(contours = FALSE, labels = FALSE, n = c(45, 30)) +
#'     scale_fill_comfort_pmv(
#'         low = "#2166AC",
#'         mid = "white",
#'         high = "#B2182B",
#'         name = "PMV"
#'     )
#'
#' @export
scale_fill_comfort_pmv <- function(
    ...,
    limits = c(-3, 3),
    low = "#3B5FFF",
    mid = "#F7F7F7",
    high = "#FF3B30",
    midpoint = 0,
    oob = scales::squish
) {
    ggplot2::scale_fill_gradient2(
        ...,
        low = low,
        mid = mid,
        high = high,
        midpoint = midpoint,
        limits = limits,
        oob = oob
    )
}
