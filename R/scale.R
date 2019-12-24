#' @include trans.R
NULL

#' Transformation object for psychrometric chart
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @param units A string indicating the system of units chosen. Should be:
#' * `waiver()` for using the parent plot units set in [ggpsychro()]
#' * Either `SI` or `"IP"`
#'
#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_drybulb_continuous {{{
scale_drybulb_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                      limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale(
        c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
        "drybulb", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), position = "bottom", ...)
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_humratio_continuous {{{
scale_humratio_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                       limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale(
        c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"),
        "humratio", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), position = "bottom", ...)
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_relhum {{{
scale_grid_relhum <- function (breaks = waiver(), minor_breaks = waiver(),
                               labels = waiver(), units = waiver(), ...) {
    psychro_continuous_scale("grid_relhum", "grid_relhum", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = c(0.0, 100.0), expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else relhum_trans(units),
        ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_wetbulb {{{
scale_grid_wetbulb <- function (breaks = waiver(), minor_breaks = waiver(),
                                labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("grid_wetbulb", "grid_wetbulb", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else wetbulb_trans(units),
        ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_vappres {{{
scale_grid_vappres <- function (breaks = waiver(), minor_breaks = waiver(),
                                labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("grid_vappres", "grid_vappres", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else vappres_trans(units),
        ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_specvol {{{
scale_grid_specvol <- function (breaks = waiver(), minor_breaks = waiver(),
                                labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("grid_spcvol", "grid_specvol", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else specvol_trans(units),
        ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_enthalpy {{{
scale_grid_enthalpy <- function (breaks = waiver(), minor_breaks = waiver(),
                                 labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("grid_enthalpy", "grid_enthalpy", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else enthalpy_trans(units),
        ...
    )
}
# }}}

# psychro_continuous_scale {{{
psychro_continuous_scale <- function (...) {
    sc <- continuous_scale(...)
    class(sc) <- c("PsyScale", class(sc))
    sc
}
# }}}
