#' @include trans.R
NULL

#' Transformation object for psychrometric chart
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#'
#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_drybulb_continuous {{{
scale_drybulb_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                      limits = NULL, units = "SI", ...) {
    cs <- continuous_scale(
        c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
        "position_c", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), trans = drybulb_trans(units),
        position = "bottom", ...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_humratio_continuous {{{
scale_humratio_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                       limits = NULL, units = "SI", ...) {
    cs <- continuous_scale(
        c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"),
        "position_c", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), trans = humratio_trans(units),
        position = "bottom", ...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_relhum {{{
scale_grid_relhum <- function (breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                               units = "SI", position = "right", ...) {
    cs <- continuous_scale("rel_hum", "grid_relhum", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = c(0.0, 1.0), expand = c(0, 0), trans = relhum_trans(units),...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_wetbulb {{{
scale_grid_wetbulb <- function (breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                limits = NULL, units = "SI", position = "right", ...) {
    cs <- continuous_scale("wet_bulb", "grid_wetbulb", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), trans = wetbulb_trans(units), ...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_vappres {{{
scale_grid_vappres <- function (breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                limits = NULL, units = "SI", position = "right", ...) {
    cs <- continuous_scale("vap_pres", "grid_vappres", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), trans = vappres_trans(units), ...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_specvol {{{
scale_grid_specvol <- function (breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                limits = NULL, units = "SI", position = "right", ...) {
    cs <- continuous_scale("spc_vol", "grid_specvol", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), trans = specvol_trans(units), ...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_grid_enthalpy {{{
scale_grid_enthalpy <- function (breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
                                limits = NULL, units = "SI", position = "right", ...) {
    cs <- continuous_scale("enthalpy", "grid_enthalpy", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0), trans = enthalpy_trans(units), ...
    )

    class(cs) <- c("PsyScale", class(cs))
    cs
}
# }}}
