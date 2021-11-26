#' @include trans.R
NULL

#' Transformation object for psychrometric chart
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @param trans TODO: determined by `units`
#'
#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
#' @examples
#' ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 50)) +
#'     scale_relhum_continuous(n.breaks = 8) +
#'     scale_wetbulb_continuous(breaks = seq(25, 30, by = 5), minor_breaks = NULL) +
#'     scale_vappres_continuous(breaks = seq(6000, 7000, by = 500), limits = c(6000, 7000))
#'
scale_drybulb_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                     n.breaks = NULL, labels = waiver(), limits = NULL,
                                     expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    # TODO: use mollier determine which aes should be used?
    psychro_continuous_scale(
        GGPSY_OPT$tdb_aes, "drybulb", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, position = NULL, super = ggplot2::ScaleContinuousPosition,
        ...
    )
}

#' @rdname scale
#' @export
scale_humratio_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                      n.breaks = NULL, labels = waiver(), limits = NULL,
                                      expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    psychro_continuous_scale(
        GGPSY_OPT$hum_aes, "humratio", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, position = NULL, super = ggplot2::ScaleContinuousPosition,
        ...
    )
}

#' @rdname scale
#' @export
scale_relhum_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                    n.breaks = NULL, labels = waiver(), limits = NULL,
                                    expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    # TODO: use the same logic as scale_x_continuous, e.g. ggplot_global$relhum
    psychro_continuous_scale(
        "relhum", "relhum", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, ...
    )
}

#' @rdname scale
#' @export
scale_wetbulb_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                     n.breaks = NULL, labels = waiver(), limits = NULL,
                                     expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    psychro_continuous_scale(
        "wetbulb", "wetbulb", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, ...
    )
}

#' @rdname scale
#' @export
scale_vappres_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                     n.breaks = NULL, labels = waiver(), limits = NULL,
                                     expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    psychro_continuous_scale(
        "vappres", "vappres", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, ...
    )
}

#' @rdname scale
#' @export
scale_specvol_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                     n.breaks = NULL, labels = waiver(), limits = NULL,
                                     expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    psychro_continuous_scale(
        "specvol", "specvol", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, ...
    )
}

#' @rdname scale
#' @export
scale_enthalpy_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                      n.breaks = NULL, labels = waiver(), limits = NULL,
                                      expand = waiver(), trans = waiver(), guide = waiver(), ...) {
    psychro_continuous_scale(
        "enthalpy", "enthalpy", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
        labels = labels, limits = limits, expand = expand, trans = init_trans(trans),
        guide = guide, ...
    )
}

psychro_continuous_scale <- function(..., super = ggplot2::ScaleContinuous) {
    scale <- continuous_scale(..., super = super)
    class(scale) <- c("PsyScale", class(scale))
    scale
}

init_trans <- function(trans = waiver()) {
    if (is.waive(trans)) empty_trans() else trans
}

# use stat to compute corresponding hum-ratio values
# scale_relhum_continuous --> for grid
# scale_relhum_color --> for points
# scale_relhum_fill --> for fill
# scale_relhum_size --> for line size
# scale_relhum_linetype --> for line type

# Look up the scale that should be used for a given aesthetic
# adopted from https://github.com/tidyverse/ggplot2/blob/master/R/aes.r
aes_to_scale <- function(var) {
    var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
    var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"

    var
}
