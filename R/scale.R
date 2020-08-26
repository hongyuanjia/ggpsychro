#' @include trans.R
NULL

#' Transformation object for psychrometric chart
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @param units A string indicating the system of units chosen. Should be:
#' * `waiver()` for using the parent plot units set in [ggpsychro()]
#' * Either `"SI"` or `"IP"`
#'
#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
#' @examples
#' ggpsychro() +
#'     geom_grid_relhum() +
#'     scale_relhum(minor_breaks = NULL) +
#'     geom_grid_wetbulb() +
#'     scale_wetbulb(breaks = seq(25, 30, by = 5), minor_breaks = NULL) +
#'     geom_grid_vappres() +
#'     scale_vappres(breaks = seq(6000, 7000, by = 500), limits = c(6000, 7000)) +
#'     geom_grid_specvol() +
#'     scale_specvol(labels = NULL)
#'
# scale_drybulb_continuous {{{
scale_drybulb_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                      labels = waiver(), limits = NULL, units = waiver(), ...) {
    if (is.waive(units)) {
        trans <- empty_trans()
    } else {
        units <- match.arg(units, c("SI", "IP"))
        trans <- drybulb_trans(units)
    }

    psychro_continuous_scale(
        c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
        "drybulb", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        trans = trans, limits = limits, expand = c(0, 0), position = "bottom",
        guide = ifelse(GGPSY_OPT$ggplot_ver > 3.3, "axis", "none"), ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_humratio_continuous {{{
scale_humratio_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                       labels = waiver(), limits = NULL, units = waiver(), ...) {
    if (is.waive(units)) {
        trans <- empty_trans()
    } else {
        units <- match.arg(units, c("SI", "IP"))
        trans <- humratio_trans(units)
    }

    psychro_continuous_scale(
        c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"),
        "humratio", identity, name = name,
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        trans = trans, limits = limits, expand = c(0, 0), position = "right",
        guide = ifelse(GGPSY_OPT$ggplot_ver > 3.3, "axis", "none"), ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_relhum {{{
scale_relhum <- function (breaks = waiver(), minor_breaks = waiver(),
                               labels = waiver(), units = waiver(), ...) {
    psychro_continuous_scale("relhum", "relhum", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = c(0.0, 1.0), expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else relhum_trans(units),
        ...
    )
}
# }}}

#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
# scale_wetbulb {{{
scale_wetbulb <- function (breaks = waiver(), minor_breaks = waiver(),
                                labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("wetbulb", "wetbulb", identity,
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
# scale_vappres {{{
scale_vappres <- function (breaks = waiver(), minor_breaks = waiver(),
                                labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("vappres", "vappres", identity,
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
# scale_specvol {{{
scale_specvol <- function (breaks = waiver(), minor_breaks = waiver(),
                                labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("specvol", "specvol", identity,
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
# scale_enthalpy {{{
scale_enthalpy <- function (breaks = waiver(), minor_breaks = waiver(),
                                 labels = waiver(), limits = NULL, units = waiver(), ...) {
    psychro_continuous_scale("enthalpy", "enthalpy", identity,
        name = "", breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = limits, expand = c(0, 0),
        trans = if (is.waive(units)) empty_trans() else enthalpy_trans(units),
        ...
    )
}
# }}}

# psychro_continuous_scale {{{
psychro_continuous_scale <- function (..., guide = "none", super = ggplot2::ScaleContinuousPosition) {
    sc <- continuous_scale(..., guide = guide, super = super)
    class(sc) <- c("PsyScale", class(sc))
    sc
}
# }}}

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

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
PsyScale <- ggproto("PsyScale", ScaleContinuous,
  secondary.axis = waiver(),
  # Position aesthetics don't map, because the coordinate system takes
  # care of it. But they do need to be made in to doubles, so stat methods
  # can tell the difference between continuous and discrete data.
  map = function(self, x, limits = self$get_limits()) {
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is.waive(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is.waive(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  make_sec_title = function(self, title) {
    if (!is.waive(self$secondary.axis)) {
      self$secondary.axis$make_title(title)
    } else {
      ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
    }
  }
)
