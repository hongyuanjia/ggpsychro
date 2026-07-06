#' @include trans.R
NULL

#' Transformation object for psychrometric chart
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @param trans,transform A transformation object or transformer name passed
#'   to the underlying ggplot2 continuous scale. The default [ggplot2::waiver()]
#'   keeps the psychrometric scale in chart display units.
#' @return A ggplot2 continuous scale object.
#'
#' @rdname scale
#' @importFrom ggplot2 continuous_scale
#' @export
#' @examples
#' # Configure dry-bulb and humidity-ratio axes.
#' ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 25)) +
#'     scale_drybulb_continuous(breaks = seq(0, 35, by = 5)) +
#'     scale_humratio_continuous(breaks = seq(0, 25, by = 5))
#'
#' # Configure relative-humidity and wet-bulb grid breaks.
#' ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 25)) +
#'     scale_relhum_continuous(n.breaks = 8) +
#'     scale_wetbulb_continuous(
#'         breaks = seq(10, 30, by = 5),
#'         minor_breaks = NULL
#'     )
#'
#' # Configure vapor-pressure, specific-volume, and enthalpy grids.
#' ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 25)) +
#'     scale_vappres_continuous(
#'         breaks = seq(1000, 4000, by = 1000),
#'         limits = c(1000, 4500)
#'     ) +
#'     scale_specvol_continuous(
#'         breaks = seq(0.80, 0.95, by = 0.05),
#'         limits = c(0.80, 0.95)
#'     ) +
#'     scale_enthalpy_continuous(
#'         breaks = seq(20000, 80000, by = 20000),
#'         limits = c(20000, 80000)
#'     )
#'
scale_drybulb_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    # TODO: use mollier determine which aes should be used?
    scale__continuous_psychro(
        GGPSY_OPT$tdb_aes,
        "drybulb",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        position = "bottom",
        super = ggplot2::ScaleContinuousPosition,
        ...
    )
}

#' @rdname scale
#' @export
scale_humratio_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    scale__continuous_psychro(
        GGPSY_OPT$hum_aes,
        "humratio",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        position = "left",
        super = ggplot2::ScaleContinuousPosition,
        ...
    )
}

#' @rdname scale
#' @export
scale_relhum_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    # TODO: use the same logic as scale_x_continuous, e.g. ggplot_global$relhum
    scale__continuous_psychro(
        "relhum",
        "relhum",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        ...
    )
}

#' @rdname scale
#' @export
scale_wetbulb_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    scale__continuous_psychro(
        "wetbulb",
        "wetbulb",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        ...
    )
}

#' @rdname scale
#' @export
scale_vappres_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    scale__continuous_psychro(
        "vappres",
        "vappres",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        ...
    )
}

#' @rdname scale
#' @export
scale_specvol_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    scale__continuous_psychro(
        "specvol",
        "specvol",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        ...
    )
}

#' @rdname scale
#' @export
scale_enthalpy_continuous <- function(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = waiver(),
    n.breaks = NULL,
    labels = waiver(),
    limits = NULL,
    expand = waiver(),
    trans = waiver(),
    transform = waiver(),
    guide = waiver(),
    ...
) {
    scale__continuous_psychro(
        "enthalpy",
        "enthalpy",
        identity,
        name = name,
        breaks = breaks,
        minor_breaks = minor_breaks,
        n.breaks = n.breaks,
        labels = labels,
        limits = limits,
        expand = expand,
        transform = scale__init_transform(trans, transform),
        guide = guide,
        ...
    )
}

# Build a psychrometric continuous scale and mark it for plot construction.
scale__continuous_psychro <- function(
    aesthetics,
    scale_name,
    palette,
    ...,
    super = ggplot2::ScaleContinuous
) {
    scale <- continuous_scale(
        aesthetics = aesthetics,
        palette = palette,
        ...,
        super = super
    )
    scale$scale_name <- scale_name
    class(scale) <- c("PsyScale", class(scale))
    scale
}

# Resolve ggplot2's waiver sentinel to the identity transformation.
scale__init_trans <- function(trans = waiver()) {
    if (util__is_waive(trans)) empty_trans() else trans
}

# Prefer the ggplot2 `transform` argument while preserving legacy `trans`.
scale__init_transform <- function(trans = waiver(), transform = waiver()) {
    if (!util__is_waive(transform)) {
        transform
    } else {
        scale__init_trans(trans)
    }
}
