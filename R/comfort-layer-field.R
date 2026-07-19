#' @include comfort-stat.R comfort-stat-pmv.R comfort-model.R
NULL

# Shared comfort field layer primitives for sampled bands, contours, zones,
# metric-specific wrappers, and point-state evaluation.
# Internal band layer draws sampled comfort metrics without widening the public
# API beyond the metric-specific comfort geoms.
comfort_layer__bands <- function(
    mapping = NULL,
    data = NULL,
    stat = NULL,
    position = "identity",
    ...,
    model = comfort_model_pmv(),
    metric = NULL,
    n = NULL,
    band_render = c("band", "tile"),
    band_method = c("auto", "root", "isoband"),
    levels = NULL,
    gap = 0,
    alpha = 0.55,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    band_render <- match.arg(band_render)
    band_method <- match.arg(band_method)
    band_metric <- comfort_dispatch__model_metric(model, metric)
    # Rendering mode controls the mark type; band_method only controls how
    # continuous band boundaries are constructed for polygon rendering.
    if (band_render == "band" && band_method == "auto") {
        band_method <- if (
            comfort__model_type(model) == "pmv" && band_metric == "pmv"
        ) {
            "root"
        } else {
            "isoband"
        }
    }
    if (
        band_render == "band" &&
            band_method == "root" &&
            (comfort__model_type(model) != "pmv" || band_metric != "pmv")
    ) {
        stop(
            "`band_method = \"root\"` is only available for PMV bands.",
            call. = FALSE
        )
    }
    if (is.null(stat)) {
        stat <- if (band_render == "tile") {
            StatComfortGrid
        } else {
            switch(
                band_method,
                root = StatComfortPmvRootBand,
                isoband = StatComfortBand
            )
        }
    }
    geom <- if (band_render == "tile") GeomComfortTile else "polygon"
    params <- list(
        na.rm = na.rm,
        model = model,
        metric = metric,
        n = n,
        alpha = alpha,
        ...
    )
    if (band_render == "band") {
        params$levels <- levels
        if (is.null(params$colour)) {
            params$colour <- NA
        }
    } else {
        params$gap <- gap
    }

    psychro_layer(
        stat = stat,
        data = comfort__layer_data(data),
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = params
    )
}

# High-level SET wrapper composes filled SET bands and optional contours.
#' Draw SET comfort layers
#'
#' `geom_comfort_set()` draws Standard Effective Temperature bands and optional
#' contour lines on a psychrometric chart.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @param model A SET comfort model object.
#' @param bands,contours,labels Single logical values controlling whether to
#'   draw filled SET bands, SET contour lines, and contour text labels.
#' @param band_levels Number of filled SET bands, or a numeric vector of SET
#'   band breaks. Used only for `band_render = "band"`.
#' @param contour_levels SET contour break values.
#' @param n Grid resolution in dry-bulb and humidity-ratio directions. If
#'   `NULL`, SET bands use `c(80, 50)`.
#' @param band_render Band rendering mode. `"band"` draws filled polygon
#'   regions from continuous band boundaries; `"tile"` draws sampled grid cells
#'   directly.
#' @param alpha Layer transparency.
#' @return A list of ggplot additions.
#'
#' @details
#' `n` trades drawing smoothness for build time. `band_render = "band"` draws
#' filled SET regions from gridded isobands, while `band_render = "tile"` draws
#' sampled grid cells directly.
#'
#' @examples
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_set(n = c(45, 30))
#'
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_set(
#'         contours = TRUE,
#'         labels = TRUE,
#'         contour_levels = seq(20, 35, 5)
#'     )
#'
#' @export
geom_comfort_set <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    model = comfort_model_set(),
    bands = TRUE,
    contours = FALSE,
    labels = FALSE,
    band_levels = NULL,
    contour_levels = NULL,
    n = NULL,
    band_render = c("band", "tile"),
    alpha = 0.55,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    assert_flag(bands)
    assert_flag(contours)
    assert_flag(labels)
    band_render <- match.arg(band_render)
    if (!is.null(n)) {
        n <- comfort_grid__n(n)
    }
    if (!is.null(band_levels) && length(band_levels) == 1L) {
        band_levels <- util__check_whole_count(
            band_levels,
            "`band_levels`",
            min = 1L,
            len = 1L
        )
    }
    if (!isTRUE(bands) && !isTRUE(contours)) {
        stop(
            "At least one of `bands` or `contours` must draw a SET layer.",
            call. = FALSE
        )
    }
    params <- list(...)
    # SET has only one continuous-band construction path; reject the old internal
    # knob if callers pass it through dots so it does not remain a hidden API.
    if ("band_method" %in% names(params)) {
        stop(
            "`band_method` is not supported by `geom_comfort_set()`; ",
            "use `band_render` to choose between polygon bands and tiles.",
            call. = FALSE
        )
    }
    layers <- list()

    if (isTRUE(bands)) {
        layers[[length(layers) + 1L]] <- do.call(
            comfort_layer__bands,
            c(
                list(
                    mapping = mapping,
                    data = data,
                    position = position,
                    model = model,
                    metric = "set",
                    levels = band_levels,
                    n = n,
                    band_render = band_render,
                    alpha = alpha,
                    na.rm = na.rm,
                    show.legend = show.legend,
                    inherit.aes = inherit.aes
                ),
                params
            )
        )
    }

    if (isTRUE(contours)) {
        layers[[length(layers) + 1L]] <- do.call(
            comfort_layer__contour,
            c(
                list(
                    mapping = mapping,
                    data = data,
                    position = position,
                    model = model,
                    metric = "set",
                    breaks = contour_levels,
                    n = n,
                    label = labels,
                    na.rm = na.rm,
                    show.legend = show.legend,
                    inherit.aes = inherit.aes
                ),
                params
            )
        )
    }

    layers
}

# High-level adaptive wrapper draws the acceptable operative-temperature zone.
#' Draw adaptive comfort zones
#'
#' `geom_comfort_adaptive()` draws the adaptive comfort acceptability region
#' for ASHRAE 55 or EN 16798.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param model An adaptive comfort model object. If `NULL`, one is created
#'   from `t_running`, `tr`, `v`, `standard`, and `category`.
#' @param t_running Running mean outdoor temperature when `model` is `NULL`.
#' @param tr Mean radiant temperature. If `NULL`, the model uses dry-bulb
#'   temperature.
#' @param v Air speed when `model` is `NULL`.
#' @param standard Adaptive comfort standard used when `model` is `NULL`.
#' @param category Adaptive comfort category.
#' @param n Grid resolution in dry-bulb and humidity-ratio directions. If
#'   `NULL`, adaptive comfort zones use `c(240, 160)`.
#' @param gap Relative gap between generated tiles.
#' @param alpha Layer transparency.
#' @return A ggplot layer.
#'
#' @details
#' Adaptive comfort zones are sampled on a dry-bulb and humidity-ratio grid.
#' Increase `n` for smoother zone boundaries and decrease it for faster
#' exploratory builds.
#'
#' @examples
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_adaptive(t_running = 22, alpha = 0.3)
#'
#' @export
geom_comfort_adaptive <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    model = NULL,
    t_running = NULL,
    tr = NULL,
    v = 0.1,
    standard = c("ashrae55", "en16798"),
    category = NULL,
    n = NULL,
    gap = 0,
    alpha = 0.3,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    params <- list(...)
    if (!is.null(n)) {
        n <- comfort_grid__n(n)
    }
    if (is.null(model)) {
        standard <- match.arg(standard)
        if (is.null(t_running)) {
            stop(
                "`t_running` must be supplied when `model` is NULL.",
                call. = FALSE
            )
        }
        model <- comfort_model_adaptive(
            t_running = t_running,
            tr = tr,
            v = v,
            standard = standard,
            category = category
        )
    }

    # Alpha is a geom styling parameter, so keep it out of the model object and
    # apply it only to the zone layer call.
    params$alpha <- params$alpha %||% alpha
    do.call(
        comfort_layer__zone,
        c(
            list(
                mapping = mapping,
                data = data,
                position = position,
                model = model,
                metric = "acceptability",
                n = n,
                gap = gap,
                na.rm = na.rm,
                show.legend = show.legend,
                inherit.aes = inherit.aes
            ),
            params
        )
    )
}
# Internal contour layer keeps contour-specific stat wiring out of the public
# API while high-level wrappers expose metric-specific options.
comfort_layer__contour <- function(
    mapping = NULL,
    data = NULL,
    stat = StatComfortContour,
    position = "identity",
    ...,
    model = comfort_model_pmv(),
    metric = NULL,
    breaks = NULL,
    n = NULL,
    label = FALSE,
    label_size = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    assert_flag(label)
    if (!is.null(label_size)) {
        assert_number(label_size, lower = 0, .var.name = "label_size")
    }
    params <- list(...)

    params <- c(
        list(
            na.rm = na.rm,
            model = model,
            metric = metric,
            breaks = breaks,
            n = n
        ),
        params
    )

    if (!isTRUE(label)) {
        params$label_path <- FALSE
        return(psychro_layer(
            stat = stat,
            data = comfort__layer_data(data),
            mapping = mapping,
            geom = "path",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = params
        ))
    }

    label_params <- comfort_contour__label_params(params, label_size)
    label_params$label_path <- TRUE
    psychro_layer(
        stat = stat,
        data = comfort__layer_data(data),
        mapping = comfort_contour__label_mapping(mapping),
        geom = GeomPsychroTextpath,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = label_params
    )
}

# Internal zone layer converts a model/range pair into polygons for wrappers
# such as adaptive comfort and PMV standards.
comfort_layer__zone <- function(
    mapping = NULL,
    data = NULL,
    stat = StatComfortZone,
    position = "identity",
    ...,
    model = comfort_model_pmv(),
    metric = NULL,
    range = NULL,
    n = NULL,
    gap = 0,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    geom <- "polygon"
    params <- list(
        na.rm = na.rm,
        model = model,
        metric = metric,
        range = range,
        n = n,
        gap = gap,
        ...
    )
    if (is.null(params$colour)) {
        params$colour <- NA
    }

    psychro_layer(
        stat = stat,
        data = comfort__layer_data(data),
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = params
    )
}
#' Evaluate comfort metrics at state points
#'
#' `stat_comfort_state()` evaluates a comfort model at supplied psychrometric
#' state points and exposes the model outputs through `after_stat()`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param geom Geom used to draw evaluated state points.
#' @param model A comfort model object.
#' @return A ggplot layer.
#'
#' @examples
#' states <- data.frame(
#'     tdb = c(24, 28, 31),
#'     relhum = c(45, 55, 65)
#' )
#'
#' ggpsychro(states, tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     stat_comfort_state(
#'         aes(tdb = tdb, relhum = relhum, colour = after_stat(pmv)),
#'         size = 3
#'     )
#'
#' @export
stat_comfort_state <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    model = comfort_model_pmv(),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    psychro_layer(
        stat = StatComfortState,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, model = model, ...)
    )
}
