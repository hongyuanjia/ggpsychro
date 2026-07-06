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
#' @rdname geom_comfort_pmv
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
    levels = NULL,
    breaks = NULL,
    n = NULL,
    band_render = c("band", "tile"),
    band_method = c("auto", "root", "isoband"),
    alpha = 0.55,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    assert_flag(bands)
    assert_flag(contours)
    assert_flag(labels)
    band_render <- match.arg(band_render)
    band_method <- match.arg(band_method)
    if (!isTRUE(bands) && !isTRUE(contours)) {
        stop(
            "At least one of `bands` or `contours` must draw a SET layer.",
            call. = FALSE
        )
    }
    params <- list(...)
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
                    levels = levels,
                    n = n,
                    band_render = band_render,
                    band_method = band_method,
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
                    breaks = breaks,
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
#' @rdname geom_comfort_pmv
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
#' @rdname geom_comfort_pmv
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
