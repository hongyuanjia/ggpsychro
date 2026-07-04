#' @include comfort-stat.R comfort-stat-pmv.R comfort-model.R
NULL

# Shared comfort field layer primitives for sampled bands, contours, zones,
# metric-specific wrappers, and point-state evaluation.
# Lower-level band primitive draws a sampled comfort metric as filled regions.
#' @rdname geom_comfort_pmv
#' @export
geom_comfort_bands <- function(
    mapping = NULL,
    data = NULL,
    stat = NULL,
    position = "identity",
    ...,
    model = comfort_model_pmv(),
    metric = NULL,
    n = NULL,
    render = c("band", "tile"),
    band_method = c("auto", "root", "isoband"),
    levels = NULL,
    gap = 0,
    alpha = 0.55,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    render <- match.arg(render)
    band_method <- match.arg(band_method)
    band_metric <- comfort_model_metric(model, metric)
    # Rendering mode controls the mark type; band_method only controls how
    # continuous band boundaries are constructed for polygon rendering.
    if (render == "band" && band_method == "auto") {
        band_method <- if (
            comfort_model_type(model) == "pmv" && band_metric == "pmv"
        ) {
            "root"
        } else {
            "isoband"
        }
    }
    if (
        render == "band" &&
            band_method == "root" &&
            (comfort_model_type(model) != "pmv" || band_metric != "pmv")
    ) {
        stop(
            "`band_method = \"root\"` is only available for PMV bands.",
            call. = FALSE
        )
    }
    if (is.null(stat)) {
        stat <- if (render == "tile") {
            StatComfortGrid
        } else {
            switch(
                band_method,
                root = StatComfortPmvRootBand,
                isoband = StatComfortBand
            )
        }
    }
    geom <- if (render == "tile") GeomComfortTile else "polygon"
    params <- list(
        na.rm = na.rm,
        model = model,
        metric = metric,
        n = n,
        alpha = alpha,
        ...
    )
    if (render == "band") {
        params$levels <- levels
        if (is.null(params$colour)) {
            params$colour <- NA
        }
    } else {
        params$gap <- gap
    }

    psychro_layer(
        stat = stat,
        data = comfort_layer_data(data),
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
    render = c("band", "tile"),
    band_method = c("auto", "root", "isoband"),
    alpha = 0.55,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    assert_flag(bands)
    assert_flag(contours)
    assert_flag(labels)
    render <- match.arg(render)
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
            geom_comfort_bands,
            c(
                list(
                    mapping = mapping,
                    data = data,
                    position = position,
                    model = model,
                    metric = "set",
                    levels = levels,
                    n = n,
                    render = render,
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
            geom_comfort_contour,
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
    adaptive_standard = c("ashrae55", "en16798"),
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
        adaptive_standard <- match.arg(adaptive_standard)
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
            standard = adaptive_standard,
            category = category
        )
    }

    # Alpha is a geom styling parameter, so keep it out of the model object and
    # apply it only to the zone layer call.
    params$alpha <- params$alpha %||% alpha
    do.call(
        geom_comfort_zone,
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
#' @rdname geom_comfort_pmv
#' @export
geom_comfort_contour <- function(
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
            data = comfort_layer_data(data),
            mapping = mapping,
            geom = "path",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = params
        ))
    }

    label_params <- comfort_contour_label_params(params, label_size)
    label_params$label_path <- TRUE
    psychro_layer(
        stat = stat,
        data = comfort_layer_data(data),
        mapping = comfort_contour_label_mapping(mapping),
        geom = GeomPsychroTextpath,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = label_params
    )
}

#' @rdname geom_comfort_pmv
#' @export
geom_comfort_zone <- function(
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
        data = comfort_layer_data(data),
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
