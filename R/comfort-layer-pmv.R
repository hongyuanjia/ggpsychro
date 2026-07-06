#' @include comfort-layer-field.R comfort-stat-pmv.R comfort-model.R
NULL

# PMV layer wrapper composes sampled bands, root-traced contour curves, and
# optional PMV-based standard zones without exposing lower-level stat classes.
#' Comfort layers for psychrometric charts
#'
#' `geom_comfort_pmv()` is the main PMV entry point and can draw filled PMV
#' bands, PMV contour lines and labels, plus optional PMV-based standard zones.
#' `geom_comfort_set()`, `geom_comfort_adaptive()`, `geom_comfort_heat_index()`,
#' and `geom_comfort_givoni()` provide higher-level overlays for other comfort
#' metrics. `stat_comfort_state()` evaluates comfort fields at supplied states.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @param model A comfort model object.
#' @param n Grid resolution in dry-bulb and humidity-ratio directions. If
#'   `NULL`, a model-specific default is used.
#' @param bands,contours,labels Single logical values controlling whether
#'   high-level comfort wrappers draw filled bands, contour lines, and text
#'   labels. For `geom_comfort_pmv()`, `bands` controls the sampled PMV field;
#'   PMV standard zones still draw when `standard` is supplied.
#' @param standard PMV-based standard object for `geom_comfort_pmv()`, or an
#'   adaptive comfort standard name for `geom_comfort_adaptive()` when `model`
#'   is `NULL`.
#' @param t_running Running mean outdoor temperature for
#'   `geom_comfort_adaptive()` when `model` is `NULL`.
#' @param tr Mean radiant temperature. If `NULL`, the model uses dry-bulb
#'   temperature.
#' @param v Air speed for `geom_comfort_adaptive()` when `model` is `NULL`.
#' @param category Adaptive comfort category for `geom_comfort_adaptive()`.
#' @param contour_levels PMV contour levels for `geom_comfort_pmv()`.
#' @param band_levels Number of PMV filled bands, or a numeric vector of PMV
#'   band breaks for `geom_comfort_pmv()`.
#' @param band_render Band rendering mode. `"band"` draws filled polygon
#'   regions from continuous band boundaries; `"tile"` draws sampled grid cells
#'   directly.
#' @param band_method Boundary construction method for `band_render = "band"`.
#'   `"auto"` uses root-traced boundaries for PMV and isobands for other
#'   metrics; `"root"` forces PMV root-traced boundaries; `"isoband"` uses
#'   gridded isobands.
#' @param levels Number of filled contour bands, or a numeric vector of band
#'   breaks. Used only for `band_render = "band"`.
#' @param gap Relative gap between generated tiles for `band_render = "tile"`.
#' @param alpha Layer transparency. PMV standards keep their own defaults unless
#'   `alpha` is supplied.
#' @param breaks Contour break values.
#' @param show_labels If `TRUE`, draw overlay labels.
#' @param strategy A Givoni bioclimatic strategy object.
#' @param show_pmv If `TRUE`, draw the PMV comfort background under the Givoni
#'   strategy outlines.
#' @param pmv_model PMV model used when `show_pmv = TRUE`.
#' @param zone_alpha Alpha for the filled Givoni comfort zone. Other Givoni
#'   strategy regions are drawn as outlines.
#' @param zone_style Optional named list of per-zone style overrides for
#'   `geom_comfort_givoni()`. Names must match Givoni zone ids such as
#'   `"comfort"`, `"winter"`, or `"air_conditioning"`. Values can be created
#'   with [element_givoni_zone()], [ggplot2::element_polygon()], or ordinary
#'   named lists with fields `fill`, `colour`/`color`, `linewidth`, `linetype`,
#'   `alpha`, and `linejoin`.
#' @return A ggplot layer or a list of ggplot additions.
#'
#' @examples
#' # Draw PMV comfort bands, contours, and labels.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(name = "PMV")
#'
#' # Draw labelled PMV contour lines.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(
#'         bands = FALSE,
#'         contours = TRUE,
#'         labels = TRUE,
#'         contour_levels = c(-1, 0, 1),
#'         n = 80
#'     )
#'
#' # Draw the neutral PMV comfort zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(
#'         standard = comfort_pmv_ashrae55(),
#'         bands = FALSE,
#'         contours = FALSE,
#'         n = 80
#'     )
#'
#' # Draw sampled PMV values as grid tiles.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(band_render = "tile", contours = FALSE, n = c(45, 30))
#'
#' # Draw a PMV-based comfort standard zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(
#'         standard = comfort_pmv_ashrae55(),
#'         bands = FALSE,
#'         contours = FALSE,
#'         n = 80
#'     )
#'
#' # Draw heat-index categories on hot conditions.
#' ggpsychro(tdb_lim = c(25, 45), hum_lim = c(0, 32)) +
#'     geom_comfort_heat_index(n = c(55, 35), show_labels = FALSE)
#'
#' # Draw Givoni bioclimatic strategy zones.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(show_labels = TRUE)
#'
#' # Evaluate PMV at supplied state points.
#' states <- data.frame(
#'     tdb = c(24, 28, 31),
#'     relhum = c(45, 55, 65)
#' )
#' ggpsychro(states, tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     stat_comfort_state(
#'         aes(tdb = tdb, relhum = relhum, colour = after_stat(pmv)),
#'         size = 3
#'     )
#'
#' @export
geom_comfort_pmv <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    model = comfort_model_pmv(),
    standard = NULL,
    bands = TRUE,
    contours = TRUE,
    labels = TRUE,
    contour_levels = seq(-3, 3, by = 0.5),
    band_levels = NULL,
    n = NULL,
    band_render = c("band", "tile"),
    band_method = c("auto", "root", "isoband"),
    alpha = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    assert_flag(bands)
    assert_flag(contours)
    assert_flag(labels)
    band_render <- match.arg(band_render)
    band_method <- match.arg(band_method)
    if (!isTRUE(bands) && !isTRUE(contours) && is.null(standard)) {
        stop(
            "At least one of `bands`, `contours`, or `standard` must draw ",
            "a PMV layer.",
            call. = FALSE
        )
    }

    params <- list(...)
    curve_n <- pmv__layer_n(n)
    layers <- list()

    if (isTRUE(bands)) {
        # Let the lower-level band geom keep its default alpha unless the PMV
        # wrapper received an explicit transparency override.
        band_params <- c(
            list(
                mapping = mapping,
                data = data,
                position = position,
                model = model,
                metric = "pmv",
                n = n,
                band_render = band_render,
                band_method = band_method,
                levels = band_levels,
                na.rm = na.rm,
                show.legend = show.legend,
                inherit.aes = inherit.aes
            ),
            params
        )
        if (!is.null(alpha)) {
            band_params$alpha <- alpha
        }
        layers[[length(layers) + 1L]] <- do.call(
            comfort_layer__bands,
            band_params
        )
    }

    if (!is.null(standard)) {
        # PMV standards define their own alpha sequence; only override it when
        # the wrapper caller supplies alpha deliberately.
        standard_params <- params
        if (!is.null(alpha)) {
            standard_params$alpha <- alpha
        }
        layers <- c(
            layers,
            pmv__standard_layers(
                standard = standard,
                mapping = mapping,
                data = data,
                position = position,
                params = standard_params,
                model = model,
                n = curve_n,
                labels = labels,
                na.rm = na.rm,
                show.legend = show.legend,
                inherit.aes = inherit.aes
            )
        )
    }

    if (isTRUE(contours)) {
        layers <- c(
            layers,
            pmv__curve_layers(
                mapping = mapping,
                data = data,
                position = position,
                params = params,
                model = model,
                levels = contour_levels,
                n = curve_n,
                labels = labels,
                na.rm = na.rm,
                show.legend = show.legend,
                inherit.aes = inherit.aes
            )
        )
    }

    layers
}

# PMV contour and standard sublayers use a one-dimensional sampling count.
pmv__layer_n <- function(n, default = 360L) {
    if (is.null(n)) {
        return(default)
    }
    n[[1L]]
}
# Build PMV contour and optional label layers for the public PMV wrapper.
pmv__curve_layers <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    params = list(),
    model = comfort_model_pmv(),
    levels = seq(-3, 3, by = 0.5),
    n = 360,
    labels = TRUE,
    axis_label_hjust = ggplot2::waiver(),
    axis_label_vjust = ggplot2::waiver(),
    sensation_label_hjust = 0.5,
    sensation_label_vjust = 0.5,
    axis_label_size = NULL,
    sensation_label_size = NULL,
    padding = grid::unit(1, "pt"),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    label <- hjust <- vjust <- NULL
    text_size <- params$size
    params$size <- NULL
    label_sensation <- isTRUE(labels)
    label_axis <- isTRUE(labels)
    if (is.null(axis_label_size)) {
        axis_label_size <- if (is.null(text_size)) 2.8 else text_size
    }
    if (is.null(sensation_label_size)) {
        sensation_label_size <- if (is.null(text_size)) 3 else text_size
    }

    params$na.rm <- na.rm
    params$model <- model
    params$levels <- levels
    params$n <- n

    if (is.null(params$colour) && is.null(params$color)) {
        params$colour <- "#4A4A4A"
    }
    if (is.null(params$linewidth)) {
        params$linewidth <- 0.45
    }

    # Lines, sensation labels, and axis labels are separate stats, but they all
    # trace the same PMV roots for a given level set inside this wrapper call.
    pmv_curve_cache <- new.env(parent = emptyenv())
    sensation_levels <- pmv__sensation_levels(levels)
    line_levels <- if (isTRUE(label_sensation)) {
        setdiff(levels, sensation_levels)
    } else {
        levels
    }

    layers <- list()
    if (length(line_levels)) {
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve,
            data = comfort__layer_data(data),
            mapping = mapping,
            geom = "path",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = utils::modifyList(
                params,
                list(
                    levels = line_levels,
                    label_type = "none",
                    curve_cache = pmv_curve_cache
                )
            )
        )
    }

    if (isTRUE(label_sensation) && length(sensation_levels)) {
        sensation_params <- params
        sensation_params$size <- sensation_label_size
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve,
            data = comfort__layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            ),
            geom = GeomPsychroTextpath,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = utils::modifyList(
                sensation_params,
                list(
                    levels = sensation_levels,
                    label_type = "sensation",
                    label_hjust = sensation_label_hjust,
                    label_vjust = sensation_label_vjust,
                    curve_cache = pmv_curve_cache,
                    reverse = TRUE,
                    gap = TRUE,
                    padding = padding,
                    upright = TRUE,
                    remove_long = FALSE
                )
            )
        )
    }

    if (isTRUE(label_axis)) {
        axis_params <- params
        axis_params$size <- axis_label_size
        axis_params$linewidth <- NULL
        axis_params$linetype <- NULL
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvAxisLabel,
            data = comfort__layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label)
            ),
            geom = GeomPsychroTextpath,
            position = position,
            show.legend = FALSE,
            inherit.aes = inherit.aes,
            params = utils::modifyList(
                axis_params,
                list(
                    axis_label_hjust = axis_label_hjust,
                    curve_cache = pmv_curve_cache,
                    hjust = pmv__axis_label_text_hjust(axis_label_hjust),
                    vjust = pmv__axis_label_text_vjust(
                        axis_label_vjust,
                        axis_label_size
                    ),
                    text_only = TRUE,
                    upright = TRUE,
                    remove_long = FALSE
                )
            )
        )
    }

    layers
}

# Build PMV-standard bands, boundary contours, and optional standard labels.
pmv__standard_layers <- function(
    standard = comfort_pmv_ashrae55(),
    mapping = NULL,
    data = NULL,
    position = "identity",
    params = list(),
    model = comfort_model_pmv(),
    n = 360,
    labels = TRUE,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    label <- hjust <- vjust <- NULL
    standard <- comfort__check_standard(standard)
    params$na.rm <- na.rm
    params$model <- model
    params$n <- n
    labels <- isTRUE(labels)

    layers <- list()
    # These caches are intentionally scoped to one layer composition: they avoid
    # duplicate root tracing without retaining model/coordinate state globally.
    pmv_rootband_cache <- new.env(parent = emptyenv())
    pmv_curve_cache <- new.env(parent = emptyenv())
    # Standards are drawn as one filled PMV band per adjacent break pair; curve
    # layers are added afterward so boundaries stay visible over the fill.
    ranges <- cbind(
        standard$breaks[-length(standard$breaks)],
        standard$breaks[-1L]
    )
    alpha_override <- params$alpha
    for (i in seq_len(nrow(ranges))) {
        band_params <- params
        band_params$range <- ranges[i, ]
        band_params$rootband_cache <- pmv_rootband_cache
        band_params$fill <- standard$fills[[i]]
        band_params$alpha <- comfort__standard_alpha(
            alpha_override,
            standard$alphas,
            i
        )
        if (is.null(band_params$colour)) {
            band_params$colour <- NA
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortZone,
            data = comfort__layer_data(data),
            mapping = mapping,
            geom = "polygon",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = band_params
        )
    }

    line_params <- params
    line_params$levels <- standard$breaks
    line_params$curve_cache <- pmv_curve_cache
    if (is.null(line_params$colour) && is.null(line_params$color)) {
        line_params$colour <- "#4A4A4A"
    }
    if (is.null(line_params$linewidth)) {
        line_params$linewidth <- 0.45
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve,
        data = comfort__layer_data(data),
        mapping = mapping,
        geom = "path",
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = c(line_params, list(label_type = "none"))
    )

    if (labels) {
        boundary_params <- line_params
        if (is.null(boundary_params$size)) {
            boundary_params$size <- 2.9
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve,
            data = comfort__layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            ),
            geom = GeomPsychroTextpath,
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = c(
                boundary_params,
                list(
                    label_type = "boundary",
                    text_only = TRUE,
                    upright = TRUE,
                    remove_long = TRUE,
                    keep_path_side = TRUE
                )
            )
        )

        comfort_params <- line_params
        comfort_params$levels <- 0
        if (is.null(comfort_params$size)) {
            comfort_params$size <- 3.2
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve,
            data = comfort__layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            ),
            geom = GeomPsychroTextpath,
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = c(
                comfort_params,
                list(
                    label_type = "comfort",
                    text_only = TRUE,
                    upright = TRUE,
                    remove_long = TRUE,
                    keep_path_side = TRUE
                )
            )
        )
    }

    layers
}
