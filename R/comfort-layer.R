#' @include comfort-stat.R comfort-givoni.R
NULL

# User-facing layer wrappers and overlay composition live here; model equations
# and ggproto stat implementations are kept in the upstream comfort modules.

#' Comfort overlays for psychrometric charts
#'
#' `geom_comfort_overlay()` samples a psychrometric panel on a dry-bulb by
#' humidity-ratio grid and draws the selected comfort metric as filled contour
#' bands by default. The default model is ISO 7730 PMV.
#' `geom_comfort_contour()` draws PMV contours with root-traced curves by
#' default, and uses the same grid as the overlay for other metrics.
#' `geom_comfort_zone()` draws a comfort region, and `stat_comfort_state()`
#' evaluates comfort fields at supplied states.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @param model A comfort model object.
#' @param metric Comfort metric to draw. Defaults to `"pmv"` for PMV, `"set"`
#'   for SET, and `"acceptability"` for adaptive comfort.
#' @param n Grid resolution in dry-bulb and humidity-ratio directions. If
#'   `NULL`, a model-specific default is used.
#' @param method Overlay drawing method. `"auto"` uses root-traced filled bands
#'   for PMV and isobands for other metrics; `"rootband"` forces PMV root-traced
#'   bands; `"isoband"` draws filled contour bands; `"tile"` keeps the
#'   rectangular tile fallback.
#' @param levels Number of filled contour bands, or a numeric vector of band
#'   breaks. Ignored for `method = "tile"`.
#' @param gap Relative gap between generated tiles for `method = "tile"`.
#' @param alpha Overlay transparency. Defaults to `0.55` so psychrometric
#'   chart grid and relative-humidity curves remain visible beneath the
#'   comfort overlay. For `geom_comfort_standard_zone()`, an `alpha` supplied
#'   through `...` overrides the standard-specific defaults.
#' @param breaks Contour break values.
#' @param contour_method Contour drawing method. `"auto"` uses root-traced
#'   curves for PMV and isobands for other metrics.
#' @param label A single logical value. If `TRUE`, label contour lines with
#'   their level values.
#' @param label_size Text size for contour labels. Defaults to `2.8`.
#' @param range Comfort value interval for PMV and SET zones.
#' @param standard A PMV-based comfort standard object.
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
#'   with [element_comfort_zone()], [ggplot2::element_polygon()], or ordinary
#'   named lists with fields `fill`, `colour`/`color`, `linewidth`, `linetype`,
#'   `alpha`, and `linejoin`.
#'
#' @examples
#' # Draw filled PMV comfort bands.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(name = "PMV")
#'
#' # Draw labelled PMV contour lines.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_contour(
#'         breaks = c(-1, 0, 1),
#'         label = TRUE,
#'         n = 80
#'     )
#'
#' # Draw the neutral PMV comfort zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_zone(
#'         range = c(-0.5, 0.5),
#'         n = c(45, 30),
#'         alpha = 0.3
#'     )
#'
#' # Draw PMV curves and thermal sensation labels.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv_lines(levels = seq(-2, 2, by = 1), n = 100)
#'
#' # Draw a PMV-based comfort standard zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_standard_zone(
#'         standard = comfort_standard_ashrae55_2017(),
#'         n = 80
#'     )
#'
#' # Draw heat-index categories on hot conditions.
#' ggpsychro(tdb_lim = c(25, 45), hum_lim = c(0, 32)) +
#'     geom_comfort_heat_index(n = c(55, 35), show_labels = FALSE)
#'
#' # Draw Givoni bioclimatic strategy zones.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(show_labels = FALSE)
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
geom_comfort_overlay <- function(mapping = NULL, data = NULL,
                                 stat = NULL,
                                 position = "identity", ...,
                                 model = comfort_model_pmv(),
                                 metric = NULL, n = NULL,
                                 method = c("auto", "rootband", "isoband", "tile"),
                                 levels = NULL, gap = 0, alpha = 0.55,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
    method <- match.arg(method)
    overlay_metric <- comfort_model_metric(model, metric)
    if (method == "auto") {
        method <- if (comfort_model_type(model) == "pmv" && overlay_metric == "pmv") {
            "rootband"
        } else {
            "isoband"
        }
    }
    if (is.null(stat)) {
        stat <- switch(method,
            rootband = StatComfortPmvRootBand,
            isoband = StatComfortBand,
            tile = StatComfortGrid
        )
    }
    geom <- if (method == "tile") GeomComfortTile else "polygon"
    params <- list(
        na.rm = na.rm, model = model, metric = metric, n = n,
        alpha = alpha, ...
    )
    if (method %in% c("rootband", "isoband")) {
        params$levels <- levels
        if (is.null(params$colour)) {
            params$colour <- NA
        }
    } else {
        params$gap <- gap
    }

    psychro_layer(
        stat = stat, data = comfort_layer_data(data), mapping = mapping,
        geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = params
    )
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_heat_index <- function(mapping = NULL, data = NULL,
                                    position = "identity", ...,
                                    model = comfort_model_heat_index(),
                                    n = c(160, 100), alpha = 0.55,
                                    show_labels = TRUE,
                                    na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE) {
    label <- angle <- NULL
    layer_mapping <- comfort_computed_xy_mapping(mapping)
    params <- list(...)
    zone_specs <- comfort_heat_index_zone_specs()
    # Share the expensive grid only across this grouped heat-index zone build.
    zone_grid_cache <- new.env(parent = emptyenv())
    layers <- vector("list", length(zone_specs))
    for (i in seq_along(zone_specs)) {
        spec <- zone_specs[[i]]
        zone_params <- utils::modifyList(params, list(
            na.rm = na.rm, model = model, n = n, category_id = spec$id,
            grid_cache = zone_grid_cache, alpha = alpha, fill = spec$fill,
            colour = NA
        ))
        layers[[i]] <- psychro_layer(
            stat = StatComfortHeatIndexZone, data = comfort_layer_data(data),
            mapping = layer_mapping, geom = "polygon", position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = zone_params
        )
    }

    line_params <- params
    if (is.null(line_params$colour) && is.null(line_params$color)) {
        line_params$colour <- "#4A4A4A"
    }
    if (is.null(line_params$linewidth)) {
        line_params$linewidth <- 0.45
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortHeatIndexContour, data = comfort_layer_data(data),
        mapping = layer_mapping, geom = "path", position = position,
        show.legend = FALSE, inherit.aes = inherit.aes,
        params = utils::modifyList(line_params, list(
            na.rm = na.rm, model = model, n = n
        ))
    )

    if (isTRUE(show_labels)) {
        text_params <- params
        if (is.null(text_params$colour) && is.null(text_params$color)) {
            text_params$colour <- "#444444"
        }
        if (is.null(text_params$fontface)) {
            text_params$fontface <- "bold"
        }
        if (is.null(text_params$size)) {
            text_params$size <- 3
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortHeatIndexLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle)
            )),
            geom = GeomComfortNullText, position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = list(
                na.rm = na.rm, model = model, n = n, alpha = 0
            )
        )
        layers[[length(layers) + 1L]] <- comfort_heat_index_foreground_labels(
            model, n, text_params
        )
    }

    layers
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_contour <- function(mapping = NULL, data = NULL,
                                 stat = StatComfortContour,
                                 position = "identity", ...,
                                 model = comfort_model_pmv(),
                                 metric = NULL, breaks = NULL,
                                 n = NULL,
                                 contour_method = c("auto", "root", "isoband"),
                                 label = FALSE, label_size = NULL,
                                 na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
    contour_method <- match.arg(contour_method)
    assert_flag(label)
    if (!is.null(label_size)) {
        assert_number(label_size, lower = 0, .var.name = "label_size")
    }

    params <- list(
        na.rm = na.rm, model = model, metric = metric,
        breaks = breaks, n = n, contour_method = contour_method, ...
    )

    if (!isTRUE(label)) {
        params$label_path <- FALSE
        return(psychro_layer(
            stat = stat, data = comfort_layer_data(data), mapping = mapping, geom = "path",
            position = position, show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = params
        ))
    }

    label_params <- comfort_contour_label_params(params, label_size)
    label_params$label_path <- TRUE
    psychro_layer(
        stat = stat, data = comfort_layer_data(data),
        mapping = comfort_contour_label_mapping(mapping),
        geom = geomtextpath::GeomTextpath,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = label_params
    )
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_zone <- function(mapping = NULL, data = NULL,
                              stat = StatComfortZone,
                              position = "identity", ...,
                              model = comfort_model_pmv(), metric = NULL,
                              range = NULL, n = NULL, gap = 0,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
    geom <- "polygon"
    params <- list(na.rm = na.rm, model = model, metric = metric, range = range,
        n = n, gap = gap, ...)
    if (is.null(params$colour)) {
        params$colour <- NA
    }

    psychro_layer(
        stat = stat, data = comfort_layer_data(data), mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = params
    )
}

#' @rdname geom_comfort_overlay
#' @param label_sensation If `TRUE`, label integer PMV curves with thermal
#'   sensation text.
#' @param label_axis If `TRUE`, label each PMV curve near the x-axis.
#' @param axis_label_hjust,axis_label_vjust Position adjustment for PMV numeric
#'   labels near the x-axis. By default, [ggplot2::waiver()] computes a shared
#'   baseline above the x-axis and offsets labels above their PMV lines.
#' @param sensation_label_hjust,sensation_label_vjust Position and vertical
#'   adjustment for thermal sensation labels.
#' @param axis_label_size,sensation_label_size Text size for PMV numeric and
#'   thermal sensation labels.
#' @param padding Gap padding around labels, passed as a grid unit.
#' @export
geom_comfort_pmv_lines <- function(mapping = NULL, data = NULL,
                                   position = "identity", ...,
                                   model = comfort_model_pmv(),
                                   levels = seq(-3, 3, by = 0.5),
                                   n = 360, label_sensation = TRUE,
                                   label_axis = TRUE,
                                   axis_label_hjust = ggplot2::waiver(),
                                   axis_label_vjust = ggplot2::waiver(),
                                   sensation_label_hjust = 0.5,
                                   sensation_label_vjust = 0.5,
                                   axis_label_size = NULL,
                                   sensation_label_size = NULL,
                                   padding = grid::unit(1, "pt"),
                                   na.rm = FALSE,
                                   show.legend = NA, inherit.aes = TRUE) {
    label <- hjust <- vjust <- NULL
    params <- list(...)
    text_size <- params$size
    params$size <- NULL
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

    sensation_levels <- comfort_pmv_sensation_levels(levels)
    line_levels <- if (isTRUE(label_sensation)) {
        setdiff(levels, sensation_levels)
    } else {
        levels
    }

    layers <- list()
    if (length(line_levels)) {
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve, data = comfort_layer_data(data),
            mapping = mapping, geom = "path", position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = utils::modifyList(params, list(
                levels = line_levels, label_type = "none"
            ))
        )
    }

    if (isTRUE(label_sensation) && length(sensation_levels)) {
        sensation_params <- params
        sensation_params$size <- sensation_label_size
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve, data = comfort_layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            ),
            geom = geomtextpath::GeomTextpath, position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = utils::modifyList(sensation_params, list(
                levels = sensation_levels, label_type = "sensation",
                label_hjust = sensation_label_hjust,
                label_vjust = sensation_label_vjust,
                reverse = TRUE, gap = TRUE, padding = padding,
                upright = TRUE, remove_long = FALSE
            ))
        )
    }

    if (isTRUE(label_axis)) {
        axis_params <- params
        axis_params$size <- axis_label_size
        axis_params$linewidth <- NULL
        axis_params$linetype <- NULL
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvAxisLabel, data = comfort_layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label)
            ),
            geom = geomtextpath::GeomTextpath, position = position,
            show.legend = FALSE,
            inherit.aes = inherit.aes,
            params = utils::modifyList(axis_params, list(
                axis_label_hjust = axis_label_hjust,
                hjust = comfort_pmv_axis_label_text_hjust(axis_label_hjust),
                vjust = comfort_pmv_axis_label_text_vjust(
                    axis_label_vjust, axis_label_size
                ),
                text_only = TRUE, upright = TRUE,
                remove_long = FALSE
            ))
        )
    }

    layers
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_standard_zone <- function(standard = comfort_standard_ashrae55_2017(),
                                       mapping = NULL, data = NULL,
                                       position = "identity", ...,
                                       model = comfort_model_pmv(), n = 360,
                                       na.rm = FALSE, show.legend = NA,
                                       inherit.aes = TRUE) {
    label <- hjust <- vjust <- NULL
    standard <- comfort_check_standard(standard)
    params <- list(...)
    params$na.rm <- na.rm
    params$model <- model
    params$n <- n

    layers <- list()
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
        band_params$fill <- standard$fills[[i]]
        band_params$alpha <- comfort_standard_alpha(
            alpha_override, standard$alphas, i
        )
        if (is.null(band_params$colour)) {
            band_params$colour <- NA
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortZone, data = comfort_layer_data(data),
            mapping = mapping, geom = "polygon", position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = band_params
        )
    }

    line_params <- params
    line_params$levels <- standard$breaks
    if (is.null(line_params$colour) && is.null(line_params$color)) {
        line_params$colour <- "#4A4A4A"
    }
    if (is.null(line_params$linewidth)) {
        line_params$linewidth <- 0.45
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve, data = comfort_layer_data(data),
        mapping = mapping, geom = "path", position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = c(line_params, list(label_type = "none"))
    )

    boundary_params <- line_params
    if (is.null(boundary_params$size)) {
        boundary_params$size <- 2.9
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve, data = comfort_layer_data(data),
        mapping = ggplot2::aes(
            label = ggplot2::after_stat(label),
            hjust = ggplot2::after_stat(hjust),
            vjust = ggplot2::after_stat(vjust)
        ),
        geom = geomtextpath::GeomTextpath, position = position,
        show.legend = FALSE, inherit.aes = FALSE,
        params = c(boundary_params, list(
            label_type = "boundary", text_only = TRUE, upright = TRUE,
            remove_long = TRUE
        ))
    )

    comfort_params <- line_params
    comfort_params$levels <- 0
    if (is.null(comfort_params$size)) {
        comfort_params$size <- 3.2
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve, data = comfort_layer_data(data),
        mapping = ggplot2::aes(
            label = ggplot2::after_stat(label),
            hjust = ggplot2::after_stat(hjust),
            vjust = ggplot2::after_stat(vjust)
        ),
        geom = geomtextpath::GeomTextpath, position = position,
        show.legend = FALSE, inherit.aes = FALSE,
        params = c(comfort_params, list(
            label_type = "comfort", text_only = TRUE, upright = TRUE,
            remove_long = TRUE
        ))
    )

    layers
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_givoni <- function(strategy = comfort_strategy_givoni(),
                                mapping = NULL, data = NULL,
                                position = "identity", ...,
                                alpha = 0.55, show_labels = TRUE,
                                show_pmv = FALSE,
                                pmv_model = comfort_model_pmv(),
                                zone_alpha = 0.2, zone_style = NULL,
                                na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE) {
    label <- angle <- hjust <- vjust <- NULL
    strategy <- comfort_check_givoni_strategy(strategy)
    layer_mapping <- comfort_computed_xy_mapping(mapping)
    params <- list(...)
    zone_specs <- comfort_givoni_zone_specs()
    zone_specs <- zone_specs[zone_specs$draw_zone, , drop = FALSE]
    zone_style <- comfort_givoni_check_zone_style(zone_style, zone_specs$zone)
    layers <- list()
    if (isTRUE(show_pmv)) {
        # The optional PMV background is a normal comfort overlay, kept separate
        # from Givoni zone paths so users can style both independently.
        layers[[length(layers) + 1L]] <- geom_comfort_overlay(
            data = data,
            model = pmv_model,
            alpha = alpha,
            method = "rootband",
            na.rm = na.rm,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
        layers[[length(layers) + 1L]] <- scale_fill_comfort_pmv(guide = "none")
    }
    for (i in seq_len(nrow(zone_specs))) {
        spec <- zone_specs[i, , drop = FALSE]
        zone_params <- comfort_givoni_zone_params(
            spec, params, zone_style, zone_alpha, na.rm, strategy
        )
        zone_is_filled <- comfort_zone_fill_is_set(zone_params$fill)
        zone_geom <- if (zone_is_filled) "polygon" else "path"
        if (!zone_is_filled) {
            zone_params$fill <- NULL
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniZone, data = comfort_layer_data(data),
            mapping = layer_mapping, geom = zone_geom, position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = zone_params
        )
    }

    mean_params <- params
    if (is.null(mean_params$colour) && is.null(mean_params$color)) {
        mean_params$colour <- "#444444"
    }
    if (is.null(mean_params$linewidth)) {
        mean_params$linewidth <- 0.8
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortGivoniMeanOutdoor,
        data = comfort_layer_data(data), mapping = layer_mapping,
        geom = "path", position = position, show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = utils::modifyList(mean_params, list(
            na.rm = na.rm, strategy = strategy, linetype = "dotted"
        ))
    )

    if (isTRUE(show_labels)) {
        label_params <- params
        if (is.null(label_params$colour) && is.null(label_params$color)) {
            label_params$colour <- "#444444"
        }
        if (is.null(label_params$fontface)) {
            label_params$fontface <- "bold"
        }
        if (is.null(label_params$size)) {
            label_params$size <- 2.7
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            )),
            geom = geomtextpath::GeomTextpath, position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = utils::modifyList(label_params, list(
                na.rm = na.rm, strategy = strategy, label_type = "path",
                text_only = TRUE, upright = FALSE, remove_long = FALSE
            ))
        )
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle)
            )),
            geom = "text", position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = utils::modifyList(label_params, list(
                na.rm = na.rm, strategy = strategy, label_type = "point"
            ))
        )
        mean_label_params <- params
        if (is.null(mean_label_params$colour) &&
                is.null(mean_label_params$color)) {
            mean_label_params$colour <- "#444444"
        }
        if (is.null(mean_label_params$fontface)) {
            mean_label_params$fontface <- "bold"
        }
        if (is.null(mean_label_params$size)) {
            mean_label_params$size <- 2.7
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniMeanOutdoorLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            )),
            geom = "text", position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = utils::modifyList(mean_label_params, list(
                na.rm = na.rm, strategy = strategy
            ))
        )
    }
    layers[[length(layers) + 1L]] <- comfort_givoni_foreground_marker(
        strategy = strategy,
        show_label = isTRUE(show_labels),
        colour = mean_params$colour %||% mean_params$color %||% "#444444",
        linewidth = mean_params$linewidth %||% 0.8,
        linetype = "dotted",
        label_size = if (exists("mean_label_params", inherits = FALSE)) {
            mean_label_params$size %||% 2.7
        } else {
            2.7
        },
        fontface = if (exists("mean_label_params", inherits = FALSE)) {
            mean_label_params$fontface %||% "bold"
        } else {
            "bold"
        }
    )

    layers
}

comfort_heat_index_foreground_labels <- function(model, n, params) {
    structure(
        list(
            type = "heat_index_labels",
            model = model,
            n = n,
            colour = params$colour %||% params$color %||% "#444444",
            alpha = params$alpha %||% NA_real_,
            size = params$size %||% 3,
            family = params$family %||% "",
            fontface = params$fontface %||% "bold",
            lineheight = params$lineheight %||% 1.2,
            hjust = params$hjust %||% 0.5,
            vjust = params$vjust %||% 0.5
        ),
        class = "PsyComfortForeground"
    )
}

GeomComfortNullText <- ggplot2::ggproto(
    "GeomComfortNullText", ggplot2::GeomText,
    draw_panel = function(data, panel_params, coord, ...) {
        grid::nullGrob()
    }
)

#' @rdname geom_comfort_overlay
#' @export
stat_comfort_state <- function(mapping = NULL, data = NULL, geom = "point",
                               position = "identity", ...,
                               model = comfort_model_pmv(), na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatComfortState, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, model = model, ...)
    )
}

#' Comfort PMV fill scale
#'
#' A diverging blue-white-red fill scale centered on PMV 0.
#'
#' @param ... Passed to [ggplot2::scale_fill_gradient2()].
#' @param limits Scale limits.
#' @param low,mid,high Endpoint and midpoint colours.
#' @param midpoint Scale midpoint.
#' @param oob Out-of-bounds handler.
#'
#' @examples
#' # Use the default PMV colour scale.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(name = "PMV")
#'
#' # Focus the legend on the usual comfort range.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(limits = c(-1.5, 1.5), name = "PMV")
#'
#' # Use a custom diverging palette.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(
#'         low = "#2166AC",
#'         mid = "white",
#'         high = "#B2182B",
#'         name = "PMV"
#'     )
#'
#' @export
scale_fill_comfort_pmv <- function(..., limits = c(-3, 3),
                                   low = "#3B5FFF", mid = "#F7F7F7",
                                   high = "#FF3B30", midpoint = 0,
                                   oob = scales::squish) {
    ggplot2::scale_fill_gradient2(
        ..., low = low, mid = mid, high = high,
        midpoint = midpoint, limits = limits, oob = oob
    )
}
