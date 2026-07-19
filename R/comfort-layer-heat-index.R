#' @include comfort-layer-field.R comfort-stat-heat-index.R comfort-heat-index.R
NULL

# Heat-index layer composition keeps category fills, boundary lines, and
# coordinate foreground labels in one domain-specific module.
#' Draw heat-index comfort categories
#'
#' `geom_comfort_heat_index()` draws Outdoor Work Heat Index categories,
#' boundary lines, and optional category labels.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param model A heat-index comfort model object.
#' @param n Grid resolution in dry-bulb and humidity-ratio directions.
#'   Defaults to `c(160, 100)`.
#' @param alpha Layer transparency.
#' @param labels If `TRUE`, draw category labels.
#' @return A list of ggplot additions.
#'
#' @details
#' Heat-index categories are sampled on a dry-bulb and humidity-ratio grid.
#' Increase `n` for smoother category boundaries and decrease it for faster
#' exploratory builds.
#'
#' @examples
#' ggpsychro(tdb_lim = c(25, 45), hum_lim = c(0, 32)) +
#'     geom_comfort_heat_index(n = c(55, 35), labels = FALSE)
#'
#' @export
geom_comfort_heat_index <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    model = comfort_model_heat_index(),
    n = c(160, 100),
    alpha = 0.55,
    labels = TRUE,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    label <- angle <- NULL
    assert_flag(labels)
    n <- comfort_grid__n(n)
    layer_mapping <- comfort__computed_xy_mapping(mapping)
    params <- list(...)
    zone_specs <- heat_index__zone_specs()
    # Keep one layer per heat-index category so fill/alpha/legend semantics stay
    # unchanged, but share the expensive node grid across those sibling layers.
    zone_grid_cache <- new.env(parent = emptyenv())
    layers <- vector("list", length(zone_specs))
    for (i in seq_along(zone_specs)) {
        spec <- zone_specs[[i]]
        zone_params <- utils::modifyList(
            params,
            list(
                na.rm = na.rm,
                model = model,
                n = n,
                category_id = spec$id,
                grid_cache = zone_grid_cache,
                alpha = alpha,
                fill = spec$fill,
                colour = NA
            )
        )
        layers[[i]] <- psychro_layer(
            stat = StatComfortHeatIndexZone,
            data = comfort__layer_data(data),
            mapping = layer_mapping,
            geom = "polygon",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
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
        stat = StatComfortHeatIndexContour,
        data = comfort__layer_data(data),
        mapping = layer_mapping,
        geom = "path",
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = utils::modifyList(
            line_params,
            list(
                na.rm = na.rm,
                model = model,
                n = n,
                grid_cache = zone_grid_cache
            )
        )
    )

    if (isTRUE(labels)) {
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
            data = comfort__layer_data(data),
            mapping = comfort__computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle)
            )),
            geom = GeomComfortNullText,
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = list(
                na.rm = na.rm,
                model = model,
                n = n,
                alpha = 0
            )
        )
        layers[[length(layers) + 1L]] <- heat_index__foreground_labels(
            model,
            n,
            text_params
        )
    }

    layers
}
# Store heat-index foreground label metadata for coord rendering.
heat_index__foreground_labels <- function(model, n, params) {
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

# Null text geom triggers the heat-index label stat while coord foreground
# rendering draws the final label positions.
GeomComfortNullText <- ggplot2::ggproto(
    "GeomComfortNullText",
    ggplot2::GeomText,
    draw_panel = function(data, panel_params, coord, ...) {
        grid::nullGrob()
    }
)
