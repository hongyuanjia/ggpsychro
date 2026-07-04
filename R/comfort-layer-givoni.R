#' @include comfort-layer-field.R comfort-stat-givoni.R comfort-givoni.R scale-comfort.R
NULL

# Givoni layer composition belongs with the Givoni strategy geometry and stats;
# the public constructor and geometry helpers remain in comfort-givoni.R.
#' @rdname geom_comfort_pmv
#' @export
geom_comfort_givoni <- function(
    strategy = comfort_strategy_givoni(),
    mapping = NULL,
    data = NULL,
    position = "identity",
    ...,
    alpha = 0.55,
    show_labels = TRUE,
    show_pmv = FALSE,
    pmv_model = comfort_model_pmv(),
    zone_alpha = 0.2,
    zone_style = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    label <- angle <- hjust <- vjust <- NULL
    strategy <- givoni__check_strategy(strategy)
    layer_mapping <- comfort_computed_xy_mapping(mapping)
    params <- list(...)
    zone_specs <- givoni__zone_specs()
    zone_specs <- zone_specs[zone_specs$draw_zone, , drop = FALSE]
    zone_style <- givoni__check_zone_style(zone_style, zone_specs$zone)
    layers <- list()
    if (isTRUE(show_pmv)) {
        # The optional PMV background is a normal comfort overlay, kept separate
        # from Givoni zone paths so users can style both independently.
        layers[[length(layers) + 1L]] <- geom_comfort_bands(
            data = data,
            model = pmv_model,
            alpha = alpha,
            band_method = "root",
            na.rm = na.rm,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
        layers[[length(layers) + 1L]] <- scale_fill_comfort_pmv(guide = "none")
    }
    for (i in seq_len(nrow(zone_specs))) {
        spec <- zone_specs[i, , drop = FALSE]
        zone_params <- givoni__zone_params(
            spec,
            params,
            zone_style,
            zone_alpha,
            na.rm,
            strategy
        )
        zone_is_filled <- givoni__zone_fill_is_set(zone_params$fill)
        zone_geom <- if (zone_is_filled) "polygon" else "path"
        if (!zone_is_filled) {
            zone_params$fill <- NULL
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniZone,
            data = comfort_layer_data(data),
            mapping = layer_mapping,
            geom = zone_geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
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
        data = comfort_layer_data(data),
        mapping = layer_mapping,
        geom = "path",
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = utils::modifyList(
            mean_params,
            list(
                na.rm = na.rm,
                strategy = strategy,
                linetype = "dotted"
            )
        )
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
            geom = GeomPsychroTextpath,
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = utils::modifyList(
                label_params,
                list(
                    na.rm = na.rm,
                    strategy = strategy,
                    label_type = "path",
                    text_only = TRUE,
                    upright = FALSE,
                    remove_long = FALSE
                )
            )
        )
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle)
            )),
            geom = "text",
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = utils::modifyList(
                label_params,
                list(
                    na.rm = na.rm,
                    strategy = strategy,
                    label_type = "point"
                )
            )
        )
        mean_label_params <- params
        if (
            is.null(mean_label_params$colour) &&
                is.null(mean_label_params$color)
        ) {
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
            geom = "text",
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = utils::modifyList(
                mean_label_params,
                list(
                    na.rm = na.rm,
                    strategy = strategy
                )
            )
        )
    }
    layers[[length(layers) + 1L]] <- givoni__foreground_marker(
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
