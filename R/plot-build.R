#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggpsychro <- function(plot, ...) {
    # Fail before the custom build pipeline starts if ggplot2 has changed one of
    # the private build helpers that this method calls through the wrappers below.
    ggplot2__check_build_internals()
    plot <- build__plot_clone(plot)
    if (length(plot@layers) == 0) {
        plot <- plot + ggplot2::geom_blank()
    }

    layers <- plot@layers
    use_internal_saturation <- build__needs_saturation_layer(layers)
    plot@coordinates$draw_saturation_fg <- !use_internal_saturation
    if (use_internal_saturation) {
        layers <- build__setup_saturation_layer(layers)
    }
    data <- rep(list(NULL), length(layers))
    scales <- build__add_default_scales(plot)
    layers <- build__setup_stat_params(layers, plot$psychro, scales)
    plot@layers <- layers

    data <- ggplot2__by_layer(
        function(l, d) l$layer_data(plot@data),
        layers,
        data,
        "computing layer data"
    )
    data <- ggplot2__by_layer(
        function(l, d) l$setup_layer(d, plot),
        layers,
        data,
        "setting up layer"
    )

    layout <- create_layout(plot@facet, plot@coordinates)
    data <- layout$setup(data, plot@data, plot@plot_env)

    data <- ggplot2__by_layer(
        function(l, d) l$compute_aesthetics(d, plot),
        layers,
        data,
        "computing aesthetics"
    )
    plot@labels <- ggplot2__setup_plot_labels(plot, layers, data)

    data <- ggplot2__ignore_data(data)
    data <- lapply(data, scales$transform_df)

    # Axis placement follows the chart convention: normal psychrometric charts
    # put dry-bulb at the bottom and humidity at the right, while Mollier charts
    # swap them to top and left.
    if (!plot$psychro$mollier) {
        scale_x <- scales$get_scales("x")
        scale_x$position <- "bottom"

        scale_y <- scales$get_scales("y")
        scale_y$position <- "right"
    } else {
        scale_x <- scales$get_scales("x")
        scale_x$position <- "top"

        scale_y <- scales$get_scales("y")
        scale_y$position <- "left"
    }

    scale_x <- function() scales$get_scales("x")
    scale_y <- function() scales$get_scales("y")

    scale_rh <- function() scales$get_scales("relhum")
    scale_wb <- function() scales$get_scales("wetbulb")
    scale_vp <- function() scales$get_scales("vappres")
    scale_sv <- function() scales$get_scales("specvol")
    scale_en <- function() scales$get_scales("enthalpy")

    layout$train_position(
        data,
        scale_x(),
        scale_y(),
        scale_rh(),
        scale_wb(),
        scale_vp(),
        scale_sv(),
        scale_en()
    )
    data <- layout$map_position(data)
    data <- ggplot2__expose_data(data)

    data <- ggplot2__by_layer(
        function(l, d) l$compute_statistic(d, layout),
        layers,
        data,
        "computing stat"
    )
    data <- ggplot2__by_layer(
        function(l, d) l$map_statistic(d, plot),
        layers,
        data,
        "mapping stat to aesthetics"
    )

    plot@scales$add_missing(c("x", "y"), plot@plot_env)

    data <- ggplot2__by_layer(
        function(l, d) l$compute_geom_1(d),
        layers,
        data,
        "setting up geom"
    )

    data <- ggplot2__by_layer(
        function(l, d) l$compute_position(d, layout),
        layers,
        data,
        "computing position"
    )

    data <- ggplot2__ignore_data(data)
    layout$reset_scales()
    layout$train_position(
        data,
        scale_x(),
        scale_y(),
        scale_rh(),
        scale_wb(),
        scale_vp(),
        scale_sv(),
        scale_en()
    )
    layout$setup_panel_params()
    data <- layout$map_position(data)
    layout$setup_panel_guides(plot@guides, plot@layers)
    plot@theme <- ggplot2__plot_theme(plot)
    layout$coord$psychro_theme <- plot@theme
    layers <- build__setup_geom_params(layers, plot@theme)
    plot@layers <- layers

    npscales <- ggproto(
        NULL,
        scales,
        scales = scales$scales[
            !scales$find("x") &
                !scales$find("y") &
                !scales$find("relhum") &
                !scales$find("wetbulb") &
                !scales$find("vappres") &
                !scales$find("specvol") &
                !scales$find("enthalpy")
        ]
    )
    # npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
        npscales$set_palettes(plot@theme)
        lapply(data, npscales$train_df)
        plot@guides <- plot@guides$build(
            npscales,
            plot@layers,
            plot@labels,
            data,
            plot@theme
        )
        data <- lapply(data, npscales$map_df)
    } else {
        plot@guides <- plot@guides$get_custom()
    }

    data <- ggplot2__expose_data(data)

    data <- ggplot2__by_layer(
        function(l, d) l$compute_geom_2(d, theme = plot@theme),
        layers,
        data,
        "setting up geom aesthetics"
    )
    data <- ggplot2__by_layer(
        function(l, d) l$finish_statistics(d),
        layers,
        data,
        "finishing layer stat"
    )

    data <- layout$finish_data(data)

    plot@labels$alt <- ggplot2::get_alt_text(plot)

    build <- ggplot2__class_ggplot_built(
        data = data,
        layout = layout,
        plot = plot
    )
    class(build) <- union(
        c("ggplot2::ggplot_built", "ggplot_built"),
        class(build)
    )
    build
}

GGPSY_GGPLOT2_BUILD_INTERNALS <- new.env(parent = emptyenv())

ggplot2__build_internal_specs <- function() {
    # These are the private ggplot2 build helpers that ggpsychro calls directly.
    # The expected argument names and order are the minimum interface contract we
    # rely on; behavior-level regressions are covered by plot build tests.
    list(
        by_layer = c("f", "layers", "data", "step"),
        .ignore_data = "data",
        .expose_data = "data",
        setup_plot_labels = c("plot", "layers", "data"),
        plot_theme = c("x", "default"),
        class_ggplot_built = c("...", "data", "layout", "plot"),
        view_scales_from_scale = c("scale", "coord_limits", "expand")
    )
}

ggplot2__build_internals <- function(refresh = FALSE) {
    # Cache successful lookups so each plot build does not repeatedly inspect the
    # ggplot2 namespace; `refresh = TRUE` keeps tests able to re-check the contract.
    if (
        !refresh &&
            exists("internals", GGPSY_GGPLOT2_BUILD_INTERNALS, inherits = FALSE)
    ) {
        return(get(
            "internals",
            GGPSY_GGPLOT2_BUILD_INTERNALS,
            inherits = FALSE
        ))
    }

    ns <- asNamespace("ggplot2")
    specs <- ggplot2__build_internal_specs()
    internals <- lapply(names(specs), function(name) {
        if (!exists(name, envir = ns, inherits = FALSE)) {
            return(NULL)
        }
        get(name, envir = ns, inherits = FALSE)
    })
    names(internals) <- names(specs)

    problems <- ggplot2__build_internal_problems(internals, specs)
    if (length(problems)) {
        stop(
            "ggpsychro's custom plot builder is not compatible with the ",
            "installed ggplot2 internals.\n",
            "Problems: ",
            paste(problems, collapse = "; "),
            "\n",
            "Installed ggplot2 version: ",
            as.character(utils::packageVersion("ggplot2")),
            "\n",
            "Update ggpsychro's ggplot2 compatibility layer before building ",
            "ggpsychro plots.",
            call. = FALSE
        )
    }

    assign("internals", internals, envir = GGPSY_GGPLOT2_BUILD_INTERNALS)
    internals
}

ggplot2__build_internal_problems <- function(
    internals,
    specs = ggplot2__build_internal_specs()
) {
    # We intentionally compare function signatures, not whole function bodies.
    # Body equality is too brittle for harmless upstream refactors, while a changed
    # calling interface is the point where this custom build pipeline cannot be
    # trusted to keep calling ggplot2 internals correctly.
    problems <- character()
    for (name in names(specs)) {
        fun <- internals[[name]]
        if (!is.function(fun)) {
            problems <- c(problems, sprintf("missing `%s()`", name))
            next
        }

        expected <- specs[[name]]
        actual <- names(formals(fun))
        if (!identical(actual, expected)) {
            problems <- c(
                problems,
                sprintf(
                    "`%s()` has incompatible arguments: expected %s; found %s",
                    name,
                    paste(expected, collapse = ", "),
                    paste(actual, collapse = ", ")
                )
            )
        }
    }
    problems
}

ggplot2__check_build_internals <- function() {
    invisible(ggplot2__build_internals())
}

ggplot2__internal <- function(name) {
    ggplot2__build_internals()[[name]]
}

ggplot2__by_layer <- function(...) {
    ggplot2__internal("by_layer")(...)
}

ggplot2__ignore_data <- function(...) {
    ggplot2__internal(".ignore_data")(...)
}

ggplot2__expose_data <- function(...) {
    ggplot2__internal(".expose_data")(...)
}

ggplot2__setup_plot_labels <- function(...) {
    ggplot2__internal("setup_plot_labels")(...)
}

ggplot2__plot_theme <- function(...) {
    ggplot2__internal("plot_theme")(...)
}

ggplot2__class_ggplot_built <- function(...) {
    ggplot2__internal("class_ggplot_built")(...)
}

ggplot2__view_scales_from_scale <- function(...) {
    ggplot2__internal("view_scales_from_scale")(...)
}

build__add_default_scales <- function(plot) {
    scales <- plot@scales
    psychro <- plot$psychro

    if (!scales$has_scale("x")) {
        if (!psychro$mollier) {
            scale_x <- scale_drybulb_continuous(
                transform = drybulb_trans(psychro$units)
            )
        } else {
            scale_x <- scale_humratio_continuous(
                transform = humratio_trans(psychro$units)
            )
        }
        scale_x$aesthetics <- GGPSY_OPT$x_aes
        scales$add(scale_x)
    }

    if (!scales$has_scale("y")) {
        if (!psychro$mollier) {
            scale_y <- scale_humratio_continuous(
                transform = humratio_trans(psychro$units)
            )
        } else {
            scale_y <- scale_drybulb_continuous(
                transform = drybulb_trans(psychro$units)
            )
        }
        scale_y$aesthetics <- GGPSY_OPT$y_aes
        scales$add(scale_y)
    }

    if (!scales$has_scale("relhum")) {
        scales$add(scale_relhum_continuous(
            transform = relhum_trans(psychro$units)
        ))
    }

    if (!scales$has_scale("wetbulb")) {
        scales$add(scale_wetbulb_continuous(
            transform = wetbulb_trans(psychro$units)
        ))
    }

    if (!scales$has_scale("vappres")) {
        scales$add(scale_vappres_continuous(
            transform = vappres_trans(psychro$units)
        ))
    }

    if (!scales$has_scale("specvol")) {
        scales$add(scale_specvol_continuous(
            transform = specvol_trans(psychro$units)
        ))
    }

    if (!scales$has_scale("enthalpy")) {
        scales$add(scale_enthalpy_continuous(
            transform = enthalpy_trans(psychro$units)
        ))
    }

    scales
}

build__setup_stat_params <- function(layers, psychro, scales = NULL) {
    state_classes <- build__state_stat_classes()
    panel_classes <- build__panel_stat_classes()
    chart_classes <- c(state_classes, panel_classes)
    stat_classes <- build__stat_classes()
    pressure <- psychrolib__with_units(
        psychro$units,
        GetStandardAtmPressure(psychro$altitude)
    )
    psychro_scales <- psychro_stat_scale_context(scales, psychro)

    lapply(layers, function(layer) {
        if (!any(vapply(stat_classes, inherits, logical(1L), x = layer$stat))) {
            return(layer)
        }

        if (
            is.null(layer$stat_params$units) ||
                util__is_waive(layer$stat_params$units)
        ) {
            layer$stat_params$units <- psychro$units
        }
        if (
            is.null(layer$stat_params$pres) ||
                util__is_waive(layer$stat_params$pres)
        ) {
            layer$stat_params$pres <- pressure
        }
        if (any(vapply(chart_classes, inherits, logical(1L), x = layer$stat))) {
            if (
                is.null(layer$stat_params$mollier) ||
                    util__is_waive(layer$stat_params$mollier)
            ) {
                layer$stat_params$mollier <- psychro$mollier
            }
        }
        if (
            any(vapply(
                c("StatPsychroState", "StatPsychroZone", panel_classes),
                inherits,
                logical(1L),
                x = layer$stat
            ))
        ) {
            if (
                is.null(layer$stat_params$tdb_lim) ||
                    util__is_waive(layer$stat_params$tdb_lim)
            ) {
                layer$stat_params$tdb_lim <- psychro$tdb_lim
            }
        }
        if (any(vapply(panel_classes, inherits, logical(1L), x = layer$stat))) {
            if (
                is.null(layer$stat_params$hum_lim) ||
                    util__is_waive(layer$stat_params$hum_lim)
            ) {
                layer$stat_params$hum_lim <- psychro$hum_lim
            }
        }
        if (build__needs_scale_context(layer)) {
            # Scale transforms have already affected stat input data by compute
            # time; pass their inverses explicitly to psychrolib-backed stats.
            layer$stat_params$psychro_scales <- psychro_scales
        }

        layer
    })
}

build__setup_saturation_layer <- function(layers) {
    if (any(vapply(layers, build__is_saturation_layer, logical(1L)))) {
        return(layers)
    }

    below <- vapply(layers, build__below_saturation, logical(1L))
    c(layers[below], list(build__saturation_layer()), layers[!below])
}

build__needs_saturation_layer <- function(layers) {
    any(vapply(layers, build__above_saturation, logical(1L)))
}

build__saturation_layer <- function() {
    ggplot2::layer(
        data = data.frame(.psychro_saturation = TRUE),
        mapping = ggplot2::aes(),
        stat = "identity",
        geom = GeomPsychroSaturation,
        position = "identity",
        show.legend = FALSE,
        inherit.aes = FALSE,
        params = list(na.rm = TRUE)
    )
}

build__is_saturation_layer <- function(layer) {
    inherits(layer$geom, "GeomPsychroSaturation")
}

build__below_saturation <- function(layer) {
    if (inherits(layer$geom, "GeomPsychroTile")) {
        return(TRUE)
    }
    is_psychro_stat <- any(vapply(
        build__stat_classes(),
        inherits,
        logical(1L),
        x = layer$stat
    ))
    is_psychro_stat && !build__above_saturation(layer)
}

build__above_saturation <- function(layer) {
    is_marker_geom <- any(vapply(
        c("GeomPoint", "GeomText", "GeomLabel"),
        inherits,
        logical(1L),
        x = layer$geom
    ))
    if (!is_marker_geom) {
        return(FALSE)
    }

    any(vapply(
        build__marker_stat_classes(),
        inherits,
        logical(1L),
        x = layer$stat
    ))
}

build__setup_geom_params <- function(layers, theme) {
    lapply(layers, function(layer) {
        if (inherits(layer$geom, "GeomPsychroSaturation")) {
            layer$geom_params$psychro.theme <- theme
            layer$computed_geom_params$psychro.theme <- theme
        }
        if (inherits(layer$geom, "GeomPsychroTile")) {
            layer$geom_params$psychro.theme <- theme
            layer$computed_geom_params$psychro.theme <- theme
        }
        if (build__needs_panel_clip(layer)) {
            layer$geom <- build__clip_geom(
                layer$geom,
                filter_anchor = build__needs_anchor_filter(layer),
                clip_polygon = build__needs_polygon_clip(layer),
                clip_path = build__needs_path_clip(layer)
            )
        }

        layer
    })
}

build__state_stat_classes <- function() {
    c("StatPsychroState", "StatPsychroZone", "StatComfortState")
}

build__marker_stat_classes <- function() {
    c(
        "StatRelhum",
        "StatWetbulb",
        "StatVappres",
        "StatSpecvol",
        "StatEnthalpy",
        "StatPsychroState",
        "StatComfortState"
    )
}

# Stats in this set either consume transformed psychrometric inputs or generate
# physical coordinates that must be returned to the active chart scale.
build__scale_context_stat_classes <- function() {
    c(
        "StatRelhum",
        "StatWetbulb",
        "StatVappres",
        "StatSpecvol",
        "StatEnthalpy",
        "StatPsychroState",
        "StatPsychroZone",
        "StatComfortState",
        build__panel_stat_classes()
    )
}

# Keep scale-context injection targeted to psychrolib-backed layers so unrelated
# stats do not receive private ggproto scale objects.
build__needs_scale_context <- function(layer) {
    any(vapply(
        build__scale_context_stat_classes(),
        inherits,
        logical(1L),
        x = layer$stat
    ))
}

build__panel_stat_classes <- function() {
    c(
        "StatComfortBand",
        "StatComfortGrid",
        "StatComfortContour",
        "StatComfortPmvCurve",
        "StatComfortPmvAxisLabel",
        "StatComfortPmvRootBand",
        "StatComfortHeatIndexZone",
        "StatComfortHeatIndexContour",
        "StatComfortHeatIndexLabel",
        "StatComfortGivoniZone",
        "StatComfortGivoniLabel",
        "StatComfortGivoniMeanOutdoor",
        "StatComfortGivoniMeanOutdoorLabel",
        "StatComfortZone"
    )
}

build__stat_classes <- function() {
    c(
        "StatRelhum",
        "StatWetbulb",
        "StatVappres",
        "StatSpecvol",
        "StatEnthalpy",
        "StatPsychroBin",
        build__state_stat_classes(),
        build__panel_stat_classes()
    )
}

build__clip_exempt_stat_classes <- function() {
    c(
        "StatComfortHeatIndexLabel",
        "StatComfortGivoniMeanOutdoor",
        "StatComfortGivoniMeanOutdoorLabel"
    )
}

build__needs_panel_clip <- function(layer) {
    if (
        inherits(layer$geom, "GeomPsychroTextpath") &&
            !build__needs_path_clip(layer)
    ) {
        return(FALSE)
    }
    if (inherits(layer$geom, "GeomPsychroTile")) {
        return(FALSE)
    }
    is_psychro_stat <- any(vapply(
        build__stat_classes(),
        inherits,
        logical(1L),
        x = layer$stat
    ))
    is_exempt <- any(vapply(
        build__clip_exempt_stat_classes(),
        inherits,
        logical(1L),
        x = layer$stat
    ))
    is_psychro_stat && !is_exempt
}

build__needs_anchor_filter <- function(layer) {
    any(vapply(
        c("GeomPoint", "GeomText", "GeomLabel"),
        inherits,
        logical(1L),
        x = layer$geom
    ))
}

build__needs_polygon_clip <- function(layer) {
    inherits(layer$geom, "GeomPolygon")
}

build__needs_path_clip <- function(layer) {
    inherits(layer$geom, "GeomPsychroTextpath") &&
        !inherits(layer$stat, "StatComfortGivoniLabel")
}

build__clip_geom <- function(
    geom,
    filter_anchor = FALSE,
    clip_polygon = FALSE,
    clip_path = FALSE
) {
    if (isTRUE(geom$psychro_clipped)) {
        return(geom)
    }
    base_geom <- geom
    ggplot2::ggproto(
        NULL,
        base_geom,
        psychro_clipped = TRUE,
        parameters = function(self, extra = FALSE) {
            # ggplot2 filters draw parameters through `parameters()`. Delegate
            # to the wrapped geom so special params such as `upright` survive
            # clipping wrappers.
            base_geom$parameters(extra = extra)
        },
        draw_panel = function(data, panel_params, coord, ...) {
            if (isTRUE(filter_anchor)) {
                data <- coord_clip__filter_data_to_panel(
                    data,
                    panel_params,
                    coord
                )
                if (!nrow(data)) {
                    return(grid::nullGrob())
                }
                return(base_geom$draw_panel(data, panel_params, coord, ...))
            }
            if (isTRUE(clip_polygon)) {
                data <- coord_clip__polygon_data_to_panel(
                    data,
                    panel_params,
                    coord
                )
                if (!nrow(data)) {
                    return(grid::nullGrob())
                }
                return(base_geom$draw_panel(data, panel_params, coord, ...))
            }
            if (isTRUE(clip_path)) {
                grob <- base_geom$draw_panel(data, panel_params, coord, ...)
                return(coord_clip__textpath_to_panel(
                    grob,
                    coord,
                    panel_params
                ))
            }
            grob <- base_geom$draw_panel(data, panel_params, coord, ...)
            coord_clip__grob_to_panel(grob, coord, panel_params)
        }
    )
}

build__plot_clone <- function(plot) {
    p <- plot
    p@scales <- plot@scales$clone()
    # Build setup writes derived psychro state into ggproto members, so keep
    # those mutations local to the transient plot used by ggplot_build().
    p@layers <- lapply(plot@layers, function(layer) {
        ggplot2::ggproto(NULL, layer)
    })
    p@coordinates <- ggplot2::ggproto(NULL, plot@coordinates)
    p@facet <- ggplot2::ggproto(NULL, plot@facet)
    p@guides <- ggplot2::ggproto(NULL, plot@guides)

    p
}
