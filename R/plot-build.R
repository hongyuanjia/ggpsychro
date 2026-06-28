#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggpsychro <- function(plot, ...) {
    plot <- plot_clone(plot)
    if (length(plot@layers) == 0) {
        plot <- plot + ggplot2::geom_blank()
    }

    layers <- setup_psychro_stat_params(plot@layers, plot$psychro)
    plot@layers <- layers
    data <- rep(list(NULL), length(layers))
    scales <- scales_add_default(plot)

    data <- ggplot2_by_layer(function(l, d) l$layer_data(plot@data),
        layers, data, "computing layer data")
    data <- ggplot2_by_layer(function(l, d) l$setup_layer(d, plot),
        layers, data, "setting up layer")

    layout <- create_layout(plot@facet, plot@coordinates)
    data <- layout$setup(data, plot@data, plot@plot_env)

    data <- ggplot2_by_layer(function(l, d) l$compute_aesthetics(d, plot),
        layers, data, "computing aesthetics")
    plot@labels <- ggplot2_setup_plot_labels(plot, layers, data)

    data <- ggplot2_ignore_data(data)
    data <- lapply(data, scales$transform_df)

    # modify scale position
    # TODO: let the user customize the position?
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

    layout$train_position(data, scale_x(), scale_y(),
        scale_rh(), scale_wb(), scale_vp(), scale_sv(), scale_en()
    )
    data <- layout$map_position(data)
    data <- ggplot2_expose_data(data)

    data <- ggplot2_by_layer(function(l, d) l$compute_statistic(d, layout),
        layers, data, "computing stat")
    data <- ggplot2_by_layer(function(l, d) l$map_statistic(d, plot),
        layers, data, "mapping stat to aesthetics")

    plot@scales$add_missing(c("x", "y"), plot@plot_env)

    data <- ggplot2_by_layer(function(l, d) l$compute_geom_1(d),
        layers, data, "setting up geom")

    data <- ggplot2_by_layer(function(l, d) l$compute_position(d, layout),
        layers, data, "computing position")

    data <- ggplot2_ignore_data(data)
    layout$reset_scales()
    layout$train_position(data, scale_x(), scale_y(),
        scale_rh(), scale_wb(), scale_vp(), scale_sv(), scale_en()
    )
    layout$setup_panel_params()
    data <- layout$map_position(data)
    layout$setup_panel_guides(plot@guides, plot@layers)
    plot@theme <- ggplot2_plot_theme(plot)

    npscales <- ggproto(NULL, scales,
        scales = scales$scales[!scales$find("x") & !scales$find("y") &
            !scales$find("relhum") & !scales$find("wetbulb") &
            !scales$find("vappres") & !scales$find("specvol") &
            !scales$find("enthalpy")]
    )
    # npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
        npscales$set_palettes(plot@theme)
        lapply(data, npscales$train_df)
        plot@guides <- plot@guides$build(npscales, plot@layers, plot@labels, data, plot@theme)
        data <- lapply(data, npscales$map_df)
    } else {
        plot@guides <- plot@guides$get_custom()
    }

    data <- ggplot2_expose_data(data)

    data <- ggplot2_by_layer(function(l, d) l$compute_geom_2(d, theme = plot@theme),
        layers, data, "setting up geom aesthetics")
    data <- ggplot2_by_layer(function(l, d) l$finish_statistics(d),
        layers, data, "finishing layer stat")

    data <- layout$finish_data(data)

    plot@labels$alt <- ggplot2::get_alt_text(plot)

    build <- ggplot2_class_ggplot_built(data = data, layout = layout, plot = plot)
    class(build) <- union(c("ggplot2::ggplot_built", "ggplot_built"), class(build))
    build
}

ggplot2_internal <- function(name) {
    utils::getFromNamespace(name, "ggplot2")
}

ggplot2_by_layer <- function(...) {
    ggplot2_internal("by_layer")(...)
}

ggplot2_ignore_data <- function(...) {
    ggplot2_internal(".ignore_data")(...)
}

ggplot2_expose_data <- function(...) {
    ggplot2_internal(".expose_data")(...)
}

ggplot2_setup_plot_labels <- function(...) {
    ggplot2_internal("setup_plot_labels")(...)
}

ggplot2_plot_theme <- function(...) {
    ggplot2_internal("plot_theme")(...)
}

ggplot2_class_ggplot_built <- function(...) {
    ggplot2_internal("class_ggplot_built")(...)
}

ggplot2_view_scales_from_scale <- function(...) {
    ggplot2_internal("view_scales_from_scale")(...)
}

scales_add_default<- function (plot) {
    scales <- plot@scales
    psychro <- plot$psychro

    if (!scales$has_scale("x")) {
        if (!psychro$mollier) {
            scale_x <- scale_drybulb_continuous(transform = drybulb_trans(psychro$units))
        } else {
            scale_x <- scale_humratio_continuous(transform = humratio_trans(psychro$units))
        }
        scale_x$aesthetics <- GGPSY_OPT$x_aes
        scales$add(scale_x)
    }

    if (!scales$has_scale("y")) {
        if (!psychro$mollier) {
            scale_y <- scale_humratio_continuous(transform = humratio_trans(psychro$units))
        } else {
            scale_y <- scale_drybulb_continuous(transform = drybulb_trans(psychro$units))
        }
        scale_y$aesthetics <- GGPSY_OPT$y_aes
        scales$add(scale_y)
    }

    if (!scales$has_scale("relhum")) {
        scales$add(scale_relhum_continuous(transform = relhum_trans(psychro$units)))
    }

    if (!scales$has_scale("wetbulb")) {
        scales$add(scale_wetbulb_continuous(transform = wetbulb_trans(psychro$units)))
    }

    if (!scales$has_scale("vappres")) {
        scales$add(scale_vappres_continuous(transform = vappres_trans(psychro$units)))
    }

    if (!scales$has_scale("specvol")) {
        scales$add(scale_specvol_continuous(transform = specvol_trans(psychro$units)))
    }

    if (!scales$has_scale("enthalpy")) {
        scales$add(scale_enthalpy_continuous(transform = enthalpy_trans(psychro$units)))
    }

    scales
}

setup_psychro_stat_params <- function(layers, psychro) {
    state_classes <- c("StatPsychroState", "StatPsychroZone", "StatComfortState")
    panel_classes <- c(
        "StatComfortBand", "StatComfortGrid", "StatComfortContour",
        "StatComfortPmvCurve", "StatComfortPmvLabelPath",
        "StatComfortPmvRootBand",
        "StatComfortZone"
    )
    chart_classes <- c(state_classes, panel_classes)
    stat_classes <- c(
        "StatRelhum", "StatWetbulb", "StatVappres", "StatSpecvol",
        "StatEnthalpy", "StatPsychroBin", chart_classes
    )
    pressure <- with_units(psychro$units, GetStandardAtmPressure(psychro$altitude))

    lapply(layers, function(layer) {
        if (!any(vapply(stat_classes, inherits, logical(1L), x = layer$stat))) {
            return(layer)
        }

        if (is.null(layer$stat_params$units) || is.waive(layer$stat_params$units)) {
            layer$stat_params$units <- psychro$units
        }
        if (is.null(layer$stat_params$pres) || is.waive(layer$stat_params$pres)) {
            layer$stat_params$pres <- pressure
        }
        if (any(vapply(chart_classes, inherits, logical(1L), x = layer$stat))) {
            if (is.null(layer$stat_params$mollier) || is.waive(layer$stat_params$mollier)) {
                layer$stat_params$mollier <- psychro$mollier
            }
        }
        if (any(vapply(c("StatPsychroState", "StatPsychroZone", panel_classes),
                inherits, logical(1L), x = layer$stat))) {
            if (is.null(layer$stat_params$tdb_lim) || is.waive(layer$stat_params$tdb_lim)) {
                layer$stat_params$tdb_lim <- psychro$tdb_lim
            }
        }
        if (any(vapply(panel_classes, inherits, logical(1L), x = layer$stat))) {
            if (is.null(layer$stat_params$hum_lim) || is.waive(layer$stat_params$hum_lim)) {
                layer$stat_params$hum_lim <- psychro$hum_lim
            }
        }

        layer
    })
}

plot_clone <- function(plot) {
    p <- plot
    p@scales <- plot@scales$clone()

    p
}


# Below are functions copied from ggplot2 which are needed to rewrite custom
# plot building process
# ------------------------------------------------------------------------------

scales_transform_df <- function(scales, df) {
    if (empty(df) || length(scales$scales) == 0) return(df)

    transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)),
        recursive = FALSE)
    new_data_frame(c(transformed, df[setdiff(names(df), names(transformed))]))
}

new_data_frame <- function(x = list(), n = NULL) {
    if (length(x) != 0 && is.null(names(x))) {
        stop("Elements must be named")
    }
    lengths <- vapply(x, length, integer(1))
    if (is.null(n)) {
        n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
    }
    for (i in seq_along(x)) {
        if (lengths[i] == n) next
        if (lengths[i] != 1) {
            stop("Elements must equal the number of rows or 1")
        }
        x[[i]] <- rep(x[[i]], n)
    }

    class(x) <- "data.frame"

    attr(x, "row.names") <- .set_row_names(n)
    x
}

scales_add_missing <- function(plot, aesthetics, env) {

    # Keep only aesthetics that aren't already in plot$scales
    aesthetics <- setdiff(aesthetics, plot$scales$input())

    for (aes in aesthetics) {
        scale_name <- paste("scale", aes, "continuous", sep = "_")

        scale_f <- find_global(scale_name, env, mode = "function")
        plot$scales$add(scale_f())
    }
}

find_global <- function(name, env, mode = "any") {
    if (exists(name, envir = env, mode = mode)) {
        return(get(name, envir = env, mode = mode))
    }

    nsenv <- asNamespace("ggplot2")
    if (exists(name, envir = nsenv, mode = mode)) {
        return(get(name, envir = nsenv, mode = mode))
    }

    NULL
}

empty <- function(df) {
    is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

scales_train_df <- function(scales, df, drop = FALSE) {
    if (empty(df) || length(scales$scales) == 0) return()

    lapply(scales$scales, function(scale) scale$train_df(df = df))
}

scales_map_df <- function(scales, df) {
    if (empty(df) || length(scales$scales) == 0) return(df)

    mapped <- unlist(lapply(scales$scales, function(scale) scale$map_df(df = df)), recursive = FALSE)

    new_data_frame(c(mapped, df[setdiff(names(df), names(mapped))]))
}
