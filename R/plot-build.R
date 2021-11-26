#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggpsychro <- function(plot) {
    if (length(plot$layers) == 0) {
        plot <- plot + ggplot2::geom_blank()
    }

    plot$scales <- plot$scales$clone()

    psychro <- plot$psychro

    layers <- plot$layers
    layer_data <- lapply(layers, function(y) y$layer_data(plot$data))

    scales <- scales_add_default(plot)

    by_layer <- function(f) {
        out <- vector("list", length(data))
        for (i in seq_along(data)) {
            out[[i]] <- f(l = layers[[i]], d = data[[i]])
        }
        out
    }

    data <- layer_data
    data <- by_layer(function(l, d) l$setup_layer(d, plot))

    layout <- create_layout(plot$facet, plot$coordinates)
    data <- layout$setup(data, plot$data, plot$plot_env)

    # will add default empty scales for x and y
    # rename corresponding colunm to x and y, respectively
    data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

    # apply Scale$trans$transform to each corresponding column
    data <- lapply(data, scales_transform_df, scales = scales)

    # modify scale position
    # TODO: let the user customize the position?
    if (!psychro$mollier) {
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

    data <- by_layer(function(l, d) l$compute_statistic(d, layout))
    data <- by_layer(function(l, d) l$map_statistic(d, plot))

    scales_add_missing(plot, c("x", "y"), plot$plot_env)

    data <- by_layer(function(l, d) l$compute_geom_1(d))

    data <- by_layer(function(l, d) l$compute_position(d, layout))

    layout$reset_scales()
    layout$train_position(data, scale_x(), scale_y(),
        scale_rh(), scale_wb(), scale_vp(), scale_sv(), scale_en()
    )
    layout$setup_panel_params()
    data <- layout$map_position(data)

    npscales <- ggproto(NULL, scales,
        scales = scales$scales[!scales$find("x") & !scales$find("y") &
            !scales$find("relhum") & !scales$find("wetbulb") &
            !scales$find("vappres") & !scales$find("specvol") &
            !scales$find("enthalpy")]
    )
    # npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
        lapply(data, scales_train_df, scales = npscales)
        data <- lapply(data, scales_map_df, scales = npscales)
    }

    data <- by_layer(function(l, d) l$compute_geom_2(d))

    data <- by_layer(function(l, d) l$finish_statistics(d))

    data <- layout$finish_data(data)

    plot$labels$alt <- ggplot2::get_alt_text(plot)

    structure(list(data = data, layout = layout, plot = plot), class = "ggplot_built")
}

scales_add_default<- function (plot) {
    scales <- plot$scales
    psychro <- plot$psychro

    if (!scales$has_scale("x")) {
        if (!psychro$mollier) {
            scale_x <- scale_drybulb_continuous(trans = drybulb_trans(psychro$units))
        } else {
            scale_x <- scale_humratio_continuous(trans = humratio_trans(psychro$units))
        }
        scale_x$aesthetics <- GGPSY_OPT$x_aes
        scales$add(scale_x)
    }

    if (!scales$has_scale("y")) {
        if (!psychro$mollier) {
            scale_y <- scale_humratio_continuous(trans = humratio_trans(psychro$units))
        } else {
            scale_y <- scale_drybulb_continuous(trans = drybulb_trans(psychro$units))
        }
        scale_y$aesthetics <- GGPSY_OPT$y_aes
        scales$add(scale_y)
    }

    if (!scales$has_scale("relhum")) {
        scales$add(scale_relhum_continuous(trans = relhum_trans(psychro$units)))
    }

    if (!scales$has_scale("wetbulb")) {
        scales$add(scale_wetbulb_continuous(trans = wetbulb_trans(psychro$units)))
    }

    if (!scales$has_scale("vappres")) {
        scales$add(scale_vappres_continuous(trans = vappres_trans(psychro$units)))
    }

    if (!scales$has_scale("specvol")) {
        scales$add(scale_specvol_continuous(trans = specvol_trans(psychro$units)))
    }

    if (!scales$has_scale("enthalpy")) {
        scales$add(scale_enthalpy_continuous(trans = enthalpy_trans(psychro$units)))
    }

    scales
}

plot_clone <- function(plot) {
    p <- plot
    p$scales <- plot$scales$clone()

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
