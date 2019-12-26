#' @importFrom ggplot2 ggplot_build
#' @export
# ggplot_build.ggpsychro {{{
ggplot_build.ggpsychro <- function (plot) {
    # retrieve meta data
    meta <- plot$psychro

    # get all layers
    layers <- plot$layers
    lapply(layers, function (l) l$aes_params)

    # move mastarea and sat line to the last
    layers <- cover_mask(layers)

    # get all layer grid types
    type <- get_geom_types(layers)

    # get current scale list
    scales <- plot$scales

    # get coord
    coord <- plot$coordinates

    # add dry-bulb and hum-ratio scales if not exist
    scales <- add_default_scales(coord, scales, meta$units)

    # do nothing if there is no grid layer
    if (all(is.na(type))) return(NextMethod())

    for (i in seq_along(type)) {
        aes <- type[[i]]

        # skip if not a valid layer
        if (is.na(aes)) next

        if (aes %in% c("maskarea", "linesat")) {
            # create init data for this layer
            layers[[i]]$data <- compute_mask_data(layers[[i]], coord, scales$get_scales("x"))
            layers[[i]]$mapping <- aes(x = x, y = y, relhum = relhum)
        } else {
            aes <- gsub("grid_", "", aes, fixed = TRUE)
            # if there is already a scale, directly use its breaks
            if (scales$has_scale(aes)) {
                sc_grid <- scales$get_scales(aes)
                new_grid <- FALSE
            # create a new scale for this variable
            } else {
                sc_grid <- get_scale_by_aes(aes)(units = plot$psychro$units)
                new_grid <- TRUE
                scales$add(sc_grid)
            }

            # need to calculte limits to get breaks
            if (!length(sc_grid$get_breaks())) {
                ranges <- coord$limits
                x_range <- scales$get_scales("x")$transform(ranges$x)
                y_range <- scales$get_scales("y")$transform(ranges$y)

                if (aes == "wetbulb") {
                    # calculate grid scale limits
                    v_range <- range(with_units(meta$units,
                        GetTWetBulbFromHumRatio(x_range, y_range, meta$pressure)))

                    # get the break step of drybulb
                    xstep <- diff(scales$get_scales("x")$get_breaks_minor())[[1L]]

                    # calculate min wetbulb with whole x steps away
                    v_range[[1L]] <- x_range[[1L]] - ceiling((x_range[[1L]] - v_range[[1L]]) / xstep) * xstep
                    v_range <- c(v_range[[1L]], x_range[[2L]])
                } else if (aes == "vappres"){
                    # calculate vapor pressure range at the y axis range
                    v_range <- with_units(meta$units, GetVapPresFromHumRatio(y_range, meta$pressure))
                    v_range <- round(v_range)
                } else if (aes == "specvol") {
                    # calculate spec vol range at the x and y axis range
                    v_range <- with_units(meta$units, GetMoistAirVolume(x_range, y_range, meta$pressure))
                } else if (aes == "enthalpy") {
                    v_range <- with_units(meta$units, GetMoistAirEnthalpy(x_range, y_range))
                    v_range <- round(v_range)
                }

                # keep the original in case it is user defined
                if (new_grid) sc_grid <- sc_grid$clone()
                # train grid scale in order to get breaks
                sc_grid$train(sc_grid$transform(v_range))
            }

            # recreate grid layer input table
            data <- compute_grid_data(layers[[i]], coord, scales$get_scales("x"), sc_grid, keep_bounds = !new_grid)

            # for wetbulb, drybulb should be greater than wetbulb
            if (aes == "wetbulb") data <- data[data$x >= data$wetbulb, ]

            # add mappings
            layers[[i]]$data <- data
            layers[[i]]$mapping <- switch(aes,
                relhum = aes(x = x, y = y, relhum = relhum, label = label),
                wetbulb = aes(x = x, y = y, wetbulb = wetbulb, label = label),
                vappres = aes(x = x, y = y, vappres = vappres, label = label),
                specvol = aes(x = x, y = y, specvol = specvol, label = label),
                enthalpy = aes(x = x, y = y, enthalpy = enthalpy, label = label)
            )
        }
    }

    # assign back
    plot$scales <- scales
    plot$layers <- layers

    NextMethod()
}
# }}}

# add_default_scales {{{
add_default_scales <- function (coord, scales, units) {
    if (!scales$has_scale("x")) {
        scales$add(scale_drybulb_continuous(units = units))
        scales$get_scales("x")$train(coord$limits$x)
    }
    if (!scales$has_scale("y")) {
        scales$add(scale_humratio_continuous(units = units))
        scales$get_scales("y")$train(coord$limits$y)
    }
    scales
}
# }}}

# get_trans_by_aes {{{
get_trans_by_aes <- function (aes) {
    get(paste0(aes, "_trans"))
}
# }}}

# get_scale_by_aes {{{
get_scale_by_aes <- function (aes) {
    get(paste0("scale_", aes))
}
# }}}

# abort_unit_waiver {{{
abort_unit_waiver <- function (object_name) {
    stop(sprintf("%s: 'units' cannot be 'waiver()' when adding to a non-ggpsychro plot.",
            gsub("()", "", object_name, fixed = TRUE)),
        call. = FALSE
    )
}
# }}}

# compute_mask_data {{{
compute_mask_data <- function (layer, coord, scale_drybulb) {
    # get drybulb limits in the transformed range
    lim_drybulb <- scale_drybulb$transform(coord$limits$x)

    # get x based on axis x limits
    x <- seq(lim_drybulb[[1L]], lim_drybulb[[2L]], length.out = layer$aes_params$n)

    new_data_frame(list(x = x, y = rep(0.0, length(x)), relhum = rep(1.0, length(x))))
}
# }}}

# compute_grid_data {{{
compute_grid_data <- function (layer, coord, scale_drybulb, scale_grid, keep_bounds) {
    # get drybulb limits in the transformed range
    lim_drybulb <- scale_drybulb$transform(coord$limits$x)

    # get major breaks
    breaks <- na.omit(scale_grid$get_breaks())
    if (is.null(breaks) || !length(breaks)) {
        empty <- new_data_frame(list(value = numeric(), x = numeric(), y = numeric(), label = character()))
        names(empty)[1L] <- scale_grid$aesthetics
        return(empty)
    }

    # get minor breaks
    breaks_minor <- scale_grid$get_breaks_minor(2)
    if (!length(breaks_minor)) breaks_minor <- breaks

    # make sure labels and breaks have the same length
    labels <- character(length(breaks_minor))
    # exclude the bounds
    idx <- breaks > min(breaks)
    labs <- scale_grid$get_labels(breaks[idx])
    if (!is.null(labs)) labels[breaks_minor %in% breaks[idx]] <- labs

    # combine in a data frame
    data <- new_data_frame(list(value = breaks_minor, label = labels))
    names(data)[1L] <- scale_grid$aesthetics

    data <- rep_dataframe(data, layer$aes_params$n)

    # get x based on axis x limits
    x <- seq(lim_drybulb[[1L]], lim_drybulb[[2L]], length.out = layer$aes_params$n)
    data$x <- rep(x, each = length(breaks_minor))
    data$y <- 0

    if (scale_grid$scale_name == "wetbulb") {
        # make sure there is a point on the saturation line
        data <- do.call(rbind, lapply(split(data, data$wetbulb), function (d) {
            if (any(d$x == min(d$wetbulb))) return(d)
            sat <- d[1L, ]
            sat$x <- min(d$wetbulb)
            rbind(sat, d)
        }))
    }

    data
}
# }}}
