#' Draw drybulb and hum ratio grid lines
#'
#' @param theme A ggplot [theme][ggplot2::theme]
#' @param tdb.minor,tdb.major A numeric vector of dry-bulb temperature
#'        minor/major breaks in **native** units.
#' @param hum.minor,hum.major A numeric vector of humidity ratio minor/major
#'        breaks in **native** units.
#' @param saturation,rh.minor,rh.major,twb.minor,twb.major,vappres.minor,
#'        vappres.major,specvol.minor,specvol.major,enthalpy.minor, enthalpy.major
#'        A list of 4 elements, i.e. `tdb`, `hum`, `len`, and `n`.
#' @param mollier A single logical value indicating whether a Mollier plot is
#'        desired
#' @noRd
guide_grid_psychro <- function(theme, tdb.minor, tdb.major, hum.minor, hum.major,
                               saturation,
                               rh.minor, rh.major, twb.minor, twb.major,
                               vappres.minor, vappres.major, specvol.minor, specvol.major,
                               enthalpy.minor, enthalpy.major, grid.labels,
                               mollier) {
    # create psychrometric chart panel
    panel <- psychro_panel_polygon(saturation, mollier)
    panel_x <- panel$x
    panel_y <- panel$y
    psychro_mask <- psychro_panel_mask_grob(theme, panel_x, panel_y)
    psychro_panel <- psychro_panel_background_grob(theme, panel_x, panel_y)
    psychro_panel_clip <- grid::polygonGrob(
        panel_x, panel_y, gp = grid::gpar(col = NA, fill = NA),
        name = "psychro-panel-clip"
    )

    if (mollier) {
        nm_tdb <- "y"
        nm_hum <- "x"
    } else {
        nm_tdb <- "x"
        nm_hum <- "y"
    }

    grid_elem <- function(x, type, var) {
        vx <- rep(x, each = 2L)
        vy <- rep(0:1, length(x))
        v <- if (var == "x") list(x = vx, y = vy) else list(x = vy, y = vx)

        ggplot2::element_render(
            theme, paste("panel.grid", type, var, sep = "."),
            x = v$x, y = v$y, id.lengths = rep(2, length(x))
        )
    }

    psy_grid_elem <- function(x, type, var) {
        ggplot2::element_render(
            theme, paste("psychro.panel.grid", type, var, sep = "."),
            x = x[[c("tdb", "hum")[c(!mollier, mollier)]]],
            y = x[[c("tdb", "hum")[c(mollier, !mollier)]]],
            id.lengths = rep(x$len, x$n)
        )
    }

    psy_grid_label <- function(x, var) {
        psychro_grid_label_grob(
            x, grid.labels[[var]], var, theme, mollier,
            panel_x, panel_y
        )
    }

    grill <- grid::grobTree(
        ggplot2::element_render(theme, "panel.background"),

        psychro_mask,

        psychro_panel,

        if (length(hum.minor)) {
            clip_grob(psychro_panel_clip, grid_elem(hum.minor, "minor", nm_hum))
        },

        if (length(tdb.minor)) {
            clip_grob(psychro_panel_clip, grid_elem(tdb.minor, "minor", nm_tdb))
        },

        if (length(hum.major)) {
            clip_grob(psychro_panel_clip, grid_elem(hum.major, "major", nm_hum))
        },

        if (length(tdb.major)) {
            clip_grob(psychro_panel_clip, grid_elem(tdb.major, "major", nm_tdb))
        },

        if (length(rh.minor)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(rh.minor, "minor", "relhum"))
        },

        if (length(rh.major)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(rh.major, "major", "relhum"))
        },

        if (length(twb.minor)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(twb.minor, "minor", "wetbulb"))
        },

        if (length(twb.major)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(twb.major, "major", "wetbulb"))
        },

        if (length(vappres.minor)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(vappres.minor, "minor", "vappres"))
        },

        if (length(vappres.major)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(vappres.major, "major", "vappres"))
        },

        if (length(specvol.minor)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(specvol.minor, "minor", "specvol"))
        },

        if (length(specvol.major)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(specvol.major, "major", "specvol"))
        },

        if (length(enthalpy.minor)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(enthalpy.minor, "minor", "enthalpy"))
        },

        if (length(enthalpy.major)) {
            clip_grob(psychro_panel_clip, psy_grid_elem(enthalpy.major, "major", "enthalpy"))
        },

        if (length(rh.major)) {
            psy_grid_label(rh.major, "relhum")
        },

        if (length(twb.major)) {
            psy_grid_label(twb.major, "wetbulb")
        },

        if (length(vappres.major)) {
            psy_grid_label(vappres.major, "vappres")
        },

        if (length(specvol.major)) {
            psy_grid_label(specvol.major, "specvol")
        },

        if (length(enthalpy.major)) {
            psy_grid_label(enthalpy.major, "enthalpy")
        }
    )

    grill$name <- grid::grobName(grill, "grill")
    grill
}

psychro_panel_polygon <- function(saturation, mollier = FALSE) {
    if (mollier) {
        return(list(
            x = c(0.0, 0.0, 1.0, rev(saturation$hum), saturation$hum[1L]),
            y = c(0.0, 1.0, 1.0, rev(saturation$tdb), 0.0)
        ))
    }

    list(
        x = c(0.0, 0.0, saturation$tdb, 1.0, 1.0),
        y = c(0.0, saturation$hum[1L], saturation$hum, 1.0, 0.0)
    )
}

psychro_panel_mask_grob <- function(theme, x, y) {
    element <- ggplot2::calc_element("psychro.panel.mask", theme)
    if (is.null(element) || inherits(element, "element_blank")) {
        return(grid::nullGrob())
    }

    # Draw the mask area as a background ring, not as a foreground cover. The
    # even-odd path fills the full panel except the valid psychrometric polygon.
    x <- c(x, x[[1L]])
    y <- c(y, y[[1L]])
    grid::pathGrob(
        x = c(0, 1, 1, 0, 0, x),
        y = c(0, 0, 1, 1, 0, y),
        id.lengths = c(5L, length(x)),
        rule = "evenodd",
        gp = grid::gpar(
            fill = element$fill,
            col = element$colour %||% element$color,
            lwd = (element$linewidth %||% element$size %||% 0) * ggplot2::.pt,
            lty = element$linetype %||% 1
        ),
        name = "psychro-panel-mask"
    )
}

psychro_panel_background_grob <- function(theme, x, y) {
    element <- ggplot2::calc_element("psychro.panel.background", theme)
    if (is.null(element) || inherits(element, "element_blank")) {
        return(grid::nullGrob())
    }
    if (inherits(element, "element_polygon")) {
        return(ggplot2::element_render(
            theme, "psychro.panel.background", x = x, y = y,
            name = "psychro-panel-background"
        ))
    }

    grid::polygonGrob(
        x, y,
        gp = grid::gpar(
            fill = element$fill,
            col = element$colour %||% element$color,
            lwd = (element$linewidth %||% element$size %||% 0) * ggplot2::.pt,
            lty = element$linetype %||% 1
        ),
        name = "psychro-panel-background"
    )
}

#' @importFrom gridGeometry polyclipGrob
clip_grob <- function(panel, grob, op = "intersection") {
    if (!identical(op, "intersection")) {
        return(gridGeometry::polyclipGrob(grob, panel, op, name = grob$name))
    }
    split <- psychro_split_styled_grob(grob)
    clipped <- lapply(split, psychro_polyclip_grob, panel = panel)
    if (length(clipped) == 1L) {
        return(clipped[[1L]])
    }
    do.call(grid::grobTree, clipped)
}

psychro_protractor_guide <- function(protractor) {
    guide <- protractor$guide %||% guide_psychro_protractor()
    validate_psychro_protractor_guide(guide)
    guide
}

psychro_protractor_breaks <- function(breaks, minor = FALSE) {
    if (is.null(breaks)) {
        return(numeric())
    }
    if (is.waive(breaks)) {
        if (isTRUE(minor)) {
            return(psychro_protractor_shr_minor_breaks())
        }
        return(psychro_protractor_shr_major_breaks())
    }
    breaks
}

psychro_protractor_shr_major_breaks <- function() {
    c(
        -5, -2, -1, -0.5, -0.3, -0.2,
        0, 0.2, 0.4, 0.6, 0.7, 0.8,
        1, 1.5, 2, 4
    )
}

psychro_protractor_shr_minor_breaks <- function() {
    c(
        -10, -4, -3, -1.5, -0.1,
        0.1, 0.3, 0.5, 0.9,
        1.2, 1.8, 3, 5, 8, 10
    )
}

psychro_protractor_grob <- function(protractor, theme, mollier,
                                    range_tdb, range_hum, units) {
    if (!is.list(protractor) || !isTRUE(protractor$show)) {
        return(NULL)
    }

    scale <- protractor$scale %||% default_psychro_protractor()$scale
    radius <- protractor$radius %||% default_psychro_protractor()$radius
    margin <- psychro_protractor_margin(
        protractor$margin %||% default_psychro_protractor()$margin
    )
    radius <- min(radius * scale, 0.5 - max(margin$x, margin$y))

    center <- psychro_protractor_center(radius, margin, mollier)
    rotation <- if (mollier) -pi / 2 else 0

    line_style <- psychro_protractor_line_style_defaults(
        theme, protractor$style %||% list()
    )
    text_style <- psychro_protractor_text_style_defaults(
        theme, protractor$label_style %||% list()
    )

    gp_line <- grid::gpar(
        col = psychro_grid_alpha(line_style$colour, line_style$alpha),
        lwd = line_style$linewidth * scale * ggplot2::.pt,
        lty = line_style$linetype,
        lineend = line_style$lineend
    )
    gp_text <- grid::gpar(
        col = psychro_grid_alpha(text_style$colour, text_style$alpha),
        fontsize = text_style$size * scale * ggplot2::.pt,
        fontfamily = text_style$family,
        fontface = text_style$fontface,
        lineheight = text_style$lineheight
    )

    arc_angle <- seq(pi, 2 * pi, length.out = 96L)
    arc <- psychro_protractor_points(center, radius, arc_angle, rotation)
    diameter <- psychro_protractor_diameter(center, radius, rotation)
    endpoint_caps <- psychro_protractor_endpoint_caps(center, radius, rotation)
    center_mark <- psychro_protractor_center_mark(center, radius, rotation)

    guide <- psychro_protractor_guide(protractor)
    shr_breaks <- psychro_protractor_breaks(guide$shr_breaks)
    shr_minor_breaks <- psychro_protractor_breaks(guide$shr_minor_breaks, minor = TRUE)
    ratio_major_breaks <- psychro_protractor_ratio_breaks(guide$ratio_breaks, units)
    ratio_minor_breaks <- psychro_protractor_ratio_breaks(guide$ratio_minor_breaks, units, minor = TRUE)

    minor_data <- psychro_protractor_shr_ticks(shr_minor_breaks, range_tdb, range_hum, units)
    major_data <- psychro_protractor_shr_ticks(shr_breaks, range_tdb, range_hum, units)
    major_data <- psychro_protractor_add_sensible_endpoint(major_data, shr_breaks)
    ratio_major_data <- psychro_protractor_ratio_ticks(
        ratio_major_breaks, range_tdb, range_hum, units
    )
    ratio_minor_data <- psychro_protractor_ratio_ticks(
        ratio_minor_breaks, range_tdb, range_hum, units
    )
    major_tick_data <- psychro_protractor_unique_ticks(rbind(
        psychro_protractor_tick_data(major_data, "shr"),
        psychro_protractor_tick_data(ratio_major_data, "ratio")
    ), major = TRUE)
    minor_tick_data <- psychro_protractor_unique_ticks(rbind(
        psychro_protractor_tick_data(minor_data, "shr"),
        psychro_protractor_tick_data(ratio_minor_data, "ratio")
    ), major = FALSE)
    minor_tick_data <- psychro_protractor_drop_tick_angles(minor_tick_data, major_tick_data)
    minor_ticks <- if (nrow(minor_tick_data)) {
        psychro_protractor_ticks(
            center, radius, minor_tick_data$angle, rotation,
            inner = minor_tick_data$inner, outer = minor_tick_data$outer
        )
    }
    major_ticks <- if (nrow(major_tick_data)) {
        psychro_protractor_ticks(
            center, radius, major_tick_data$angle, rotation,
            inner = major_tick_data$inner, outer = major_tick_data$outer
        )
    }

    axis_labels <- NULL
    titles <- NULL
    if (isTRUE(protractor$label)) {
        shr_label_spec <- psychro_protractor_label_spec(
            shr_breaks, guide$shr_labels, "shr", units
        )
        ratio_label_spec <- psychro_protractor_label_spec(
            ratio_major_breaks, guide$ratio_labels, "ratio", units
        )

        label_ticks <- psychro_protractor_label_ticks(major_data, shr_label_spec)
        shr_labels <- psychro_protractor_label_data(
            center, radius, label_ticks$label, label_ticks$angle, rotation,
            label_ticks$scale, anchor = label_ticks$anchor, rot = label_ticks$rot,
            size_scale = label_ticks$size_scale
        )
        ratio_label_ticks <- psychro_protractor_ratio_label_ticks(
            ratio_major_data, ratio_label_spec
        )
        ratio_labels <- psychro_protractor_label_data(
            center, radius, ratio_label_ticks$label, ratio_label_ticks$angle, rotation,
            ratio_label_ticks$scale, anchor = ratio_label_ticks$anchor,
            rot = ratio_label_ticks$rot,
            size_scale = ratio_label_ticks$size_scale
        )
        infinity_labels <- psychro_protractor_infinity_labels(
            center, radius, rotation
        )
        axis_labels <- psychro_protractor_combine_label_data(
            shr_labels, ratio_labels, infinity_labels
        )
    }
    if (!isFALSE(protractor$annotation)) {
        titles <- psychro_protractor_titles(
            center, radius, rotation, protractor$annotation
        )
    }

    grobs <- list(
        grid::polylineGrob(
            arc$x, arc$y, gp = gp_line,
            name = "psychro-protractor-arc"
        ),
        grid::segmentsGrob(
            diameter$x[[1L]], diameter$y[[1L]], diameter$x[[2L]], diameter$y[[2L]],
            gp = gp_line,
            name = "psychro-protractor-diameter"
        ),
        grid::segmentsGrob(
            endpoint_caps$x0, endpoint_caps$y0, endpoint_caps$x1, endpoint_caps$y1,
            gp = gp_line,
            name = "psychro-protractor-end-caps"
        ),
        grid::segmentsGrob(
            center_mark$x0, center_mark$y0, center_mark$x1, center_mark$y1,
            gp = gp_line,
            name = "psychro-protractor-center-mark"
        ),
        grid::nullGrob(name = "psychro-protractor-center")
    )

    if (!is.null(minor_ticks)) {
        grobs <- c(grobs, list(grid::segmentsGrob(
            minor_ticks$x0, minor_ticks$y0, minor_ticks$x1, minor_ticks$y1,
            gp = gp_line,
            name = "psychro-protractor-minor-ticks"
        )))
    }
    if (!is.null(major_ticks)) {
        grobs <- c(grobs, list(grid::segmentsGrob(
            major_ticks$x0, major_ticks$y0, major_ticks$x1, major_ticks$y1,
            gp = gp_line,
            name = "psychro-protractor-major-ticks"
        )))
    }

    if (!is.null(axis_labels)) {
        axis_gp <- gp_text
        axis_gp$fontsize <- gp_text$fontsize * axis_labels$size_scale
        grobs <- c(grobs, list(grid::textGrob(
            axis_labels$label, axis_labels$x, axis_labels$y,
            hjust = axis_labels$hjust, vjust = axis_labels$vjust,
            rot = axis_labels$rot,
            check.overlap = isTRUE(guide$check_overlap),
            gp = axis_gp,
            name = "psychro-protractor-labels"
        )))
    }
    if (!is.null(titles)) {
        grobs <- c(grobs, list(grid::textGrob(
            titles$label, titles$x, titles$y,
            hjust = titles$hjust, vjust = titles$vjust, rot = titles$rot,
            check.overlap = isTRUE(guide$check_overlap),
            gp = gp_text,
            name = "psychro-protractor-titles"
        )))
    }

    do.call(grid::grobTree, c(grobs, list(name = "psychro-protractor")))
}

psychro_protractor_margin <- function(margin) {
    margin <- margin %||% default_psychro_protractor()$margin
    if (length(margin) == 1L) {
        margin <- rep(margin, 2L)
    }
    list(x = margin[[1L]], y = margin[[2L]])
}

psychro_protractor_center <- function(radius, margin, mollier = FALSE) {
    margin <- psychro_protractor_margin(margin)
    if (isTRUE(mollier)) {
        return(list(
            x = grid::unit(1, "npc") - grid::unit(margin$x, "snpc"),
            y = grid::unit(0, "npc") + grid::unit(margin$y + radius, "snpc")
        ))
    }

    list(
        x = grid::unit(0, "npc") + grid::unit(margin$x + radius, "snpc"),
        y = grid::unit(1, "npc") - grid::unit(margin$y, "snpc")
    )
}

psychro_protractor_points <- function(center, radius, angle, rotation = 0, scale = 1) {
    dx <- radius * scale * cos(angle)
    dy <- radius * scale * sin(angle)
    psychro_protractor_cartesian_points(center, dx, dy, rotation)
}

psychro_protractor_cartesian_points <- function(center, dx, dy, rotation = 0) {
    offset <- psychro_protractor_rotate_offset(dx, dy, rotation)

    list(
        x = center$x + grid::unit(offset$x, "snpc"),
        y = center$y + grid::unit(offset$y, "snpc")
    )
}

psychro_protractor_rotate_offset <- function(dx, dy, angle) {
    if (isTRUE(all.equal(angle, 0))) {
        return(list(x = dx, y = dy))
    }

    list(
        x = dx * cos(angle) - dy * sin(angle),
        y = dx * sin(angle) + dy * cos(angle)
    )
}

psychro_protractor_ticks <- function(center, radius, angle, rotation = 0,
                                     inner = 0.94, outer = 1.04) {
    inner <- psychro_protractor_points(center, radius, angle, rotation, scale = inner)
    outer <- psychro_protractor_points(center, radius, angle, rotation, scale = outer)

    list(x0 = inner$x, y0 = inner$y, x1 = outer$x, y1 = outer$y)
}

psychro_protractor_diameter <- function(center, radius, rotation = 0) {
    psychro_protractor_cartesian_points(
        center,
        c(-0.78 * radius, 0.78 * radius),
        c(0, 0),
        rotation
    )
}

psychro_protractor_endpoint_caps <- function(center, radius, rotation = 0) {
    start <- psychro_protractor_cartesian_points(
        center, c(-radius, radius), rep(-0.055 * radius, 2L), rotation
    )
    end <- psychro_protractor_cartesian_points(
        center, c(-radius, radius), rep(0.105 * radius, 2L), rotation
    )

    list(x0 = start$x, y0 = start$y, x1 = end$x, y1 = end$y)
}

psychro_protractor_center_mark <- function(center, radius, rotation = 0) {
    start <- psychro_protractor_cartesian_points(center, 0, -0.055 * radius, rotation)
    end <- psychro_protractor_cartesian_points(center, 0, 0.125 * radius, rotation)

    list(x0 = start$x, y0 = start$y, x1 = end$x, y1 = end$y)
}

psychro_protractor_label_data <- function(center, radius, labels, angle, rotation = 0,
                                          scale = 1.06, anchor = "outward",
                                          rotate = TRUE, rot = NULL,
                                          size_scale = 1) {
    if (!length(labels) || !length(angle)) {
        return(NULL)
    }
    scale <- scale %||% 1.06
    scale[is.na(scale)] <- 1.06
    pts <- psychro_protractor_points(center, radius, angle, rotation, scale = scale)
    offset <- psychro_protractor_rotate_offset(cos(angle), sin(angle), rotation)
    anchor <- rep_len(anchor, length(angle))
    hjust <- ifelse(offset$x < -0.15, 1, ifelse(offset$x > 0.15, 0, 0.5))
    vjust <- ifelse(offset$y < -0.15, 1, ifelse(offset$y > 0.15, 0, 0.5))
    center <- identical(anchor, "center") | anchor == "center"
    inward <- identical(anchor, "inward") | anchor == "inward"
    hjust[center] <- 0.5
    vjust[center] <- 0.5
    hjust[inward] <- ifelse(offset$x[inward] < -0.15, 0, ifelse(offset$x[inward] > 0.15, 1, 0.5))
    vjust[inward] <- ifelse(offset$y[inward] < -0.15, 0, ifelse(offset$y[inward] > 0.15, 1, 0.5))

    label_rot <- if (isTRUE(rotate)) {
        psychro_protractor_label_rotation(angle, rotation)
    } else {
        rep(0, length(angle))
    }
    if (!is.null(rot)) {
        replace <- !is.na(rot)
        label_rot[replace] <- rot[replace]
    }

    list(
        x = pts$x,
        y = pts$y,
        label = labels,
        hjust = hjust,
        vjust = vjust,
        rot = label_rot,
        size_scale = rep_len(size_scale, length(angle))
    )
}

psychro_protractor_combine_label_data <- function(...) {
    labels <- Filter(Negate(is.null), list(...))
    if (!length(labels)) {
        return(NULL)
    }

    first <- labels[[1L]]
    first$size_scale <- first$size_scale %||% rep(1, length(first$label))
    for (label in labels[-1L]) {
        label$size_scale <- label$size_scale %||% rep(1, length(label$label))
        first$x <- grid::unit.c(first$x, label$x)
        first$y <- grid::unit.c(first$y, label$y)
        first$label <- c(first$label, label$label)
        first$hjust <- c(first$hjust, label$hjust)
        first$vjust <- c(first$vjust, label$vjust)
        first$rot <- c(first$rot, label$rot)
        first$size_scale <- c(first$size_scale, label$size_scale)
    }
    first
}

psychro_protractor_label_rotation <- function(angle, rotation = 0) {
    offset <- psychro_protractor_rotate_offset(cos(angle), sin(angle), rotation)
    degrees <- atan2(offset$y, offset$x) * 180 / pi
    label_rotation <- ((degrees + 90) %% 180) - 90
    label_rotation[abs(label_rotation + 90) <= 1e-8 & degrees < 0] <- 90
    label_rotation
}

psychro_protractor_fixed_text_rotation <- function(rotation = 0) {
    rotation * 180 / pi
}

psychro_protractor_label_ticks <- function(ticks, labels) {
    if (is.null(labels) || !nrow(ticks)) {
        return(new_data_frame(list(
            value = numeric(), angle = numeric(), label = character(),
            scale = numeric(), rot = numeric(), anchor = character(),
            size_scale = numeric()
        )))
    }
    loc <- match_psychro_breaks(ticks$value, labels$breaks)
    keep <- !is.na(loc)
    ticks <- ticks[keep, , drop = FALSE]
    loc <- loc[keep]
    endpoint <- abs(ticks$value - 1) <= 1e-8 &
        (abs(ticks$angle - pi) <= 1e-8 | abs(ticks$angle - 2 * pi) <= 1e-8)
    tick_order <- c(which(endpoint), which(!endpoint))
    ticks <- ticks[tick_order, , drop = FALSE]
    loc <- loc[tick_order]
    endpoint <- endpoint[tick_order]
    if (!nrow(ticks)) {
        return(new_data_frame(list(
            value = numeric(), angle = numeric(), label = character(),
            scale = numeric(), rot = numeric(), anchor = character(),
            size_scale = numeric()
        )))
    }
    ticks$label <- labels$labels[loc]
    ticks$scale <- rep(0.86, nrow(ticks))
    ticks$rot <- NA_real_
    ticks$anchor <- rep("center", nrow(ticks))
    ticks$size_scale <- rep(1, nrow(ticks))
    ticks
}

psychro_protractor_ratio_label_ticks <- function(ticks, labels) {
    if (!nrow(ticks)) {
        return(new_data_frame(list(
            value = numeric(), angle = numeric(), label = character(),
            scale = numeric(), rot = numeric(), anchor = character(),
            size_scale = numeric()
        )))
    }
    if (is.null(labels)) {
        return(new_data_frame(list(
            value = numeric(), angle = numeric(), label = character(),
            scale = numeric(), rot = numeric(), anchor = character(),
            size_scale = numeric()
        )))
    }

    loc <- match_psychro_breaks(ticks$value, labels$breaks)
    keep <- !is.na(loc)
    ticks <- ticks[keep, , drop = FALSE]
    if (!nrow(ticks)) {
        return(new_data_frame(list(
            value = numeric(), angle = numeric(), label = character(),
            scale = numeric(), rot = numeric(), anchor = character(),
            size_scale = numeric()
        )))
    }
    ticks$label <- labels$labels[loc[keep]]
    ticks$scale <- rep(1.13, nrow(ticks))
    ticks$rot <- NA_real_
    ticks$anchor <- rep("center", nrow(ticks))
    ticks$size_scale <- rep(1, nrow(ticks))
    ticks
}

psychro_protractor_label_spec <- function(breaks, labels, axis, units) {
    breaks <- remove_na(breaks)
    breaks <- breaks[is.finite(breaks)]
    if (!length(breaks)) {
        return(NULL)
    }

    text <- psychro_protractor_label_text(breaks, labels, axis, units)
    if (is.null(text) || !length(text)) {
        return(NULL)
    }
    if (length(text) != length(breaks)) {
        stop(
            sprintf("`%s_labels` must return one label for each break.", axis),
            call. = FALSE
        )
    }

    list(breaks = breaks, labels = text)
}

psychro_protractor_label_text <- function(breaks, labels, axis, units) {
    if (is.null(labels)) {
        return(NULL)
    }
    if (is.waive(labels)) {
        if (identical(axis, "shr")) {
            return(psychro_format_shr_labels(breaks))
        }
        return(psychro_format_heat_ratio_labels(breaks, units))
    }
    if (is.function(labels)) {
        return(labels(breaks))
    }
    labels
}

psychro_format_heat_ratio_labels <- function(x, units = "SI") {
    if (identical(units, "IP")) {
        labels <- sprintf("%.2f", round(x, 2))
        labels[abs(x) >= 1] <- sprintf("%.1f", round(x[abs(x) >= 1], 1))
        labels[abs(x) <= 1e-8] <- "0"
        return(labels)
    }
    labels <- sprintf("%.1f", round(x, 1))
    labels[abs(x) <= 1e-8] <- "0"
    labels
}

psychro_protractor_infinity_labels <- function(center, radius, rotation = 0) {
    psychro_protractor_label_data(
        center, radius, expression(+infinity, -infinity), c(pi, 2 * pi),
        rotation, scale = 1.13, anchor = "center", size_scale = 1.45
    )
}

psychro_protractor_titles <- function(center, radius, rotation = 0, annotation = TRUE) {
    annotation <- psychro_protractor_annotation(annotation)
    if (is.null(annotation)) return(NULL)

    pts <- psychro_protractor_cartesian_points(
        center, c(0, 0), c(-0.32, -1.36) * radius, rotation
    )

    list(
        x = pts$x,
        y = pts$y,
        label = annotation,
        hjust = rep(0.5, 2L),
        vjust = rep(0.5, 2L),
        rot = rep(psychro_protractor_fixed_text_rotation(rotation), 2L)
    )
}

psychro_protractor_annotation <- function(annotation) {
    if (isFALSE(annotation)) {
        return(NULL)
    }
    if (is.character(annotation)) {
        return(annotation)
    }
    if (is.expression(annotation)) {
        return(annotation)
    }
    expression(
        frac("SENSIBLE HEAT", "TOTAL HEAT") == frac(Delta * H[s], Delta * H[t]),
        frac("ENTHALPY", "HUMIDITY RATIO") == frac(Delta * h, Delta * W)
    )
}

psychro_protractor_shr_ticks <- function(shr, range_tdb, range_hum, units) {
    shr <- remove_na(shr)
    shr <- shr[is.finite(shr)]
    if (!length(shr)) {
        return(new_data_frame(list(value = numeric(), angle = numeric())))
    }

    cp <- if (units == "IP") 0.24 else 1.006
    latent <- if (units == "IP") 1061 else 2501
    ratio <- diff(range_tdb) / diff(range_hum)

    slope_angle <- atan(cp / latent * (1 / shr - 1) * ratio)
    angle <- ifelse(slope_angle >= 0, pi + slope_angle, 2 * pi + slope_angle)
    angle[shr == 0] <- 3 * pi / 2

    new_data_frame(list(value = shr, angle = angle))
}

psychro_protractor_ratio_ticks <- function(ratio, range_tdb, range_hum, units) {
    value <- remove_na(ratio)
    value <- value[is.finite(value)]
    if (!length(value)) {
        return(new_data_frame(list(value = numeric(), angle = numeric())))
    }

    cp <- if (units == "IP") 0.24 else 1.006
    latent <- if (units == "IP") 1061 else 2501
    divisor <- psychro_protractor_ratio_divisor(units)
    axis_ratio <- diff(range_tdb) / diff(range_hum)

    heat_ratio <- value * divisor
    denominator <- heat_ratio - latent
    slope <- cp / denominator * axis_ratio
    slope_angle <- atan(slope)
    angle <- ifelse(slope >= 0, pi + slope_angle, 2 * pi + slope_angle)
    angle[abs(denominator) <= sqrt(.Machine$double.eps)] <- 3 * pi / 2

    new_data_frame(list(value = heat_ratio / divisor, angle = angle))
}

psychro_protractor_ratio_major_breaks <- function(units) {
    latent <- (if (units == "IP") 1061 else 2501) / psychro_protractor_ratio_divisor(units)
    if (identical(units, "IP")) {
        return(c(0.60, 0.40, 0.30, 0.20, latent, 0.10, 0.05, 0, -0.10, -0.20, -0.50))
    }
    c(10, 5, 4, 3, latent, 2, 1.5, 1, 0, -1, -2, -5)
}

psychro_protractor_ratio_minor_breaks <- function(units) {
    if (identical(units, "IP")) {
        return(c(0.50, 0.35, 0.25, 0.175, 0.15, 0.125, 0.075, -0.05, -0.15, -0.30))
    }
    c(8, 6, 4.5, 3.5, 2.25, 1.75, 1.25, 0.5, -0.5, -1.5, -3, -8, -10)
}

psychro_protractor_ratio_breaks <- function(breaks, units, minor = FALSE) {
    if (is.null(breaks)) {
        return(numeric())
    }
    if (is.waive(breaks)) {
        if (isTRUE(minor)) {
            return(psychro_protractor_ratio_minor_breaks(units))
        }
        return(psychro_protractor_ratio_major_breaks(units))
    }
    breaks
}

psychro_protractor_ratio_divisor <- function(units) {
    if (identical(units, "IP")) 7000 else 1000
}

psychro_protractor_tick_data <- function(ticks, axis) {
    if (!nrow(ticks)) {
        return(new_data_frame(list(
            axis = character(), value = numeric(), angle = numeric()
        )))
    }

    new_data_frame(list(
        axis = rep(axis, nrow(ticks)),
        value = ticks$value,
        angle = ticks$angle
    ))
}

psychro_protractor_unique_ticks <- function(ticks, major = FALSE) {
    if (!nrow(ticks)) {
        return(new_data_frame(list(
            axis = character(), value = numeric(), angle = numeric(),
            inner = numeric(), outer = numeric()
        )))
    }
    ticks <- ticks[is.finite(ticks$angle), , drop = FALSE]
    ticks <- ticks[order(ticks$angle), , drop = FALSE]
    ticks$key <- round(ticks$angle, 8)

    keys <- unique(ticks$key)
    out <- lapply(keys, function(key) {
        rows <- ticks[ticks$key == key, , drop = FALSE]
        has_shr <- any(rows$axis == "shr")
        has_ratio <- any(rows$axis == "ratio")
        axis <- if (has_shr && has_ratio) {
            "both"
        } else if (has_shr) {
            "shr"
        } else {
            "ratio"
        }

        lengths <- psychro_protractor_tick_scales(has_shr, has_ratio, major)
        new_data_frame(list(
            axis = axis,
            value = rows$value[[1L]],
            angle = rows$angle[[1L]],
            inner = lengths$inner,
            outer = lengths$outer
        ))
    })
    do.call(rbind, out)
}

psychro_protractor_tick_scales <- function(has_shr, has_ratio, major = FALSE) {
    if (isTRUE(major)) {
        return(list(
            inner = if (has_shr) 0.965 else 0.995,
            outer = if (has_ratio) 1.035 else 1.005
        ))
    }

    list(
        inner = if (has_shr) 0.978 else 0.997,
        outer = if (has_ratio) 1.022 else 1.003
    )
}

psychro_protractor_drop_tick_angles <- function(ticks, reference) {
    if (!nrow(ticks) || !nrow(reference)) {
        return(ticks)
    }
    ticks[!round(ticks$angle, 8) %in% round(reference$angle, 8), , drop = FALSE]
}

psychro_protractor_add_sensible_endpoint <- function(ticks, breaks) {
    if (!length(breaks) || !any(abs(breaks - 1) <= 1e-8)) {
        return(ticks)
    }

    rbind(
        ticks,
        new_data_frame(list(value = 1, angle = 2 * pi))
    )
}

psychro_protractor_line_style_defaults <- function(theme, style = list()) {
    line <- ggplot2::calc_element("psychro.panel.protractor", theme)
    defaults <- list(
        colour = line$colour %||% "black",
        linewidth = line$linewidth %||% line$size %||% 0.3,
        linetype = line$linetype %||% 1,
        lineend = line$lineend %||% "butt",
        alpha = NA_real_
    )

    utils::modifyList(defaults, style)
}

psychro_protractor_text_style_defaults <- function(theme, style = list()) {
    text <- ggplot2::calc_element("psychro.panel.protractor.text", theme)
    defaults <- list(
        colour = text$colour %||% "black",
        size = text$size %||% 1.9,
        alpha = NA_real_,
        family = text$family %||% "",
        fontface = text$face %||% 1,
        lineheight = text$lineheight %||% 1.2
    )

    utils::modifyList(defaults, style)
}

psychro_grid_label_grob <- function(grid, label, type, theme, mollier,
                                    panel_x, panel_y) {
    if (is.null(grid) || is.null(label) || !isTRUE(label$show)) {
        return(NULL)
    }

    data <- psychro_grid_label_data(grid, label$labels, mollier, panel_x, panel_y)
    if (!nrow(data$path)) return(NULL)

    style <- psychro_grid_label_style_defaults(type, theme, label$style)
    labels <- psychro_grid_normalise_labels(data$labels, label$label_parse)
    if (!length(labels)) return(NULL)

    colour <- psychro_grid_alpha(style$colour, style$alpha)
    gp_text <- grid::gpar(
        col = colour,
        fontsize = style$size * ggplot2::.pt,
        fontfamily = style$family,
        fontface = style$fontface,
        lineheight = style$lineheight
    )

    # Grid labels use the internal textpath renderer so CRAN examples avoid the
    # old external renderer's cold glyph-index cost.
    psychro_textpath_grob(
        label = labels,
        x = data$path$x,
        y = data$path$y,
        id = data$path$id,
        hjust = rep(label$label_loc, length(labels)),
        vjust = rep(style$vjust, length(labels)),
        upright = style$upright,
        straight = style$straight,
        remove_long = style$remove_long,
        gp_text = gp_text,
        default.units = "npc",
        name = paste0("psychro-grid-label-", type)
    )
}

psychro_grid_label_data <- function(grid, labels, mollier, panel_x, panel_y) {
    x <- grid[[c("tdb", "hum")[c(!mollier, mollier)]]]
    y <- grid[[c("tdb", "hum")[c(mollier, !mollier)]]]
    id <- grid$group

    label_missing <- psychro_grid_label_missing(labels)
    keep_group <- seq_along(labels)[!label_missing]
    inside <- psychro_inside_polygon(x, y, panel_x, panel_y) & id %in% keep_group

    pieces <- lapply(keep_group, function(group) {
        idx <- which(inside & id == group)
        if (length(idx) < 2L) return(NULL)
        new_data_frame(list(
            x = x[idx],
            y = y[idx],
            id = rep(group, length(idx))
        ))
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (!length(pieces)) {
        return(list(path = new_data_frame(list()), labels = labels[0]))
    }

    path <- do.call(rbind, pieces)
    used <- unique(path$id)
    path$id <- match(path$id, used)

    list(path = path, labels = labels[used])
}

psychro_grid_label_missing <- function(labels) {
    if (is.null(labels)) return(TRUE)
    if (is.atomic(labels)) return(is.na(labels))

    vapply(labels, function(label) {
        length(label) == 0L || anyNA(as.character(label))
    }, logical(1))
}

psychro_grid_normalise_labels <- function(labels, parse = FALSE) {
    if (is.null(labels)) return(labels)
    if (is.expression(labels)) return(labels)
    if (is.list(labels) && !is.data.frame(labels)) {
        return(as.expression(labels))
    }
    if (!parse) return(labels)

    as.expression(lapply(as.character(labels), function(label) {
        parsed <- parse(text = label)
        if (length(parsed)) parsed[[1L]] else NA
    }))
}

psychro_grid_label_style_defaults <- function(type, theme, style = list()) {
    line <- ggplot2::calc_element(
        paste("psychro.panel.grid.major", type, sep = "."),
        theme
    )
    defaults <- list(
        colour = line$colour %||% "black",
        size = 3.2,
        alpha = NA_real_,
        family = "",
        fontface = 1,
        lineheight = 1.2,
        vjust = -0.3,
        gap = NA,
        upright = TRUE,
        straight = FALSE,
        padding = grid::unit(0.05, "inch"),
        remove_long = FALSE
    )

    utils::modifyList(defaults, style)
}

psychro_grid_alpha <- function(colour, alpha) {
    if (is.null(alpha) || length(alpha) == 0L || is.na(alpha)) {
        return(colour)
    }
    grDevices::adjustcolor(colour, alpha.f = alpha)
}

psychro_inside_polygon <- function(x, y, polygon_x, polygon_y, tolerance = 1e-8) {
    n <- length(polygon_x)
    inside <- rep(FALSE, length(x))
    on_boundary <- rep(FALSE, length(x))
    j <- n

    for (i in seq_len(n)) {
        xi <- polygon_x[[i]]
        yi <- polygon_y[[i]]
        xj <- polygon_x[[j]]
        yj <- polygon_y[[j]]

        cross <- (x - xi) * (yj - yi) - (y - yi) * (xj - xi)
        within <- x >= min(xi, xj) - tolerance &
            x <= max(xi, xj) + tolerance &
            y >= min(yi, yj) - tolerance &
            y <= max(yi, yj) + tolerance
        on_boundary <- on_boundary | (abs(cross) <= tolerance & within)

        intersects <- ((yi > y) != (yj > y)) &
            (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
        inside[intersects] <- !inside[intersects]
        j <- i
    }

    inside | on_boundary
}

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}
