# Build the optional sensible-heat-ratio protractor guide grob.
# Resolve and validate the protractor guide configuration.
protractor__guide <- function(protractor) {
    guide <- protractor$guide %||% guide_psychro_protractor()
    protractor__validate_guide(guide)
    guide
}

# Resolve user-supplied, waived, or disabled SHR break vectors.
protractor__breaks <- function(breaks, minor = FALSE) {
    if (is.null(breaks)) {
        return(numeric())
    }
    if (util__is_waive(breaks)) {
        if (isTRUE(minor)) {
            return(protractor__shr_minor_breaks())
        }
        return(protractor__shr_major_breaks())
    }
    breaks
}

# Return the default major SHR breaks used by the protractor.
protractor__shr_major_breaks <- function() {
    c(
        -5,
        -2,
        -1,
        -0.5,
        -0.3,
        -0.2,
        0,
        0.2,
        0.4,
        0.6,
        0.7,
        0.8,
        1,
        1.5,
        2,
        4
    )
}

# Return the default minor SHR breaks used by the protractor.
protractor__shr_minor_breaks <- function() {
    c(
        -10,
        -4,
        -3,
        -1.5,
        -0.1,
        0.1,
        0.3,
        0.5,
        0.9,
        1.2,
        1.8,
        3,
        5,
        8,
        10
    )
}

# Assemble the full protractor grob from line, tick, label, and title pieces.
protractor__grob <- function(
    protractor,
    theme,
    mollier,
    range_tdb,
    range_hum,
    units
) {
    if (!is.list(protractor) || !isTRUE(protractor$show)) {
        return(NULL)
    }

    scale <- protractor$scale %||% psychro__default_protractor()$scale
    radius <- protractor$radius %||% psychro__default_protractor()$radius
    margin <- protractor__margin(
        protractor$margin %||% psychro__default_protractor()$margin
    )
    radius <- min(radius * scale, 0.5 - max(margin$x, margin$y))

    center <- protractor__center(radius, margin, mollier)
    rotation <- if (mollier) -pi / 2 else 0

    line_style <- protractor__line_style_defaults(
        theme,
        protractor$style %||% list()
    )
    text_style <- protractor__text_style_defaults(
        theme,
        protractor$label_style %||% list()
    )

    gp_line <- grid::gpar(
        col = util__apply_alpha(line_style$colour, line_style$alpha),
        lwd = line_style$linewidth * scale * ggplot2::.pt,
        lty = line_style$linetype,
        lineend = line_style$lineend
    )
    gp_text <- grid::gpar(
        col = util__apply_alpha(text_style$colour, text_style$alpha),
        fontsize = text_style$size * scale * ggplot2::.pt,
        fontfamily = text_style$family,
        fontface = text_style$fontface,
        lineheight = text_style$lineheight
    )

    arc_angle <- seq(pi, 2 * pi, length.out = 96L)
    arc <- protractor__points(center, radius, arc_angle, rotation)
    diameter <- protractor__diameter(center, radius, rotation)
    endpoint_caps <- protractor__endpoint_caps(center, radius, rotation)
    center_mark <- protractor__center_mark(center, radius, rotation)

    guide <- protractor__guide(protractor)
    shr_breaks <- protractor__breaks(guide$shr_breaks)
    shr_minor_breaks <- protractor__breaks(
        guide$shr_minor_breaks,
        minor = TRUE
    )
    ratio_major_breaks <- protractor__ratio_breaks(
        guide$ratio_breaks,
        units
    )
    ratio_minor_breaks <- protractor__ratio_breaks(
        guide$ratio_minor_breaks,
        units,
        minor = TRUE
    )

    minor_data <- protractor__shr_ticks(
        shr_minor_breaks,
        range_tdb,
        range_hum,
        units
    )
    major_data <- protractor__shr_ticks(
        shr_breaks,
        range_tdb,
        range_hum,
        units
    )
    major_data <- protractor__add_sensible_endpoint(
        major_data,
        shr_breaks
    )
    ratio_major_data <- protractor__ratio_ticks(
        ratio_major_breaks,
        range_tdb,
        range_hum,
        units
    )
    ratio_minor_data <- protractor__ratio_ticks(
        ratio_minor_breaks,
        range_tdb,
        range_hum,
        units
    )
    major_tick_data <- protractor__unique_ticks(
        rbind(
            protractor__tick_data(major_data, "shr"),
            protractor__tick_data(ratio_major_data, "ratio")
        ),
        major = TRUE
    )
    minor_tick_data <- protractor__unique_ticks(
        rbind(
            protractor__tick_data(minor_data, "shr"),
            protractor__tick_data(ratio_minor_data, "ratio")
        ),
        major = FALSE
    )
    minor_tick_data <- protractor__drop_tick_angles(
        minor_tick_data,
        major_tick_data
    )
    minor_ticks <- if (nrow(minor_tick_data)) {
        protractor__ticks(
            center,
            radius,
            minor_tick_data$angle,
            rotation,
            inner = minor_tick_data$inner,
            outer = minor_tick_data$outer
        )
    }
    major_ticks <- if (nrow(major_tick_data)) {
        protractor__ticks(
            center,
            radius,
            major_tick_data$angle,
            rotation,
            inner = major_tick_data$inner,
            outer = major_tick_data$outer
        )
    }

    axis_labels <- NULL
    titles <- NULL
    if (isTRUE(protractor$label)) {
        shr_label_spec <- protractor__label_spec(
            shr_breaks,
            guide$shr_labels,
            "shr",
            units
        )
        ratio_label_spec <- protractor__label_spec(
            ratio_major_breaks,
            guide$ratio_labels,
            "ratio",
            units
        )

        label_ticks <- protractor__label_ticks(
            major_data,
            shr_label_spec
        )
        shr_labels <- protractor__label_data(
            center,
            radius,
            label_ticks$label,
            label_ticks$angle,
            rotation,
            label_ticks$scale,
            anchor = label_ticks$anchor,
            rot = label_ticks$rot,
            size_scale = label_ticks$size_scale
        )
        ratio_label_ticks <- protractor__ratio_label_ticks(
            ratio_major_data,
            ratio_label_spec
        )
        ratio_labels <- protractor__label_data(
            center,
            radius,
            ratio_label_ticks$label,
            ratio_label_ticks$angle,
            rotation,
            ratio_label_ticks$scale,
            anchor = ratio_label_ticks$anchor,
            rot = ratio_label_ticks$rot,
            size_scale = ratio_label_ticks$size_scale
        )
        infinity_labels <- protractor__infinity_labels(
            center,
            radius,
            rotation
        )
        axis_labels <- protractor__combine_label_data(
            shr_labels,
            ratio_labels,
            infinity_labels
        )
    }
    if (!isFALSE(protractor$annotation)) {
        titles <- protractor__titles(
            center,
            radius,
            rotation,
            protractor$annotation
        )
    }

    grobs <- list(
        grid::polylineGrob(
            arc$x,
            arc$y,
            gp = gp_line,
            name = "psychro-protractor-arc"
        ),
        grid::segmentsGrob(
            diameter$x[[1L]],
            diameter$y[[1L]],
            diameter$x[[2L]],
            diameter$y[[2L]],
            gp = gp_line,
            name = "psychro-protractor-diameter"
        ),
        grid::segmentsGrob(
            endpoint_caps$x0,
            endpoint_caps$y0,
            endpoint_caps$x1,
            endpoint_caps$y1,
            gp = gp_line,
            name = "psychro-protractor-end-caps"
        ),
        grid::segmentsGrob(
            center_mark$x0,
            center_mark$y0,
            center_mark$x1,
            center_mark$y1,
            gp = gp_line,
            name = "psychro-protractor-center-mark"
        ),
        grid::nullGrob(name = "psychro-protractor-center")
    )

    if (!is.null(minor_ticks)) {
        grobs <- c(
            grobs,
            list(grid::segmentsGrob(
                minor_ticks$x0,
                minor_ticks$y0,
                minor_ticks$x1,
                minor_ticks$y1,
                gp = gp_line,
                name = "psychro-protractor-minor-ticks"
            ))
        )
    }
    if (!is.null(major_ticks)) {
        grobs <- c(
            grobs,
            list(grid::segmentsGrob(
                major_ticks$x0,
                major_ticks$y0,
                major_ticks$x1,
                major_ticks$y1,
                gp = gp_line,
                name = "psychro-protractor-major-ticks"
            ))
        )
    }

    if (!is.null(axis_labels)) {
        axis_gp <- gp_text
        axis_gp$fontsize <- gp_text$fontsize * axis_labels$size_scale
        grobs <- c(
            grobs,
            list(grid::textGrob(
                axis_labels$label,
                axis_labels$x,
                axis_labels$y,
                hjust = axis_labels$hjust,
                vjust = axis_labels$vjust,
                rot = axis_labels$rot,
                check.overlap = isTRUE(guide$check_overlap),
                gp = axis_gp,
                name = "psychro-protractor-labels"
            ))
        )
    }
    if (!is.null(titles)) {
        grobs <- c(
            grobs,
            list(grid::textGrob(
                titles$label,
                titles$x,
                titles$y,
                hjust = titles$hjust,
                vjust = titles$vjust,
                rot = titles$rot,
                check.overlap = isTRUE(guide$check_overlap),
                gp = gp_text,
                name = "psychro-protractor-titles"
            ))
        )
    }

    do.call(grid::grobTree, c(grobs, list(name = "psychro-protractor")))
}

# Normalize scalar or two-value protractor margins to x/y components.
protractor__margin <- function(margin) {
    margin <- margin %||% psychro__default_protractor()$margin
    if (length(margin) == 1L) {
        margin <- rep(margin, 2L)
    }
    list(x = margin[[1L]], y = margin[[2L]])
}

# Locate the protractor center in panel coordinates for normal or Mollier plots.
protractor__center <- function(radius, margin, mollier = FALSE) {
    margin <- protractor__margin(margin)
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

# Convert polar protractor angles to rotated panel unit points.
protractor__points <- function(
    center,
    radius,
    angle,
    rotation = 0,
    scale = 1
) {
    dx <- radius * scale * cos(angle)
    dy <- radius * scale * sin(angle)
    protractor__cartesian_points(center, dx, dy, rotation)
}

# Convert Cartesian offsets around the protractor center to panel units.
protractor__cartesian_points <- function(center, dx, dy, rotation = 0) {
    offset <- protractor__rotate_offset(dx, dy, rotation)

    list(
        x = center$x + grid::unit(offset$x, "snpc"),
        y = center$y + grid::unit(offset$y, "snpc")
    )
}

# Rotate x/y offsets around the protractor center.
protractor__rotate_offset <- function(dx, dy, angle) {
    if (isTRUE(all.equal(angle, 0))) {
        return(list(x = dx, y = dy))
    }

    list(
        x = dx * cos(angle) - dy * sin(angle),
        y = dx * sin(angle) + dy * cos(angle)
    )
}

# Build inner and outer tick segment endpoints for protractor angles.
protractor__ticks <- function(
    center,
    radius,
    angle,
    rotation = 0,
    inner = 0.94,
    outer = 1.04
) {
    inner <- protractor__points(
        center,
        radius,
        angle,
        rotation,
        scale = inner
    )
    outer <- protractor__points(
        center,
        radius,
        angle,
        rotation,
        scale = outer
    )

    list(x0 = inner$x, y0 = inner$y, x1 = outer$x, y1 = outer$y)
}

# Build the protractor diameter endpoints.
protractor__diameter <- function(center, radius, rotation = 0) {
    protractor__cartesian_points(
        center,
        c(-0.78 * radius, 0.78 * radius),
        c(0, 0),
        rotation
    )
}

# Build short end-cap segments at both ends of the protractor arc.
protractor__endpoint_caps <- function(center, radius, rotation = 0) {
    start <- protractor__cartesian_points(
        center,
        c(-radius, radius),
        rep(-0.055 * radius, 2L),
        rotation
    )
    end <- protractor__cartesian_points(
        center,
        c(-radius, radius),
        rep(0.105 * radius, 2L),
        rotation
    )

    list(x0 = start$x, y0 = start$y, x1 = end$x, y1 = end$y)
}

# Build the short center mark segment on the protractor diameter.
protractor__center_mark <- function(center, radius, rotation = 0) {
    start <- protractor__cartesian_points(
        center,
        0,
        -0.055 * radius,
        rotation
    )
    end <- protractor__cartesian_points(
        center,
        0,
        0.125 * radius,
        rotation
    )

    list(x0 = start$x, y0 = start$y, x1 = end$x, y1 = end$y)
}

# Position label text around the protractor while preserving readable anchors.
protractor__label_data <- function(
    center,
    radius,
    labels,
    angle,
    rotation = 0,
    scale = 1.06,
    anchor = "outward",
    rotate = TRUE,
    rot = NULL,
    size_scale = 1
) {
    if (!length(labels) || !length(angle)) {
        return(NULL)
    }
    scale <- scale %||% 1.06
    scale[is.na(scale)] <- 1.06
    pts <- protractor__points(
        center,
        radius,
        angle,
        rotation,
        scale = scale
    )
    offset <- protractor__rotate_offset(cos(angle), sin(angle), rotation)
    anchor <- rep_len(anchor, length(angle))
    hjust <- ifelse(offset$x < -0.15, 1, ifelse(offset$x > 0.15, 0, 0.5))
    vjust <- ifelse(offset$y < -0.15, 1, ifelse(offset$y > 0.15, 0, 0.5))
    center <- identical(anchor, "center") | anchor == "center"
    inward <- identical(anchor, "inward") | anchor == "inward"
    hjust[center] <- 0.5
    vjust[center] <- 0.5
    hjust[inward] <- ifelse(
        offset$x[inward] < -0.15,
        0,
        ifelse(offset$x[inward] > 0.15, 1, 0.5)
    )
    vjust[inward] <- ifelse(
        offset$y[inward] < -0.15,
        0,
        ifelse(offset$y[inward] > 0.15, 1, 0.5)
    )

    label_rot <- if (isTRUE(rotate)) {
        protractor__label_rotation(angle, rotation)
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

# Combine label data blocks that contain grid unit vectors and plain vectors.
protractor__combine_label_data <- function(...) {
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

# Compute tangent-friendly text rotation for labels at protractor angles.
protractor__label_rotation <- function(angle, rotation = 0) {
    offset <- protractor__rotate_offset(cos(angle), sin(angle), rotation)
    degrees <- atan2(offset$y, offset$x) * 180 / pi
    label_rotation <- ((degrees + 90) %% 180) - 90
    label_rotation[abs(label_rotation + 90) <= 1e-8 & degrees < 0] <- 90
    label_rotation
}

# Convert fixed protractor rotation from radians to grid text degrees.
protractor__fixed_text_rotation <- function(rotation = 0) {
    rotation * 180 / pi
}

# Match SHR label specs to the visible major tick data.
protractor__label_ticks <- function(ticks, labels) {
    if (is.null(labels) || !nrow(ticks)) {
        return(util__new_data_frame(list(
            value = numeric(),
            angle = numeric(),
            label = character(),
            scale = numeric(),
            rot = numeric(),
            anchor = character(),
            size_scale = numeric()
        )))
    }
    loc <- guide__match_break_values(ticks$value, labels$breaks)
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
        return(util__new_data_frame(list(
            value = numeric(),
            angle = numeric(),
            label = character(),
            scale = numeric(),
            rot = numeric(),
            anchor = character(),
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

# Match enthalpy/humidity-ratio label specs to visible ratio tick data.
protractor__ratio_label_ticks <- function(ticks, labels) {
    if (!nrow(ticks)) {
        return(util__new_data_frame(list(
            value = numeric(),
            angle = numeric(),
            label = character(),
            scale = numeric(),
            rot = numeric(),
            anchor = character(),
            size_scale = numeric()
        )))
    }
    if (is.null(labels)) {
        return(util__new_data_frame(list(
            value = numeric(),
            angle = numeric(),
            label = character(),
            scale = numeric(),
            rot = numeric(),
            anchor = character(),
            size_scale = numeric()
        )))
    }

    loc <- guide__match_break_values(ticks$value, labels$breaks)
    keep <- !is.na(loc)
    ticks <- ticks[keep, , drop = FALSE]
    if (!nrow(ticks)) {
        return(util__new_data_frame(list(
            value = numeric(),
            angle = numeric(),
            label = character(),
            scale = numeric(),
            rot = numeric(),
            anchor = character(),
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

# Build a validated label specification from breaks and user label settings.
protractor__label_spec <- function(breaks, labels, axis, units) {
    breaks <- util__remove_na(breaks)
    breaks <- breaks[is.finite(breaks)]
    if (!length(breaks)) {
        return(NULL)
    }

    text <- protractor__label_text(breaks, labels, axis, units)
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

# Resolve waived, function, or literal labels for a protractor axis.
protractor__label_text <- function(breaks, labels, axis, units) {
    if (is.null(labels)) {
        return(NULL)
    }
    if (util__is_waive(labels)) {
        if (identical(axis, "shr")) {
            return(protractor__format_shr_labels(breaks))
        }
        return(protractor__format_heat_ratio_labels(breaks, units))
    }
    if (is.function(labels)) {
        return(labels(breaks))
    }
    labels
}

# Format sensible-heat-ratio labels without trailing decimal noise at zero.
protractor__format_shr_labels <- function(x) {
    labels <- sprintf("%.1f", x)
    labels[abs(x) <= 1e-8] <- "0"
    labels
}

# Format enthalpy/humidity-ratio labels with unit-specific precision.
protractor__format_heat_ratio_labels <- function(x, units = "SI") {
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

# Add the positive and negative infinity labels at the protractor endpoints.
protractor__infinity_labels <- function(center, radius, rotation = 0) {
    protractor__label_data(
        center,
        radius,
        expression(+infinity, -infinity),
        c(pi, 2 * pi),
        rotation,
        scale = 1.13,
        anchor = "center",
        size_scale = 1.45
    )
}

# Position the two annotation titles inside the protractor.
protractor__titles <- function(
    center,
    radius,
    rotation = 0,
    annotation = TRUE
) {
    annotation <- protractor__annotation(annotation)
    if (is.null(annotation)) {
        return(NULL)
    }

    pts <- protractor__cartesian_points(
        center,
        c(0, 0),
        c(-0.32, -1.36) * radius,
        rotation
    )

    list(
        x = pts$x,
        y = pts$y,
        label = annotation,
        hjust = rep(0.5, 2L),
        vjust = rep(0.5, 2L),
        rot = rep(protractor__fixed_text_rotation(rotation), 2L)
    )
}

# Resolve default, custom, or disabled protractor annotations.
protractor__annotation <- function(annotation) {
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

# Convert SHR break values into protractor tick angles.
protractor__shr_ticks <- function(shr, range_tdb, range_hum, units) {
    shr <- util__remove_na(shr)
    shr <- shr[is.finite(shr)]
    if (!length(shr)) {
        return(util__new_data_frame(list(value = numeric(), angle = numeric())))
    }

    cp <- if (units == "IP") 0.24 else 1.006
    latent <- if (units == "IP") 1061 else 2501
    ratio <- diff(range_tdb) / diff(range_hum)

    slope_angle <- atan(cp / latent * (1 / shr - 1) * ratio)
    angle <- ifelse(slope_angle >= 0, pi + slope_angle, 2 * pi + slope_angle)
    angle[shr == 0] <- 3 * pi / 2

    util__new_data_frame(list(value = shr, angle = angle))
}

# Convert enthalpy/humidity-ratio break values into protractor tick angles.
protractor__ratio_ticks <- function(ratio, range_tdb, range_hum, units) {
    value <- util__remove_na(ratio)
    value <- value[is.finite(value)]
    if (!length(value)) {
        return(util__new_data_frame(list(value = numeric(), angle = numeric())))
    }

    cp <- if (units == "IP") 0.24 else 1.006
    latent <- if (units == "IP") 1061 else 2501
    divisor <- protractor__ratio_divisor(units)
    axis_ratio <- diff(range_tdb) / diff(range_hum)

    heat_ratio <- value * divisor
    denominator <- heat_ratio - latent
    slope <- cp / denominator * axis_ratio
    slope_angle <- atan(slope)
    angle <- ifelse(slope >= 0, pi + slope_angle, 2 * pi + slope_angle)
    angle[abs(denominator) <= sqrt(.Machine$double.eps)] <- 3 * pi / 2

    util__new_data_frame(list(value = heat_ratio / divisor, angle = angle))
}

# Return unit-specific major enthalpy/humidity-ratio breaks.
protractor__ratio_major_breaks <- function(units) {
    latent <- (if (units == "IP") 1061 else 2501) /
        protractor__ratio_divisor(units)
    if (identical(units, "IP")) {
        return(c(
            0.60,
            0.40,
            0.30,
            0.20,
            latent,
            0.10,
            0.05,
            0,
            -0.10,
            -0.20,
            -0.50
        ))
    }
    c(10, 5, 4, 3, latent, 2, 1.5, 1, 0, -1, -2, -5)
}

# Return unit-specific minor enthalpy/humidity-ratio breaks.
protractor__ratio_minor_breaks <- function(units) {
    if (identical(units, "IP")) {
        return(c(
            0.50,
            0.35,
            0.25,
            0.175,
            0.15,
            0.125,
            0.075,
            -0.05,
            -0.15,
            -0.30
        ))
    }
    c(8, 6, 4.5, 3.5, 2.25, 1.75, 1.25, 0.5, -0.5, -1.5, -3, -8, -10)
}

# Resolve user-supplied, waived, or disabled ratio break vectors.
protractor__ratio_breaks <- function(breaks, units, minor = FALSE) {
    if (is.null(breaks)) {
        return(numeric())
    }
    if (util__is_waive(breaks)) {
        if (isTRUE(minor)) {
            return(protractor__ratio_minor_breaks(units))
        }
        return(protractor__ratio_major_breaks(units))
    }
    breaks
}

# Return the unit divisor used to display humidity-ratio-derived heat ratios.
protractor__ratio_divisor <- function(units) {
    if (identical(units, "IP")) 7000 else 1000
}

# Normalize tick data to a common axis/value/angle table.
protractor__tick_data <- function(ticks, axis) {
    if (!nrow(ticks)) {
        return(util__new_data_frame(list(
            axis = character(),
            value = numeric(),
            angle = numeric()
        )))
    }

    util__new_data_frame(list(
        axis = rep(axis, nrow(ticks)),
        value = ticks$value,
        angle = ticks$angle
    ))
}

# Merge coincident SHR and ratio ticks into one drawable tick per angle.
protractor__unique_ticks <- function(ticks, major = FALSE) {
    if (!nrow(ticks)) {
        return(util__new_data_frame(list(
            axis = character(),
            value = numeric(),
            angle = numeric(),
            inner = numeric(),
            outer = numeric()
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

        lengths <- protractor__tick_scales(has_shr, has_ratio, major)
        util__new_data_frame(list(
            axis = axis,
            value = rows$value[[1L]],
            angle = rows$angle[[1L]],
            inner = lengths$inner,
            outer = lengths$outer
        ))
    })
    do.call(rbind, out)
}

# Choose inner and outer tick lengths based on which axes share the angle.
protractor__tick_scales <- function(has_shr, has_ratio, major = FALSE) {
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

# Drop minor ticks whose angles are already occupied by major ticks.
protractor__drop_tick_angles <- function(ticks, reference) {
    if (!nrow(ticks) || !nrow(reference)) {
        return(ticks)
    }
    ticks[!round(ticks$angle, 8) %in% round(reference$angle, 8), , drop = FALSE]
}

# Add the second SHR = 1 endpoint so both diameter ends can be labeled.
protractor__add_sensible_endpoint <- function(ticks, breaks) {
    if (!length(breaks) || !any(abs(breaks - 1) <= 1e-8)) {
        return(ticks)
    }

    rbind(
        ticks,
        util__new_data_frame(list(value = 1, angle = 2 * pi))
    )
}

# Resolve protractor line style defaults from the theme.
protractor__line_style_defaults <- function(theme, style = list()) {
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

# Resolve protractor text style defaults from the theme.
protractor__text_style_defaults <- function(theme, style = list()) {
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
