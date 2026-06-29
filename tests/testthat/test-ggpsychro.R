collect_grobs <- function(grob) {
    children <- c(
        if (!is.null(grob$grobs)) grob$grobs else list(),
        if (!is.null(grob$children)) as.list(grob$children) else list()
    )

    c(list(grob), unlist(lapply(children, collect_grobs), recursive = FALSE))
}

count_line_shapes <- function(plot) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    sum(vapply(grobs, function(grob) {
        inherits(grob, "polyline") || inherits(grob, "polygon")
    }, logical(1)))
}

count_textpath_shapes <- function(plot) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    sum(vapply(grobs, function(grob) {
        inherits(grob, "textpath")
    }, logical(1)))
}

count_named_grobs <- function(plot, pattern) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    sum(vapply(grobs, function(grob) {
        name <- grob$name
        !is.null(name) && grepl(pattern, name)
    }, logical(1)))
}

find_named_grobs <- function(plot, pattern) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    grobs[vapply(grobs, function(grob) {
        name <- grob$name
        !is.null(name) && grepl(pattern, name)
    }, logical(1))]
}

convert_units_in_rectangular_viewport <- function(x, y) {
    path <- tempfile(fileext = ".pdf")
    grDevices::pdf(path, width = 8, height = 4)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(width = grid::unit(8, "cm"), height = grid::unit(4, "cm")))
    out <- list(
        x = grid::convertX(x, "mm", valueOnly = TRUE),
        y = grid::convertY(y, "mm", valueOnly = TRUE)
    )
    grid::popViewport()
    grDevices::dev.off()
    unlink(path)
    out
}

protractor_arc_radius_mm <- function(plot) {
    arc <- find_named_grobs(plot, "psychro-protractor-arc")[[1L]]
    arc_mm <- convert_units_in_rectangular_viewport(arc$x, arc$y)
    diff(range(arc_mm$x)) / 2
}

expect_trained_panel_ranges <- function(plot) {
    built <- ggplot2::ggplot_build(plot)
    panel <- built$layout$panel_params[[1L]]

    expect_length(panel$x$scale$range$range, 2L)
    expect_length(panel$y$scale$range$range, 2L)
    expect_gt(diff(panel$x$continuous_range), 0)
    expect_gt(diff(panel$y$continuous_range), 0)
    expect_false(
        isTRUE(all.equal(panel$x$continuous_range, c(0, 1))) &&
            isTRUE(all.equal(panel$y$continuous_range, c(0, 1)))
    )

    invisible(built)
}

test_that("Psychrometric chart creation", {
    expect_s3_class(p <- ggpsychro(), "ggpsychro")
    expect_true(!is.null(p$psychro))
    expect_equal(p$psychro$tdb_lim, NULL)
    expect_equal(p$psychro$hum_lim, NULL)
    expect_equal(
        p$psychro,
        list(
            mollier = FALSE,
            units = "SI",
            altitude = 0,
            tdb_lim = NULL,
            hum_lim = NULL,
            grids = default_psychro_grids(),
            grid_labels = list(),
            protractor = default_psychro_protractor()
        )
    )
    expect_s3_class(p$coordinates, "CoordPsychro")
    expect_type(p$labels$x, "expression")
    expect_type(p$labels$y, "expression")
    expect_true(is.ggpsychro(p))

    vdiffr::expect_doppelganger(
        "empty chart",
        ggpsychro()
    )

    vdiffr::expect_doppelganger(
        "empty mollier chart",
        ggpsychro(mollier = TRUE)
    )

    vdiffr::expect_doppelganger(
        "empty chart with tdb limits",
        ggpsychro(tdb_lim = c(0, 50))
    )

    vdiffr::expect_doppelganger(
        "empty chart with hum limits",
        ggpsychro(hum_lim = c(0, 50))
    )

    vdiffr::expect_doppelganger(
        "basic chart",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50))
    )

    vdiffr::expect_doppelganger(
        "basic chart in IP units",
        ggpsychro(tdb_lim = c(32, 122), hum_lim = c(0, 350), units = "IP")
    )

    vdiffr::expect_doppelganger(
        "basic mollier chart in SI units",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE)
    )
})

test_that("Empty psychrometric charts train display ranges", {
    expect_trained_panel_ranges(ggpsychro())
    expect_trained_panel_ranges(ggpsychro(mollier = TRUE))
    expect_trained_panel_ranges(ggpsychro(tdb_lim = c(0, 50)))
    expect_trained_panel_ranges(ggpsychro(hum_lim = c(0, 50)))

    expect_gt(count_line_shapes(ggpsychro()), 10L)
    expect_gt(count_line_shapes(ggpsychro(mollier = TRUE)), 10L)
})

test_that("Relative humidity grid breaks use psychrolib fractions", {
    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_grid_relhum()
    )
    breaks <- remove_na(built$layout$panel_params[[1L]]$relhum$get_breaks())
    expect_equal(breaks, c(0.25, 0.50, 0.75, 1.00), tolerance = 1e-8)

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_grid_relhum() +
            scale_relhum_continuous(
                limits = c(25, 100),
                breaks = seq(25, 100, by = 25),
                minor_breaks = NULL
            )
    )
    breaks <- remove_na(built$layout$panel_params[[1L]]$relhum$get_breaks())
    expect_equal(breaks, c(0.25, 0.50, 0.75, 1.00), tolerance = 1e-8)

    expect_equal(
        valid_relhum_grid_breaks(c(NA, 0, 0.25, 0.50, 1.00, 1.25)),
        c(0.25, 0.50)
    )
})

test_that("Psychrometric grid helpers update coord metadata", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_grid_relhum(color = "red", label.size = 5) +
        geom_grid_enthalpy()

    expect_length(p$layers, 0L)
    expect_true(p$psychro$grids$relhum)
    expect_true(p$psychro$grids$enthalpy)
    expect_true(p$psychro$grid_labels$relhum$show)
    expect_equal(p$psychro$grid_labels$relhum$label_loc, 0.95)
    expect_equal(p$psychro$grid_labels$relhum$style$size, 5)
    expect_true(p$coordinates$grids$relhum)
    expect_true(p$coordinates$grids$enthalpy)
    expect_equal(p$coordinates$grid_labels, p$psychro$grid_labels)
    expect_no_error(ggplot2::ggplot_build(p))

    p <- p + geom_grid_relhum(show = FALSE)
    expect_false(p$psychro$grids$relhum)
    expect_false(p$psychro$grid_labels$relhum$show)
    expect_false(p$coordinates$grids$relhum)
    expect_no_error(ggplot2::ggplot_build(p))

    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_grid_relhum(label.size = 5) +
        geom_grid_relhum(label.size = 2, label.vjust = 0.8)
    expect_equal(p$psychro$grid_labels$relhum$style$size, 2)
    expect_equal(p$psychro$grid_labels$relhum$style$vjust, 0.8)
})

test_that("Psychrometric protractor helper updates coord metadata", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_protractor(color = "red", label.size = 5)

    expect_length(p$layers, 0L)
    expect_true(p$psychro$protractor$show)
    expect_true(p$psychro$protractor$label)
    expect_true(p$psychro$protractor$annotation)
    expect_equal(p$psychro$protractor$scale, 1)
    expect_s3_class(p$psychro$protractor$guide, "PsyProtractorGuide")
    expect_equal(p$psychro$protractor$style$colour, "red")
    expect_equal(p$psychro$protractor$label_style$size, 5)
    expect_equal(p$coordinates$protractor, p$psychro$protractor)
    expect_no_error(ggplot2::ggplot_build(p))
    expect_gt(count_named_grobs(p, "psychro-protractor"), 0L)
    arc <- find_named_grobs(p, "psychro-protractor-arc")[[1L]]
    expect_s3_class(arc$x, "unit")
    expect_s3_class(arc$y, "unit")
    arc_mm <- convert_units_in_rectangular_viewport(arc$x, arc$y)
    radius_x <- diff(range(arc_mm$x)) / 2
    radius_y <- diff(range(arc_mm$y))
    expect_equal(radius_x, radius_y, tolerance = 0.2)
    diameter <- find_named_grobs(p, "psychro-protractor-diameter")[[1L]]
    diameter_mm <- convert_units_in_rectangular_viewport(
        grid::unit.c(diameter$x0, diameter$x1),
        grid::unit.c(diameter$y0, diameter$y1)
    )
    expect_lt(diff(range(diameter_mm$x)), diff(range(arc_mm$x)) * 0.82)
    expect_gt(diff(range(diameter_mm$x)), diff(range(arc_mm$x)) * 0.74)

    p_mollier <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE) +
        geom_psychro_protractor(label = FALSE)
    expect_true(p_mollier$psychro$protractor$show)
    expect_false(p_mollier$psychro$protractor$label)
    expect_gt(count_named_grobs(p_mollier, "psychro-protractor"), 0L)
    expect_equal(count_named_grobs(p_mollier, "psychro-protractor-labels"), 0L)
    expect_equal(count_named_grobs(p_mollier, "psychro-protractor-titles"), 1L)
    mollier_titles <- find_named_grobs(p_mollier, "psychro-protractor-titles")[[1L]]
    expect_equal(mollier_titles$rot, c(-90, -90), tolerance = 1e-8)

    p_hidden <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_protractor(show = FALSE)
    expect_false(p_hidden$psychro$protractor$show)
    expect_equal(count_named_grobs(p_hidden, "psychro-protractor"), 0L)

    p_scaled <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_protractor(
            annotation = c("custom SHR", "custom ratio"),
            guide = guide_psychro_protractor(
                shr_breaks = c(0, 0.5, 1),
                shr_minor_breaks = c(0.25, 0.75),
                shr_labels = c("zero", "half", "one"),
                ratio_labels = NULL
            )
        )
    tick_labels <- find_named_grobs(p_scaled, "psychro-protractor-labels")[[1L]]
    expect_true("half" %in% as.character(tick_labels$label))
    expect_true(tick_labels$check.overlap)
    titles <- find_named_grobs(p_scaled, "psychro-protractor-titles")[[1L]]
    expect_equal(as.character(titles$label), c("custom SHR", "custom ratio"))

    p_default <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor()
    default_labels <- find_named_grobs(p_default, "psychro-protractor-labels")[[1L]]
    default_label_pos <- convert_units_in_rectangular_viewport(
        default_labels$x, default_labels$y
    )
    default_label_text <- as.character(default_labels$label)
    expect_true(all(c(
        "-5.0", "-2.0", "-1.0", "-0.5", "-0.3", "-0.2",
        "0", "0.2", "0.4", "0.6", "0.7", "0.8",
        "1.0", "1.5", "2.0", "4.0"
    ) %in% default_label_text))
    expect_true(any(abs(default_labels$rot) > 1))
    expect_true(any(abs(default_labels$rot) < 1))
    expect_true(all(c(
        "10.0", "5.0", "4.0", "3.0", "2.5", "2.0",
        "1.5", "1.0", "0", "-1.0", "-2.0", "-5.0"
    ) %in% default_label_text))
    expect_equal(default_label_text[seq_len(2L)], c("1.0", "1.0"))
    expect_true(all(c("+infinity", "-infinity") %in% default_label_text))
    expect_type(default_labels$label, "expression")
    infinity_loc <- match(c("+infinity", "-infinity"), default_label_text)
    expect_gt(
        min(default_labels$gp$fontsize[infinity_loc]),
        max(default_labels$gp$fontsize[-infinity_loc])
    )
    expect_equal(count_named_grobs(p_default, "psychro-protractor-end-caps"), 1L)
    expect_equal(count_named_grobs(p_default, "psychro-protractor-center-mark"), 1L)
    expect_gte(sum(default_label_text == "1.0"), 2L)
    loc_zero <- match("0", default_label_text)
    loc_two_tenths <- match("0.2", default_label_text)
    expect_gt(dist_euclid(
        default_label_pos$x[[loc_zero]], default_label_pos$y[[loc_zero]],
        default_label_pos$x[[loc_two_tenths]], default_label_pos$y[[loc_two_tenths]]
    ), 2)
    p_mollier_default <- ggpsychro(
        tdb_lim = c(0, 50), hum_lim = c(0, 30), mollier = TRUE
    ) + geom_psychro_protractor()
    mollier_labels <- find_named_grobs(p_mollier_default, "psychro-protractor-labels")[[1L]]
    expect_equal(mollier_labels$rot[seq_len(2L)], c(-90, 90), tolerance = 1e-8)

    p_base_scale <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(radius = 0.1, scale = 1, linewidth = 0.4, label.size = 2)
    p_scaled_scale <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(radius = 0.1, scale = 2, linewidth = 0.4, label.size = 2)
    expect_equal(
        protractor_arc_radius_mm(p_scaled_scale),
        protractor_arc_radius_mm(p_base_scale) * 2,
        tolerance = 0.2
    )
    expect_equal(
        find_named_grobs(p_scaled_scale, "psychro-protractor-arc")[[1L]]$gp$lwd,
        find_named_grobs(p_base_scale, "psychro-protractor-arc")[[1L]]$gp$lwd * 2,
        tolerance = 1e-8
    )
    expect_equal(
        find_named_grobs(p_scaled_scale, "psychro-protractor-labels")[[1L]]$gp$fontsize,
        find_named_grobs(p_base_scale, "psychro-protractor-labels")[[1L]]$gp$fontsize * 2,
        tolerance = 1e-8
    )
    p_half_radius <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(radius = 0.2, scale = 0.5)
    expect_equal(
        protractor_arc_radius_mm(p_half_radius),
        protractor_arc_radius_mm(p_base_scale),
        tolerance = 0.2
    )

    normal_center <- psychro_protractor_center(0.1, c(0.05, 0.12), mollier = FALSE)
    normal_center_mm <- convert_units_in_rectangular_viewport(
        normal_center$x, normal_center$y
    )
    normal_expected_mm <- convert_units_in_rectangular_viewport(
        grid::unit(0, "npc") + grid::unit(0.15, "snpc"),
        grid::unit(1, "npc") - grid::unit(0.12, "snpc")
    )
    expect_equal(normal_center_mm$x, normal_expected_mm$x, tolerance = 1e-8)
    expect_equal(normal_center_mm$y, normal_expected_mm$y, tolerance = 1e-8)

    mollier_center <- psychro_protractor_center(0.1, c(0.05, 0.12), mollier = TRUE)
    mollier_center_mm <- convert_units_in_rectangular_viewport(
        mollier_center$x, mollier_center$y
    )
    mollier_expected_mm <- convert_units_in_rectangular_viewport(
        grid::unit(1, "npc") - grid::unit(0.05, "snpc"),
        grid::unit(0, "npc") + grid::unit(0.22, "snpc")
    )
    expect_equal(mollier_center_mm$x, mollier_expected_mm$x, tolerance = 1e-8)
    expect_equal(mollier_center_mm$y, mollier_expected_mm$y, tolerance = 1e-8)

    p_no_annotation <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_protractor(annotation = FALSE)
    expect_equal(count_named_grobs(p_no_annotation, "psychro-protractor-titles"), 0L)

    expect_error(
        ggplot2::ggplot() + geom_psychro_protractor(),
        "only be added to a ggpsychro plot"
    )
    expect_error(
        geom_psychro_protractor(annotation = "invalid"),
        "`annotation`"
    )
    expect_error(
        geom_psychro_protractor(scale = 0),
        "scale"
    )
    expect_error(
        geom_psychro_protractor(scale = -1),
        "scale"
    )
    expect_error(
        geom_psychro_protractor(margin = c(0.1, 0.2, 0.3)),
        "margin"
    )
    expect_error(
        geom_psychro_protractor(margin = c(0.1, NA)),
        "margin"
    )
    expect_error(
        geom_psychro_protractor(guide = list()),
        "`guide`"
    )
    expect_error(
        guide_psychro_protractor(ratio_labels = c("a", "b")),
        "`ratio_labels` requires explicit `ratio_breaks`"
    )

    p_ratio_scaled <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(
            guide = guide_psychro_protractor(
                shr_labels = NULL,
                ratio_breaks = c(5, 0, -5),
                ratio_minor_breaks = NULL,
                ratio_labels = c("warm", "flat", "cool")
            )
        )
    ratio_scaled <- find_named_grobs(p_ratio_scaled, "psychro-protractor-labels")[[1L]]
    expect_true(all(c("warm", "flat", "cool") %in% as.character(ratio_scaled$label)))

    p_empty_guide <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(
            guide = guide_psychro_protractor(
                shr_breaks = NULL,
                shr_minor_breaks = NULL,
                shr_labels = NULL,
                ratio_breaks = NULL,
                ratio_minor_breaks = NULL,
                ratio_labels = NULL
            )
        )
    expect_no_error(ggplot2::ggplotGrob(p_empty_guide))
    expect_equal(count_named_grobs(p_empty_guide, "psychro-protractor-major-ticks"), 0L)

    p_expr <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(
            annotation = expression(A / B, C / D)
        )
    expr_titles <- find_named_grobs(p_expr, "psychro-protractor-titles")[[1L]]
    expect_type(expr_titles$label, "expression")

    vdiffr::expect_doppelganger("psychro protractor default", p_default)
    vdiffr::expect_doppelganger(
        "psychro protractor mollier",
        p_mollier_default
    )
    vdiffr::expect_doppelganger("psychro protractor custom guide", p_scaled)
    vdiffr::expect_doppelganger("psychro protractor scaled", p_scaled_scale)
})

test_that("Psychrometric protractor tick angles use transformed humidity ratios", {
    ticks <- psychro_protractor_shr_ticks(c(-1, 0, 0.5, 1, 2), c(0, 50), c(0, 0.03), "SI")
    expected <- pi + atan(1.006 / 2501 * 50 / 0.03)

    expect_equal(ticks$value, c(-1, 0, 0.5, 1, 2))
    expect_equal(ticks$angle[[3L]], expected, tolerance = 1e-10)
    expect_gt(ticks$angle[[3L]], pi)
    expect_lt(ticks$angle[[3L]], 3 * pi / 2)
    expect_equal(ticks$angle[[2L]], 3 * pi / 2)
    expect_equal(ticks$angle[[4L]], pi)
    expect_gt(ticks$angle[[1L]], 3 * pi / 2)
    expect_lt(ticks$angle[[1L]], 2 * pi)
    expect_gt(ticks$angle[[5L]], 3 * pi / 2)
    expect_lt(ticks$angle[[5L]], 2 * pi)

    endpoints <- psychro_protractor_add_sensible_endpoint(ticks[ticks$value == 1, ], 1)
    expect_equal(endpoints$angle, c(pi, 2 * pi))
    expect_equal(
        psychro_protractor_label_rotation(c(pi, 3 * pi / 2, 2 * pi)),
        c(0, 90, 0),
        tolerance = 1e-8
    )
    expect_equal(
        psychro_protractor_label_rotation(c(pi, 2 * pi), -pi / 2),
        c(-90, 90),
        tolerance = 1e-8
    )

    shr_spec <- psychro_protractor_label_spec(
        psychro_protractor_shr_major_breaks(), waiver(), "shr", "SI"
    )
    shr_label_ticks <- psychro_protractor_label_ticks(
        psychro_protractor_add_sensible_endpoint(
            psychro_protractor_shr_ticks(
                psychro_protractor_shr_major_breaks(), c(0, 50), c(0, 0.03), "SI"
            ),
            psychro_protractor_shr_major_breaks()
        ),
        shr_spec
    )
    shr_endpoint_labels <- abs(shr_label_ticks$value - 1) <= 1e-8 &
        (abs(shr_label_ticks$angle - pi) <= 1e-8 |
            abs(shr_label_ticks$angle - 2 * pi) <= 1e-8)
    expect_equal(shr_label_ticks$angle[shr_endpoint_labels], c(pi, 2 * pi))
    expect_true(all(abs(shr_label_ticks$scale - 0.86) < 1e-8))
    expect_true(all(shr_label_ticks$anchor == "center"))

    ratio_ticks <- psychro_protractor_ratio_ticks(c(10, 2501 / 1000, 0, -10), c(0, 50), c(0, 0.03), "SI")
    expect_gt(ratio_ticks$angle[[1L]], pi)
    expect_lt(ratio_ticks$angle[[1L]], 3 * pi / 2)
    expect_equal(ratio_ticks$angle[[2L]], 3 * pi / 2)
    expect_gt(ratio_ticks$angle[[3L]], 3 * pi / 2)
    expect_gt(ratio_ticks$angle[[4L]], ratio_ticks$angle[[3L]])
    expect_lt(ratio_ticks$angle[[4L]], 2 * pi)
    ratio_spec <- psychro_protractor_label_spec(
        psychro_protractor_ratio_major_breaks("SI"), waiver(), "ratio", "SI"
    )
    ratio_label_ticks <- psychro_protractor_ratio_label_ticks(
        psychro_protractor_ratio_ticks(
            psychro_protractor_ratio_major_breaks("SI"), c(0, 50), c(0, 0.03), "SI"
        ),
        ratio_spec
    )
    expect_true(all(abs(ratio_label_ticks$scale - 1.13) < 1e-8))

    raw_ticks <- new_data_frame(list(
        axis = c("shr", "ratio"),
        value = c(0, 0),
        angle = c(4, 5)
    ))
    major_layout <- psychro_protractor_unique_ticks(raw_ticks, major = TRUE)
    minor_layout <- psychro_protractor_unique_ticks(raw_ticks, major = FALSE)
    major_shr <- major_layout[major_layout$axis == "shr", ]
    major_ratio <- major_layout[major_layout$axis == "ratio", ]
    minor_shr <- minor_layout[minor_layout$axis == "shr", ]
    minor_ratio <- minor_layout[minor_layout$axis == "ratio", ]
    expect_gt(1 - major_shr$inner, major_shr$outer - 1)
    expect_gt(major_ratio$outer - 1, 1 - major_ratio$inner)
    expect_gt(1 - major_shr$inner, 1 - minor_shr$inner)
    expect_gt(major_ratio$outer - 1, minor_ratio$outer - 1)

    ip_latent <- 1061 / 7000
    expect_equal(psychro_protractor_ratio_divisor("IP"), 7000)
    ip_ratio_ticks <- psychro_protractor_ratio_ticks(ip_latent, c(32, 122), c(0, 350), "IP")
    expect_equal(ip_ratio_ticks$value, ip_latent)
    expect_equal(ip_ratio_ticks$angle, 3 * pi / 2)

    expect_false("scale_shr_continuous" %in% getNamespaceExports("ggpsychro"))
    expect_false("shr_trans" %in% getNamespaceExports("ggpsychro"))
    expect_true("guide_psychro_protractor" %in% getNamespaceExports("ggpsychro"))
})

test_that("Psychrometric charts build with common ggplot features", {
    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(10, 30), hum_lim = c(10, 20), units = "IP", altitude = -10) +
                geom_grid_relhum() +
                geom_grid_wetbulb()
        )
    )

    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE) +
                geom_grid_enthalpy()
        )
    )

    d <- data.frame(x = 20:25, y = 5:10)
    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(d, ggplot2::aes(x, y), tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_relhum() +
                ggplot2::geom_point() +
                ggplot2::facet_wrap(~ y > 7)
            )
    )

    expect_no_error(
        ggplot2::ggplotGrob(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_relhum(label = FALSE) +
                geom_grid_wetbulb(label_loc = NA) +
                geom_grid_specvol() +
                scale_specvol_continuous(labels = NULL)
        )
    )
})

test_that("Psychrometric grid labels are rendered only for explicit helpers", {
    expect_equal(
        count_textpath_shapes(ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50))),
        0L
    )
    expect_gt(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_relhum()
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_relhum(label = FALSE)
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_relhum(label_loc = NA)
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_specvol() +
                scale_specvol_continuous(labels = NULL)
        ),
        0L
    )

    expect_gt(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_grid_relhum() +
                geom_grid_wetbulb() +
                geom_grid_vappres() +
                geom_grid_specvol() +
                geom_grid_enthalpy()
        ),
        1L
    )

    # Text-on-path glyph positions depend on platform font metrics. Keep these
    # visual snapshots on macOS, while the behavior checks above run everywhere.
    testthat::skip_on_os(c("linux", "windows"))

    vdiffr::expect_doppelganger(
        "relative humidity grid labels",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_grid_relhum()
    )

    vdiffr::expect_doppelganger(
        "combined grid labels",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_grid_relhum() +
            geom_grid_wetbulb() +
            geom_grid_vappres() +
            geom_grid_specvol() +
            geom_grid_enthalpy()
    )

    vdiffr::expect_doppelganger(
        "mollier grid labels",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE) +
            geom_grid_enthalpy()
    )
})

test_that("Psychrometric presets configure themes and grids", {
    expect_s3_class(theme_psychro_ashrae(), "theme")
    expect_s3_class(theme_psychro_minimal(), "theme")
    expect_s3_class(theme_psychro_ashrae()$panel.border, "element_rect")
    expect_equal(
        ggplot2::calc_element("axis.title.y", theme_psychro_minimal())$angle,
        90
    )

    expect_error(psychro_preset("invalid"), "arg")
    expect_error(psychro_preset("ashrae", labels = NA))

    p_ashrae <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        psychro_preset("ashrae")
    expect_true(p_ashrae$psychro$grids$relhum)
    expect_true(p_ashrae$psychro$grids$wetbulb)
    expect_false(p_ashrae$psychro$grids$vappres)
    expect_true(p_ashrae$psychro$grids$specvol)
    expect_true(p_ashrae$psychro$grids$enthalpy)
    expect_true(p_ashrae$psychro$grid_labels$relhum$show)
    expect_true(p_ashrae$psychro$grid_labels$wetbulb$show)
    expect_true(p_ashrae$psychro$grid_labels$specvol$show)
    expect_true(p_ashrae$psychro$grid_labels$enthalpy$show)
    expect_no_error(built_ashrae <- ggplot2::ggplot_build(p_ashrae))
    panel_ashrae <- built_ashrae$layout$panel_params[[1L]]
    expect_equal(remove_na(panel_ashrae$x$get_breaks()), seq(0, 50, by = 5))
    expect_equal(remove_na(panel_ashrae$x$get_breaks_minor()), seq(0, 50, by = 1))
    expect_equal(
        remove_na(panel_ashrae$y$get_breaks()),
        seq(0, 0.03, by = 0.005),
        tolerance = 1e-8
    )
    expect_equal(
        remove_na(panel_ashrae$y$get_breaks_minor()),
        seq(0, 0.03, by = 0.0005),
        tolerance = 1e-8
    )
    expect_equal(
        remove_na(panel_ashrae$relhum$get_breaks()),
        seq(0.1, 0.9, by = 0.1),
        tolerance = 1e-8
    )
    expect_length(remove_na(panel_ashrae$relhum$get_breaks_minor()), 0L)
    expect_equal(remove_na(panel_ashrae$wetbulb$get_breaks()), seq(0, 30, by = 5))
    expect_equal(
        remove_na(panel_ashrae$specvol$get_breaks()),
        seq(0.80, 0.95, by = 0.05),
        tolerance = 1e-8
    )
    expect_equal(
        remove_na(panel_ashrae$specvol$get_breaks_minor()),
        seq(0.78, 0.96, by = 0.01),
        tolerance = 1e-8
    )
    expect_equal(
        remove_na(panel_ashrae$enthalpy$get_breaks()),
        c(50000, 100000)
    )
    expect_equal(
        remove_na(panel_ashrae$enthalpy$get_breaks_minor()),
        seq(10000, 130000, by = 10000)
    )
    expect_gt(count_textpath_shapes(p_ashrae), 1L)

    p_minimal <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        psychro_preset("minimal")
    expect_true(p_minimal$psychro$grids$relhum)
    expect_true(p_minimal$psychro$grids$wetbulb)
    expect_false(p_minimal$psychro$grids$vappres)
    expect_true(p_minimal$psychro$grids$specvol)
    expect_true(p_minimal$psychro$grids$enthalpy)
    expect_false(p_minimal$psychro$grid_labels$relhum$show)
    expect_true(p_minimal$psychro$grid_labels$wetbulb$show)
    expect_false(p_minimal$psychro$grid_labels$specvol$show)
    expect_false(p_minimal$psychro$grid_labels$enthalpy$show)
    expect_no_error(built_minimal <- ggplot2::ggplot_build(p_minimal))
    panel_minimal <- built_minimal$layout$panel_params[[1L]]
    expect_equal(remove_na(panel_minimal$x$get_breaks()), seq(0, 50, by = 5))
    expect_length(remove_na(panel_minimal$x$get_breaks_minor()), 0L)
    expect_equal(
        remove_na(panel_minimal$relhum$get_breaks()),
        seq(0.2, 0.8, by = 0.2),
        tolerance = 1e-8
    )
    expect_length(remove_na(panel_minimal$relhum$get_breaks_minor()), 0L)
    expect_equal(remove_na(panel_minimal$wetbulb$get_breaks()), seq(10, 30, by = 10))
    expect_true(all(seq(5, 35, by = 5) %in% remove_na(panel_minimal$wetbulb$get_breaks_minor())))
    expect_equal(
        remove_na(panel_minimal$specvol$get_breaks()),
        seq(0.86, 0.98, by = 0.04),
        tolerance = 1e-8
    )
    expect_equal(
        remove_na(panel_minimal$enthalpy$get_breaks()),
        c(20000, 60000, 100000)
    )
    expect_gt(count_textpath_shapes(p_minimal), 0L)

    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                psychro_preset("ashrae", labels = FALSE)
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                psychro_preset("minimal", labels = FALSE)
        ),
        0L
    )

    testthat::skip_on_os(c("linux", "windows"))

    vdiffr::expect_doppelganger(
        "ashrae preset",
        p_ashrae
    )

    vdiffr::expect_doppelganger(
        "minimal preset",
        p_minimal
    )
})

test_that("Psychrometric stats inherit units and pressure from the plot", {
    d <- data.frame(
        dry_bulb_temperature = seq(10, 35, length.out = 10),
        relative_humidity = seq(30, 90, length.out = 10)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            ggplot2::geom_point(
                ggplot2::aes(dry_bulb_temperature, relhum = relative_humidity),
                stat = "relhum"
            )
    )
    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))
    expected <- with_units(
        "SI",
        psychrolib::GetHumRatioFromRelHum(
            d$dry_bulb_temperature,
            d$relative_humidity / 100,
            pressure
        )
    )

    expect_equal(built$data[[1L]]$y, expected, tolerance = 1e-8)
    expect_gt(max(built$data[[1L]]$y), 0.01)
})

test_that("building ggpsychro plots does not mutate source plot state", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_tile(
            ggplot2::aes(x, y),
            data = data.frame(x = 20, y = 10)
        )

    # Guard against build-time psychro metadata leaking back onto the user-held plot.
    stat_param_names <- names(p@layers[[1L]]$stat_params)
    expect_null(p@layers[[1L]]$geom_params$psychro.theme)
    expect_null(p@coordinates$pressure)

    invisible(ggplot2::ggplot_build(p))

    expect_equal(names(p@layers[[1L]]$stat_params), stat_param_names)
    expect_false(any(c("units", "pres", "mollier", "tdb_lim", "hum_lim") %in%
        names(p@layers[[1L]]$stat_params)))
    expect_null(p@coordinates$pressure)

    invisible(ggplot2::ggplotGrob(p))

    expect_equal(names(p@layers[[1L]]$stat_params), stat_param_names)
    expect_null(p@layers[[1L]]$geom_params$psychro.theme)
    expect_null(p@coordinates$pressure)
})

test_that("rebuilt plots do not reuse stale inherited psychro params", {
    d <- data.frame(tdb = 77, relhum = 50)
    p <- ggpsychro(d, tdb_lim = c(50, 100), hum_lim = c(0, 60)) +
        stat_psychro_state(ggplot2::aes(tdb = tdb, relhum = relhum))

    invisible(ggplot2::ggplot_build(p))
    suppressMessages(
        rebuilt <- p + coord_psychro(
            tdb_lim = c(50, 100), hum_lim = c(0, 140), units = "IP"
        )
    )
    fresh <- ggpsychro(
        d, tdb_lim = c(50, 100), hum_lim = c(0, 140), units = "IP"
    ) +
        stat_psychro_state(ggplot2::aes(tdb = tdb, relhum = relhum))

    rebuilt_data <- ggplot2::ggplot_build(rebuilt)$data[[1L]]
    fresh_data <- ggplot2::ggplot_build(fresh)$data[[1L]]

    expect_null(p@layers[[1L]]$stat_params$units)
    expect_equal(rebuilt_data$y, fresh_data$y, tolerance = 1e-8)
})

test_that("Enthalpy stat creates y output without an explicit y aesthetic", {
    d <- data.frame(
        dry_bulb_temperature = c(18, 24, 30),
        enthalpy = c(35000, 50000, 65000)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
            stat_enthalpy(
                ggplot2::aes(x = dry_bulb_temperature, enthalpy = enthalpy),
                data = d
            )
    )
    expected <- with_units(
        "SI",
        GetHumRatioFromEnthalpyAndTDryBulb(d$enthalpy, d$dry_bulb_temperature)
    )

    expect_equal(built$data[[1L]]$y, expected, tolerance = 1e-8)
    expect_true(all(is.finite(built$data[[1L]]$y)))
})
