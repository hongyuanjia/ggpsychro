# Focused psychrometric chart, coordinate, guide, and stat tests.

test_that("Relative humidity grid breaks use psychrolib fractions", {
    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_psychro_grid_relhum()
    )
    breaks <- util__remove_na(built$layout$panel_params[[
        1L
    ]]$relhum$get_breaks())
    expect_equal(breaks, c(0.25, 0.50, 0.75, 1.00), tolerance = 1e-8)

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_psychro_grid_relhum() +
            scale_relhum_continuous(
                limits = c(25, 100),
                breaks = seq(25, 100, by = 25),
                minor_breaks = NULL
            )
    )
    breaks <- util__remove_na(built$layout$panel_params[[
        1L
    ]]$relhum$get_breaks())
    expect_equal(breaks, c(0.25, 0.50, 0.75, 1.00), tolerance = 1e-8)

    expect_equal(
        coord_psy__relhum_grid_breaks(c(NA, 0, 0.25, 0.50, 1.00, 1.25)),
        c(0.25, 0.50)
    )
})
test_that("Psychrometric grid helpers update coord metadata", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_grid_relhum(color = "red", label.size = 5) +
        geom_psychro_grid_enthalpy()

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

    p <- p + geom_psychro_grid_relhum(show = FALSE)
    expect_false(p$psychro$grids$relhum)
    expect_false(p$psychro$grid_labels$relhum$show)
    expect_false(p$coordinates$grids$relhum)
    expect_no_error(ggplot2::ggplot_build(p))

    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_grid_relhum(label.size = 5) +
        geom_psychro_grid_relhum(label.size = 2, label.vjust = 0.8)
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

    p_mollier <- ggpsychro(
        tdb_lim = c(0, 50),
        hum_lim = c(0, 50),
        mollier = TRUE
    ) +
        geom_psychro_protractor(label = FALSE)
    expect_true(p_mollier$psychro$protractor$show)
    expect_false(p_mollier$psychro$protractor$label)
    expect_gt(count_named_grobs(p_mollier, "psychro-protractor"), 0L)
    expect_equal(count_named_grobs(p_mollier, "psychro-protractor-labels"), 0L)
    expect_equal(count_named_grobs(p_mollier, "psychro-protractor-titles"), 1L)
    mollier_titles <- find_named_grobs(p_mollier, "psychro-protractor-titles")[[
        1L
    ]]
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
    default_labels <- find_named_grobs(p_default, "psychro-protractor-labels")[[
        1L
    ]]
    default_label_pos <- convert_units_in_rectangular_viewport(
        default_labels$x,
        default_labels$y
    )
    default_label_text <- as.character(default_labels$label)
    expect_true(all(
        c(
            "-5.0",
            "-2.0",
            "-1.0",
            "-0.5",
            "-0.3",
            "-0.2",
            "0",
            "0.2",
            "0.4",
            "0.6",
            "0.7",
            "0.8",
            "1.0",
            "1.5",
            "2.0",
            "4.0"
        ) %in%
            default_label_text
    ))
    expect_true(any(abs(default_labels$rot) > 1))
    expect_true(any(abs(default_labels$rot) < 1))
    expect_true(all(
        c(
            "10.0",
            "5.0",
            "4.0",
            "3.0",
            "2.5",
            "2.0",
            "1.5",
            "1.0",
            "0",
            "-1.0",
            "-2.0",
            "-5.0"
        ) %in%
            default_label_text
    ))
    expect_equal(default_label_text[seq_len(2L)], c("1.0", "1.0"))
    expect_true(all(c("+infinity", "-infinity") %in% default_label_text))
    expect_type(default_labels$label, "expression")
    infinity_loc <- match(c("+infinity", "-infinity"), default_label_text)
    expect_gt(
        min(default_labels$gp$fontsize[infinity_loc]),
        max(default_labels$gp$fontsize[-infinity_loc])
    )
    expect_equal(
        count_named_grobs(p_default, "psychro-protractor-end-caps"),
        1L
    )
    expect_equal(
        count_named_grobs(p_default, "psychro-protractor-center-mark"),
        1L
    )
    expect_gte(sum(default_label_text == "1.0"), 2L)
    loc_zero <- match("0", default_label_text)
    loc_two_tenths <- match("0.2", default_label_text)
    expect_gt(
        sqrt(
            (default_label_pos$x[[loc_two_tenths]] -
                default_label_pos$x[[loc_zero]])^2 +
                (default_label_pos$y[[loc_two_tenths]] -
                    default_label_pos$y[[loc_zero]])^2
        ),
        2
    )
    p_mollier_default <- ggpsychro(
        tdb_lim = c(0, 50),
        hum_lim = c(0, 30),
        mollier = TRUE
    ) +
        geom_psychro_protractor()
    mollier_labels <- find_named_grobs(
        p_mollier_default,
        "psychro-protractor-labels"
    )[[1L]]
    expect_equal(mollier_labels$rot[seq_len(2L)], c(-90, 90), tolerance = 1e-8)

    p_base_scale <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(
            radius = 0.1,
            scale = 1,
            linewidth = 0.4,
            label.size = 2
        )
    p_scaled_scale <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(
            radius = 0.1,
            scale = 2,
            linewidth = 0.4,
            label.size = 2
        )
    expect_equal(
        protractor_arc_radius_mm(p_scaled_scale),
        protractor_arc_radius_mm(p_base_scale) * 2,
        tolerance = 0.2
    )
    expect_equal(
        find_named_grobs(p_scaled_scale, "psychro-protractor-arc")[[1L]]$gp$lwd,
        find_named_grobs(p_base_scale, "psychro-protractor-arc")[[1L]]$gp$lwd *
            2,
        tolerance = 1e-8
    )
    expect_equal(
        find_named_grobs(p_scaled_scale, "psychro-protractor-labels")[[
            1L
        ]]$gp$fontsize,
        find_named_grobs(p_base_scale, "psychro-protractor-labels")[[
            1L
        ]]$gp$fontsize *
            2,
        tolerance = 1e-8
    )
    p_half_radius <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_protractor(radius = 0.2, scale = 0.5)
    expect_equal(
        protractor_arc_radius_mm(p_half_radius),
        protractor_arc_radius_mm(p_base_scale),
        tolerance = 0.2
    )

    normal_center <- protractor__center(
        0.1,
        c(0.05, 0.12),
        mollier = FALSE
    )
    normal_center_mm <- convert_units_in_rectangular_viewport(
        normal_center$x,
        normal_center$y
    )
    normal_expected_mm <- convert_units_in_rectangular_viewport(
        grid::unit(0, "npc") + grid::unit(0.15, "snpc"),
        grid::unit(1, "npc") - grid::unit(0.12, "snpc")
    )
    expect_equal(normal_center_mm$x, normal_expected_mm$x, tolerance = 1e-8)
    expect_equal(normal_center_mm$y, normal_expected_mm$y, tolerance = 1e-8)

    mollier_center <- protractor__center(
        0.1,
        c(0.05, 0.12),
        mollier = TRUE
    )
    mollier_center_mm <- convert_units_in_rectangular_viewport(
        mollier_center$x,
        mollier_center$y
    )
    mollier_expected_mm <- convert_units_in_rectangular_viewport(
        grid::unit(1, "npc") - grid::unit(0.05, "snpc"),
        grid::unit(0, "npc") + grid::unit(0.22, "snpc")
    )
    expect_equal(mollier_center_mm$x, mollier_expected_mm$x, tolerance = 1e-8)
    expect_equal(mollier_center_mm$y, mollier_expected_mm$y, tolerance = 1e-8)

    p_no_annotation <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_protractor(annotation = FALSE)
    expect_equal(
        count_named_grobs(p_no_annotation, "psychro-protractor-titles"),
        0L
    )

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
    ratio_scaled <- find_named_grobs(
        p_ratio_scaled,
        "psychro-protractor-labels"
    )[[1L]]
    expect_true(all(
        c("warm", "flat", "cool") %in% as.character(ratio_scaled$label)
    ))

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
    expect_equal(
        count_named_grobs(p_empty_guide, "psychro-protractor-major-ticks"),
        0L
    )

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
    ticks <- protractor__shr_ticks(
        c(-1, 0, 0.5, 1, 2),
        c(0, 50),
        c(0, 0.03),
        "SI"
    )
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

    endpoints <- protractor__add_sensible_endpoint(
        ticks[ticks$value == 1, ],
        1
    )
    expect_equal(endpoints$angle, c(pi, 2 * pi))
    expect_equal(
        protractor__label_rotation(c(pi, 3 * pi / 2, 2 * pi)),
        c(0, 90, 0),
        tolerance = 1e-8
    )
    expect_equal(
        protractor__label_rotation(c(pi, 2 * pi), -pi / 2),
        c(-90, 90),
        tolerance = 1e-8
    )

    shr_spec <- protractor__label_spec(
        protractor__shr_major_breaks(),
        waiver(),
        "shr",
        "SI"
    )
    shr_label_ticks <- protractor__label_ticks(
        protractor__add_sensible_endpoint(
            protractor__shr_ticks(
                protractor__shr_major_breaks(),
                c(0, 50),
                c(0, 0.03),
                "SI"
            ),
            protractor__shr_major_breaks()
        ),
        shr_spec
    )
    shr_endpoint_labels <- abs(shr_label_ticks$value - 1) <= 1e-8 &
        (abs(shr_label_ticks$angle - pi) <= 1e-8 |
            abs(shr_label_ticks$angle - 2 * pi) <= 1e-8)
    expect_equal(shr_label_ticks$angle[shr_endpoint_labels], c(pi, 2 * pi))
    expect_true(all(abs(shr_label_ticks$scale - 0.86) < 1e-8))
    expect_true(all(shr_label_ticks$anchor == "center"))

    ratio_ticks <- protractor__ratio_ticks(
        c(10, 2501 / 1000, 0, -10),
        c(0, 50),
        c(0, 0.03),
        "SI"
    )
    expect_gt(ratio_ticks$angle[[1L]], pi)
    expect_lt(ratio_ticks$angle[[1L]], 3 * pi / 2)
    expect_equal(ratio_ticks$angle[[2L]], 3 * pi / 2)
    expect_gt(ratio_ticks$angle[[3L]], 3 * pi / 2)
    expect_gt(ratio_ticks$angle[[4L]], ratio_ticks$angle[[3L]])
    expect_lt(ratio_ticks$angle[[4L]], 2 * pi)
    ratio_spec <- protractor__label_spec(
        protractor__ratio_major_breaks("SI"),
        waiver(),
        "ratio",
        "SI"
    )
    ratio_label_ticks <- protractor__ratio_label_ticks(
        protractor__ratio_ticks(
            protractor__ratio_major_breaks("SI"),
            c(0, 50),
            c(0, 0.03),
            "SI"
        ),
        ratio_spec
    )
    expect_true(all(abs(ratio_label_ticks$scale - 1.13) < 1e-8))

    raw_ticks <- util__new_data_frame(list(
        axis = c("shr", "ratio"),
        value = c(0, 0),
        angle = c(4, 5)
    ))
    major_layout <- protractor__unique_ticks(raw_ticks, major = TRUE)
    minor_layout <- protractor__unique_ticks(raw_ticks, major = FALSE)
    major_shr <- major_layout[major_layout$axis == "shr", ]
    major_ratio <- major_layout[major_layout$axis == "ratio", ]
    minor_shr <- minor_layout[minor_layout$axis == "shr", ]
    minor_ratio <- minor_layout[minor_layout$axis == "ratio", ]
    expect_gt(1 - major_shr$inner, major_shr$outer - 1)
    expect_gt(major_ratio$outer - 1, 1 - major_ratio$inner)
    expect_gt(1 - major_shr$inner, 1 - minor_shr$inner)
    expect_gt(major_ratio$outer - 1, minor_ratio$outer - 1)

    ip_latent <- 1061 / 7000
    expect_equal(protractor__ratio_divisor("IP"), 7000)
    ip_ratio_ticks <- protractor__ratio_ticks(
        ip_latent,
        c(32, 122),
        c(0, 350),
        "IP"
    )
    expect_equal(ip_ratio_ticks$value, ip_latent)
    expect_equal(ip_ratio_ticks$angle, 3 * pi / 2)

    expect_false("scale_shr_continuous" %in% getNamespaceExports("ggpsychro"))
    expect_false("shr_trans" %in% getNamespaceExports("ggpsychro"))
    expect_true(
        "guide_psychro_protractor" %in% getNamespaceExports("ggpsychro")
    )
})
test_that("Psychrometric grid labels are rendered only for explicit helpers", {
    expect_equal(
        count_textpath_shapes(ggpsychro(
            tdb_lim = c(0, 50),
            hum_lim = c(0, 50)
        )),
        0L
    )
    expect_gt(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_relhum()
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_relhum(label = FALSE)
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_relhum(label_loc = NA)
        ),
        0L
    )
    expect_equal(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_specvol() +
                scale_specvol_continuous(labels = NULL)
        ),
        0L
    )

    expect_gt(
        count_textpath_shapes(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_relhum() +
                geom_psychro_grid_wetbulb() +
                geom_psychro_grid_vappres() +
                geom_psychro_grid_specvol() +
                geom_psychro_grid_enthalpy()
        ),
        1L
    )

    fast_labels <- find_named_grobs(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_psychro_grid_relhum(),
        "psychro-grid-label-relhum"
    )
    expect_true(any(vapply(
        fast_labels,
        inherits,
        logical(1L),
        "psychro_textpath"
    )))
    expect_true(textpath__supported("label", vjust = 0.5))

    # Text-on-path glyph positions depend on platform font metrics. Keep these
    # visual snapshots on macOS, while the behavior checks above run everywhere.
    testthat::skip_on_os(c("linux", "windows"))

    vdiffr::expect_doppelganger(
        "relative humidity grid labels",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_psychro_grid_relhum()
    )

    vdiffr::expect_doppelganger(
        "combined grid labels",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
            geom_psychro_grid_relhum() +
            geom_psychro_grid_wetbulb() +
            geom_psychro_grid_vappres() +
            geom_psychro_grid_specvol() +
            geom_psychro_grid_enthalpy()
    )

    vdiffr::expect_doppelganger(
        "mollier grid labels",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE) +
            geom_psychro_grid_enthalpy()
    )
})
