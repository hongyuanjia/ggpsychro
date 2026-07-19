# Focused psychrometric chart, coordinate, guide, and stat tests.

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
            grids = grid__defaults(),
            grid_labels = list(),
            protractor = psychro__default_protractor()
        )
    )
    expect_s3_class(p$coordinates, "CoordPsychro")
    expect_type(p$labels$x, "expression")
    expect_type(p$labels$y, "expression")
    expect_true(is_ggpsychro(p))

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
test_that("Unit labels and IP humidity limits match display units", {
    expect_equal(psychro__hum_limits("IP"), c(0, 420))
    expect_no_error(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(32, 122), hum_lim = c(0, 420), units = "IP")
    ))

    expect_equal(
        label_vappres(units = "SI")(c(1000, 2000)),
        c("Vappres 1 kPa", "2 kPa")
    )
    expect_equal(
        label_vappres(units = "IP")(c(0.1, 0.2)),
        c("Vappres 0.1 psi", "0.2 psi")
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
test_that("Psychrometric panel backgrounds keep ggplot and psychro semantics", {
    default_grobs <- collect_grobs(ggplot2::ggplotGrob(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50))
    ))
    default_name <- vapply(
        default_grobs,
        function(grob) {
            grob$name %||% ""
        },
        character(1)
    )
    expect_false(any(grepl("psychro[.-]panel[.-]mask", default_name)))

    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(
                fill = "#F0F0A0",
                colour = NA
            ),
            panel.background = ggplot2::element_rect(
                fill = "#F0A0A0",
                colour = NA
            ),
            psychro.panel.background = element_polygon(
                fill = "#A0F0A0",
                color = NA
            ),
            psychro.panel.mask = element_polygon(fill = "#A0A0F0", color = NA),
            plot.margin = ggplot2::margin(6, 6, 6, 6)
        )
    grobs <- collect_grobs(ggplot2::ggplotGrob(p))
    fill <- vapply(
        grobs,
        function(grob) {
            fill <- grob$gp$fill
            if (is.null(fill) || !length(fill)) {
                return(NA_character_)
            }
            as.character(fill)[[1L]]
        },
        character(1)
    )
    name <- vapply(
        grobs,
        function(grob) {
            grob$name %||% ""
        },
        character(1)
    )

    expect_true(any(fill == "#F0F0A0", na.rm = TRUE))
    expect_true(any(grepl("panel.background", name) & fill == "#F0A0A0"))
    expect_true(any(
        grepl("psychro[.-]panel[.-]background", name) & fill == "#A0F0A0"
    ))
    expect_true(any(
        grepl("psychro[.-]panel[.-]mask", name) & fill == "#A0A0F0"
    ))
})
test_that("Psychrometric charts build with common ggplot features", {
    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(
                tdb_lim = c(10, 30),
                hum_lim = c(10, 20),
                units = "IP",
                altitude = -10
            ) +
                geom_psychro_grid_relhum() +
                geom_psychro_grid_wetbulb()
        )
    )

    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE) +
                geom_psychro_grid_enthalpy()
        )
    )

    d <- data.frame(x = 20:25, y = 5:10)
    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(
                d,
                ggplot2::aes(x, y),
                tdb_lim = c(0, 50),
                hum_lim = c(0, 50)
            ) +
                geom_psychro_grid_relhum() +
                ggplot2::geom_point() +
                ggplot2::facet_wrap(~ y > 7)
        )
    )

    expect_no_error(
        ggplot2::ggplotGrob(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_relhum(label = FALSE) +
                geom_psychro_grid_wetbulb(label_loc = NA) +
                geom_psychro_grid_specvol() +
                scale_specvol_continuous(labels = NULL)
        )
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
    expect_equal(
        util__remove_na(panel_ashrae$x$get_breaks()),
        seq(0, 50, by = 5)
    )
    expect_equal(
        util__remove_na(panel_ashrae$x$get_breaks_minor()),
        seq(0, 50, by = 1)
    )
    expect_equal(
        util__remove_na(panel_ashrae$y$get_breaks()),
        seq(0, 0.03, by = 0.005),
        tolerance = 1e-8
    )
    expect_equal(
        util__remove_na(panel_ashrae$y$get_breaks_minor()),
        seq(0, 0.03, by = 0.0005),
        tolerance = 1e-8
    )
    expect_equal(
        util__remove_na(panel_ashrae$relhum$get_breaks()),
        seq(0.1, 0.9, by = 0.1),
        tolerance = 1e-8
    )
    expect_length(util__remove_na(panel_ashrae$relhum$get_breaks_minor()), 0L)
    expect_equal(
        util__remove_na(panel_ashrae$wetbulb$get_breaks()),
        seq(0, 30, by = 5)
    )
    expect_equal(
        util__remove_na(panel_ashrae$specvol$get_breaks()),
        seq(0.80, 0.95, by = 0.05),
        tolerance = 1e-8
    )
    expect_equal(
        util__remove_na(panel_ashrae$specvol$get_breaks_minor()),
        seq(0.78, 0.96, by = 0.01),
        tolerance = 1e-8
    )
    expect_equal(
        util__remove_na(panel_ashrae$enthalpy$get_breaks()),
        c(50000, 100000)
    )
    expect_equal(
        util__remove_na(panel_ashrae$enthalpy$get_breaks_minor()),
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
    expect_equal(
        util__remove_na(panel_minimal$x$get_breaks()),
        seq(0, 50, by = 5)
    )
    expect_length(util__remove_na(panel_minimal$x$get_breaks_minor()), 0L)
    expect_equal(
        util__remove_na(panel_minimal$relhum$get_breaks()),
        seq(0.2, 0.8, by = 0.2),
        tolerance = 1e-8
    )
    expect_length(util__remove_na(panel_minimal$relhum$get_breaks_minor()), 0L)
    expect_equal(
        util__remove_na(panel_minimal$wetbulb$get_breaks()),
        seq(10, 30, by = 10)
    )
    expect_true(all(
        seq(5, 35, by = 5) %in%
            util__remove_na(panel_minimal$wetbulb$get_breaks_minor())
    ))
    expect_equal(
        util__remove_na(panel_minimal$specvol$get_breaks()),
        seq(0.86, 0.98, by = 0.04),
        tolerance = 1e-8
    )
    expect_equal(
        util__remove_na(panel_minimal$enthalpy$get_breaks()),
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
