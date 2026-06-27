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
            grid_labels = list()
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
        seq(0, 0.03, by = 0.001),
        tolerance = 1e-8
    )
    expect_equal(
        remove_na(panel_ashrae$relhum$get_breaks()),
        seq(0.1, 1, by = 0.1),
        tolerance = 1e-8
    )
    expect_gt(
        length(remove_na(panel_ashrae$wetbulb$get_breaks_minor())),
        length(remove_na(panel_ashrae$wetbulb$get_breaks()))
    )
    expect_gt(count_textpath_shapes(p_ashrae), 1L)

    p_minimal <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        psychro_preset("minimal")
    expect_true(p_minimal$psychro$grids$relhum)
    expect_true(p_minimal$psychro$grids$wetbulb)
    expect_false(p_minimal$psychro$grids$vappres)
    expect_false(p_minimal$psychro$grids$specvol)
    expect_false(p_minimal$psychro$grids$enthalpy)
    expect_true(p_minimal$psychro$grid_labels$relhum$show)
    expect_false(p_minimal$psychro$grid_labels$wetbulb$show)
    expect_no_error(ggplot2::ggplot_build(p_minimal))
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
