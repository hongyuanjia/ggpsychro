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
            grids = default_psychro_grids()
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
        ggpsychro(tdb_lim = c(10, 30), hum_lim = c(10, 20), units = "IP", altitude = -10)
    )

    vdiffr::expect_doppelganger(
        "basic mollier chart in SI units",
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE)
    )
})

test_that("Psychrometric grid helpers update coord metadata", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_grid_relhum(color = "red") +
        geom_grid_enthalpy()

    expect_length(p$layers, 0L)
    expect_true(p$psychro$grids$relhum)
    expect_true(p$psychro$grids$enthalpy)
    expect_true(p$coordinates$grids$relhum)
    expect_true(p$coordinates$grids$enthalpy)
    expect_no_error(ggplot2::ggplot_build(p))

    p <- p + geom_grid_relhum(show = FALSE)
    expect_false(p$psychro$grids$relhum)
    expect_false(p$coordinates$grids$relhum)
    expect_no_error(ggplot2::ggplot_build(p))
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
})

test_that("Psychrometric stats inherit units and pressure from the plot", {
    d <- data.frame(
        dry_bulb_temperature = seq(10, 35, length.out = 10),
        relative_humidity = seq(0.3, 0.9, length.out = 10)
    )

    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(d, tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
                ggplot2::geom_point(
                    ggplot2::aes(dry_bulb_temperature, relhum = relative_humidity),
                    stat = "relhum"
                )
        )
    )
})
