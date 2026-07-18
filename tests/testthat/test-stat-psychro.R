# Focused psychrometric chart, coordinate, guide, and stat tests.

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
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )
    expected <- psychrolib__with_units(
        "SI",
        psychrolib::GetHumRatioFromRelHum(
            d$dry_bulb_temperature,
            d$relative_humidity / 100,
            pressure
        )
    )

    expect_equal(first_built_data(built)$y, expected, tolerance = 1e-8)
    expect_gt(max(first_built_data(built)$y), 0.01)
})
test_that("Psychrometric stats retain ordinary aesthetics after transformation", {
    d <- data.frame(
        tdb = c(17, 20, 23, 26, 29),
        relhum = c(70, 62, 54, 48, 42),
        wetbulb = c(12, 14, 16, 18, 20),
        vappres = c(1200, 1500, 1800, 2100, 2400),
        specvol = c(0.82, 0.84, 0.86, 0.88, 0.90),
        enthalpy = c(30000, 38000, 46000, 54000, 62000),
        weight = 1:5,
        state = factor(c("old", "old", "middle", "new", "new")),
        label = LETTERS[1:5]
    )

    cases <- list(
        relhum = list(
            plot = ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
                ggplot2::geom_point(
                    ggplot2::aes(tdb, relhum = relhum, alpha = weight),
                    stat = "relhum",
                    size = 4
                ),
            aes = "alpha"
        ),
        wetbulb = list(
            plot = ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
                ggplot2::geom_point(
                    ggplot2::aes(tdb, wetbulb = wetbulb, size = weight),
                    stat = "wetbulb",
                    alpha = 0.8
                ),
            aes = "size"
        ),
        vappres = list(
            plot = ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
                ggplot2::geom_point(
                    ggplot2::aes(tdb, vappres = vappres, shape = state),
                    stat = "vappres",
                    size = 3
                ),
            aes = "shape"
        ),
        specvol = list(
            plot = ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
                ggplot2::geom_point(
                    ggplot2::aes(tdb, specvol = specvol, colour = state),
                    stat = "specvol",
                    size = 3
                ),
            aes = "colour"
        ),
        enthalpy = list(
            plot = ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
                ggplot2::geom_text(
                    ggplot2::aes(tdb, enthalpy = enthalpy, label = label),
                    stat = "enthalpy"
                ),
            aes = "label"
        )
    )

    for (case in cases) {
        expect_warning(built <- ggplot2::ggplot_build(case$plot), NA)
        values <- first_built_data(built)[[case$aes]]
        expect_false(anyNA(values))
        expect_gt(length(unique(values)), 1L)
    }
})
test_that("Psychrometric stats draw retained aesthetics in common plots", {
    d <- data.frame(
        tdb = c(17, 20, 23, 26, 29),
        relhum = c(72, 64, 56, 50, 44),
        wetbulb = c(12, 14, 16, 18, 20),
        vappres = c(1200, 1500, 1800, 2100, 2400),
        enthalpy = c(30000, 38000, 46000, 54000, 62000),
        weight = 1:5,
        state = factor(c("early", "early", "middle", "late", "late")),
        label = LETTERS[1:5]
    )
    line_data <- data.frame(
        tdb = c(18, 20, 22, 24, 26, 22, 24, 26, 28, 30),
        wetbulb = c(13, 14, 15, 16, 17, 16, 17, 18, 19, 20),
        process = rep(c("cooling", "heating"), each = 5),
        load = rep(c(1.2, 2.0), each = 5)
    )

    p_alpha <- ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
        geom_psychro_grid_relhum(label = FALSE) +
        ggplot2::geom_point(
            ggplot2::aes(tdb, relhum = relhum, alpha = weight),
            stat = "relhum",
            colour = "#175676",
            size = 4
        ) +
        ggplot2::scale_alpha_continuous(range = c(0.25, 1))

    p_lines <- ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
        geom_psychro_grid_wetbulb(label = FALSE) +
        ggplot2::geom_line(
            ggplot2::aes(
                tdb,
                wetbulb = wetbulb,
                colour = process,
                linetype = process,
                linewidth = load,
                group = process
            ),
            data = line_data,
            stat = "wetbulb"
        ) +
        # Keep this vdiffr case focused on retained stat aesthetics instead of
        # ggplot2's version-dependent guide ordering heuristics.
        ggplot2::guides(
            colour = ggplot2::guide_legend(order = 1L),
            linetype = ggplot2::guide_legend(order = 1L),
            linewidth = ggplot2::guide_legend(order = 2L)
        )

    p_mixed <- ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
        geom_psychro_grid_relhum(label = FALSE) +
        ggplot2::geom_point(
            ggplot2::aes(tdb, vappres = vappres, size = weight, shape = state),
            stat = "vappres",
            colour = "#7F2CCB",
            alpha = 0.78
        ) +
        ggplot2::geom_text(
            ggplot2::aes(tdb, enthalpy = enthalpy, label = label),
            stat = "enthalpy",
            colour = "#2B7A0B",
            size = 3,
            nudge_y = 0.0016
        )

    testthat::skip_on_os(c("linux", "windows"))

    vdiffr::expect_doppelganger(
        "psychro stat alpha points",
        p_alpha
    )
    vdiffr::expect_doppelganger(
        "psychro stat path aesthetics",
        p_lines
    )
    vdiffr::expect_doppelganger(
        "psychro stat mixed aesthetics",
        p_mixed
    )
})
