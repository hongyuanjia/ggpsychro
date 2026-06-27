test_that("Psychrometric state stat converts supported properties", {
    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))

    cases <- list(
        humratio = list(
            data = data.frame(tdb = 25, humratio = 10),
            mapping = ggplot2::aes(tdb = tdb, humratio = humratio),
            expected = 0.010
        ),
        relhum = list(
            data = data.frame(tdb = 25, relhum = 50),
            mapping = ggplot2::aes(tdb = tdb, relhum = relhum),
            expected = with_units("SI", psychrolib::GetHumRatioFromRelHum(25, 0.5, pressure))
        ),
        wetbulb = list(
            data = data.frame(tdb = 25, wetbulb = 18),
            mapping = ggplot2::aes(tdb = tdb, wetbulb = wetbulb),
            expected = with_units("SI", psychrolib::GetHumRatioFromTWetBulb(25, 18, pressure))
        ),
        vappres = list(
            data = data.frame(tdb = 25, vappres = 1500),
            mapping = ggplot2::aes(tdb = tdb, vappres = vappres),
            expected = with_units("SI", psychrolib::GetHumRatioFromVapPres(1500, pressure))
        ),
        specvol = list(
            data = data.frame(tdb = 25, specvol = 0.86),
            mapping = ggplot2::aes(tdb = tdb, specvol = specvol),
            expected = with_units("SI", GetHumRatioFromAirVolume(25, 0.86, pressure))
        ),
        enthalpy = list(
            data = data.frame(tdb = 25, enthalpy = 55000),
            mapping = ggplot2::aes(tdb = tdb, enthalpy = enthalpy),
            expected = with_units("SI", GetHumRatioFromEnthalpyAndTDryBulb(55000, 25))
        )
    )

    for (case in cases) {
        built <- ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
                stat_psychro_state(case$mapping, data = case$data)
        )
        expect_equal(built$data[[1L]]$y, case$expected, tolerance = 1e-8)
    }
})

test_that("Psychrometric process supports IP units and Mollier charts", {
    ip <- data.frame(tdb = c(70, 75, 80), relhum = c(40, 50, 60))
    pressure_ip <- with_units("IP", psychrolib::GetStandardAtmPressure(0))
    expected_ip <- with_units(
        "IP",
        psychrolib::GetHumRatioFromRelHum(ip$tdb, ip$relhum / 100, pressure_ip)
    )

    built_ip <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(50, 100), hum_lim = c(0, 140), units = "IP") +
            geom_psychro_process(ggplot2::aes(tdb = tdb, relhum = relhum), data = ip)
    )
    expect_equal(built_ip$data[[1L]]$x, ip$tdb, tolerance = 1e-8)
    expect_equal(built_ip$data[[1L]]$y, expected_ip, tolerance = 1e-8)

    built_mollier <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30), mollier = TRUE) +
            geom_psychro_process(ggplot2::aes(tdb = tdb, relhum = relhum),
                data = data.frame(tdb = c(20, 25), relhum = c(50, 60)))
    )
    expect_equal(built_mollier$data[[1L]]$y, c(20, 25), tolerance = 1e-8)
    expect_gt(built_mollier$data[[1L]]$x[[2L]], built_mollier$data[[1L]]$x[[1L]])
})

test_that("Psychrometric state stat validates property inputs", {
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
                geom_psychro_process(ggplot2::aes(tdb = tdb),
                    data = data.frame(tdb = 25))
        ),
        "One psychrometric state aesthetic"
    )

    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
                geom_psychro_process(
                    ggplot2::aes(tdb = tdb, relhum = relhum, wetbulb = wetbulb),
                    data = data.frame(tdb = 25, relhum = 50, wetbulb = 18)
                )
        ),
        "Only one psychrometric state aesthetic"
    )

    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
                geom_psychro_process(ggplot2::aes(tdb = tdb, relhum = relhum),
                    data = data.frame(tdb = 25, relhum = 150))
        ),
        "`relhum` must be in the range"
    )
})

test_that("Psychrometric zones build all supported zone types", {
    base <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30))

    zones <- list(
        `dbt-rh` = base + geom_psychro_zone(
            data = data.frame(tdb_min = 20, tdb_max = 28, relhum_min = 40, relhum_max = 60),
            ggplot2::aes(tdb_min = tdb_min, tdb_max = tdb_max,
                relhum_min = relhum_min, relhum_max = relhum_max),
            type = "dbt-rh"
        ),
        `enthalpy-rh` = base + geom_psychro_zone(
            data = data.frame(enthalpy_min = 40000, enthalpy_max = 65000,
                relhum_min = 30, relhum_max = 80),
            ggplot2::aes(enthalpy_min = enthalpy_min, enthalpy_max = enthalpy_max,
                relhum_min = relhum_min, relhum_max = relhum_max),
            type = "enthalpy-rh"
        ),
        `specvol-rh` = base + geom_psychro_zone(
            data = data.frame(specvol_min = 0.84, specvol_max = 0.90,
                relhum_min = 30, relhum_max = 90),
            ggplot2::aes(specvol_min = specvol_min, specvol_max = specvol_max,
                relhum_min = relhum_min, relhum_max = relhum_max),
            type = "specvol-rh"
        ),
        `dbt-wmax` = base + geom_psychro_zone(
            data = data.frame(tdb_min = 10, tdb_max = 35, humratio_max = 12),
            ggplot2::aes(tdb_min = tdb_min, tdb_max = tdb_max,
                humratio_max = humratio_max),
            type = "dbt-wmax"
        ),
        `xy-points` = base + geom_psychro_zone(
            data = data.frame(tdb = c(20, 26, 28), humratio = c(6, 8, 6)),
            ggplot2::aes(tdb = tdb, humratio = humratio),
            type = "xy-points"
        )
    )

    for (zone in zones) {
        built <- ggplot2::ggplot_build(zone)
        expect_gt(nrow(built$data[[1L]]), 0L)
        expect_true(all(is.finite(built$data[[1L]]$x)))
        expect_true(all(is.finite(built$data[[1L]]$y)))
    }

    wmax <- ggplot2::ggplot_build(zones$`dbt-wmax`)$data[[1L]]
    expect_lte(max(wmax$y), 0.012)
    expect_gte(min(wmax$y), 0)

    xy <- ggplot2::ggplot_build(zones$`xy-points`)$data[[1L]]
    expect_equal(nrow(xy), 4L)
    expect_equal(xy$x[[1L]], xy$x[[nrow(xy)]], tolerance = 1e-8)
    expect_equal(xy$y[[1L]], xy$y[[nrow(xy)]], tolerance = 1e-8)
})

test_that("Psychrometric zone aliases and validation work", {
    base <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30))
    data <- data.frame(specvol_min = 0.84, specvol_max = 0.90,
        relhum_min = 30, relhum_max = 90)
    mapping <- ggplot2::aes(specvol_min = specvol_min, specvol_max = specvol_max,
        relhum_min = relhum_min, relhum_max = relhum_max)

    specvol <- ggplot2::ggplot_build(
        base + geom_psychro_zone(mapping, data = data, type = "specvol-rh")
    )$data[[1L]]
    volume <- ggplot2::ggplot_build(
        base + geom_psychro_zone(mapping, data = data, type = "volume-rh")
    )$data[[1L]]
    expect_equal(volume$x, specvol$x, tolerance = 1e-8)
    expect_equal(volume$y, specvol$y, tolerance = 1e-8)

    expect_error(
        ggplot2::ggplot_build(
            base + geom_psychro_zone(
                data = data.frame(tdb_min = 20, tdb_max = 25, relhum_min = -1, relhum_max = 50),
                ggplot2::aes(tdb_min = tdb_min, tdb_max = tdb_max,
                    relhum_min = relhum_min, relhum_max = relhum_max),
                type = "dbt-rh"
            )
        ),
        "`relhum` limits must be in the range"
    )
})

test_that("Psychrometric zones and process lines have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    zones <- rbind(
        data.frame(name = "comfort", tdb_min = 20, tdb_max = 26, relhum_min = 35, relhum_max = 60),
        data.frame(name = "humid", tdb_min = 24, tdb_max = 32, relhum_min = 60, relhum_max = 85)
    )
    process <- data.frame(tdb = c(18, 23, 28, 31), relhum = c(70, 55, 45, 55))

    vdiffr::expect_doppelganger(
        "psychro zones",
        ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
            psychro_preset("minimal") +
            geom_psychro_zone(
                ggplot2::aes(tdb_min = tdb_min, tdb_max = tdb_max,
                    relhum_min = relhum_min, relhum_max = relhum_max, fill = name),
                data = zones,
                type = "dbt-rh", alpha = 0.28, colour = NA
            )
    )

    vdiffr::expect_doppelganger(
        "psychro process",
        ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
            psychro_preset("minimal") +
            geom_psychro_process(
                ggplot2::aes(tdb = tdb, relhum = relhum),
                data = process,
                colour = "#0f766e", linewidth = 1,
                arrow = grid::arrow(length = grid::unit(0.08, "inches"))
            ) +
            stat_psychro_state(
                ggplot2::aes(tdb = tdb, relhum = relhum),
                data = process,
                colour = "#0f766e", size = 2
            )
    )
})
