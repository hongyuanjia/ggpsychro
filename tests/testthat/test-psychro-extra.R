test_that("GetTDewPointFromHumRatioOnly()", {
    with_units(
        "SI",
        expect_equal(
            GetTDewPointFromHumRatioOnly(0.01, 101325),
            psychrolib::GetTDewPointFromHumRatio(20, 0.01, 101325),
            tolerance = 1E-4
        )
    )

    with_units("SI", {
        pressure <- psychrolib::GetStandardAtmPressure(0)
        hum_ratio <- c(1e-7, 0.004, 0.01, 0.02)
        vap_pres <- psychrolib::GetVapPresFromHumRatio(hum_ratio, pressure)
        fallback <- vapply(vap_pres, GetTDewPointFromVapPresOnly, double(1L))
        native <- GetTDewPointFromHumRatioOnly(hum_ratio, pressure)
        expect_equal(native, fallback, tolerance = 1e-4)
    })

    with_units("IP", {
        pressure <- psychrolib::GetStandardAtmPressure(0)
        hum_ratio <- c(1e-7, 0.004, 0.01, 0.02)
        vap_pres <- psychrolib::GetVapPresFromHumRatio(hum_ratio, pressure) *
            6894.7572931783
        fallback <- vapply(vap_pres, GetTDewPointFromVapPresOnly, double(1L)) *
            9 / 5 + 32
        native <- GetTDewPointFromHumRatioOnly(hum_ratio, pressure)
        expect_equal(native, fallback, tolerance = 1e-4)
    })
})

test_that("with_units restores nested unit systems", {
    with_units("IP", {
        expect_true(psychrolib::isIP())
        with_units("SI", {
            expect_false(psychrolib::isIP())
        })
        expect_true(psychrolib::isIP())
    })
})

test_that("GetHumRatioFromAirVolume() round-trips specific volume", {
    with_units("SI", {
        pressure <- psychrolib::GetStandardAtmPressure(0)
        hum_ratio <- 0.01
        volume <- psychrolib::GetMoistAirVolume(25, hum_ratio, pressure)

        expect_equal(
            GetHumRatioFromAirVolume(25, volume, pressure),
            hum_ratio,
            tolerance = 1E-8
        )
    })

    with_units("IP", {
        pressure <- psychrolib::GetStandardAtmPressure(0)
        hum_ratio <- 0.008
        volume <- psychrolib::GetMoistAirVolume(77, hum_ratio, pressure)

        expect_equal(
            GetHumRatioFromAirVolume(77, volume, pressure),
            hum_ratio,
            tolerance = 1E-8
        )
    })
})
