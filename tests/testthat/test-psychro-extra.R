test_that("GetTDewPointFromHumRatioOnly()", {
    with_units(
        "SI",
        expect_equal(
            GetTDewPointFromHumRatioOnly(0.01, 101325),
            psychrolib::GetTDewPointFromHumRatio(20, 0.01, 101325),
            tolerance = 1E-4
        )
    )
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
