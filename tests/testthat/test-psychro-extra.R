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

