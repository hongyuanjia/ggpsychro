# Focused comfort calculation, model, and layer behavior tests.

test_that("comfort PMV and PPD match fixed pythermalcomfort oracle values", {
    cases <- list(
        si_default_22 = list(
            args = list(
                tdb = 22,
                tr = 25,
                vr = 0.1,
                rh = 50,
                met = 1.4,
                clo = 0.5
            ),
            tolerance = 1e-8
        ),
        si_default_25 = list(
            args = list(
                tdb = 25,
                tr = 25,
                vr = 0.1,
                rh = 50,
                met = 1.4,
                clo = 0.5
            ),
            tolerance = 1e-8
        ),
        vector_mixed_inputs = list(
            args = list(
                tdb = c(22, 25, 28),
                tr = c(25, 25, 26),
                vr = c(0.1, 0.2, 0.3),
                rh = c(50, 55, 60),
                met = c(1.1, 1.4, 1.8),
                clo = c(0.5, 0.6, 0.7)
            ),
            tolerance = 1e-8
        ),
        si_unrounded = list(
            args = list(
                tdb = 26,
                tr = 25,
                vr = 0.2,
                rh = 60,
                met = 1.2,
                clo = 0.7,
                round_output = FALSE
            ),
            tolerance = 1e-6
        ),
        ip_default = list(
            args = list(
                tdb = 77,
                tr = 77,
                vr = 0.328084,
                rh = 50,
                met = 1.2,
                clo = 0.5,
                units = "IP"
            ),
            tolerance = 1e-8
        ),
        limit_inputs_low_tdb = list(
            args = list(
                tdb = 5,
                tr = 25,
                vr = 0.1,
                rh = 50,
                met = 1.2,
                clo = 0.5
            ),
            tolerance = 1e-8
        ),
        limit_inputs_false_low_tdb = list(
            args = list(
                tdb = 5,
                tr = 25,
                vr = 0.1,
                rh = 50,
                met = 1.2,
                clo = 0.5,
                limit_inputs = FALSE,
                round_output = FALSE
            ),
            tolerance = 1e-6
        ),
        high_rh_clo = list(
            args = list(
                tdb = 24,
                tr = 24,
                vr = 0.1,
                rh = 100,
                met = 1.2,
                clo = 1.5
            ),
            tolerance = 1e-8
        ),
        upper_tr_v_met = list(
            args = list(
                tdb = 28,
                tr = 35,
                vr = 1,
                rh = 70,
                met = 2,
                clo = 0.3
            ),
            tolerance = 1e-8
        )
    )

    for (case in names(cases)) {
        result <- do.call(comfort_pmv, cases[[case]]$args)
        for (metric in c("pmv", "ppd", "tsv")) {
            expect_comfort_oracle(
                result,
                "pmv",
                case,
                metric,
                tolerance = cases[[case]]$tolerance
            )
        }
    }
})
test_that("comfort SET matches fixed pythermalcomfort oracle value", {
    cases <- list(
        gagge_default = list(
            args = list(
                tdb = 25,
                tr = 25,
                v = 0.1,
                rh = 50,
                met = 1.2,
                clo = 0.5
            ),
            tolerance = 1e-8
        ),
        vector_mixed_inputs = list(
            args = list(
                tdb = c(24, 26, 28),
                tr = c(24, 27, 30),
                v = c(0.1, 0.4, 0.8),
                rh = c(45, 60, 70),
                met = c(1.1, 1.6, 2.0),
                clo = c(0.5, 0.7, 0.8)
            ),
            tolerance = 1e-8
        ),
        standing_unrounded = list(
            args = list(
                tdb = 28,
                tr = 30,
                v = 0.6,
                rh = 70,
                met = 1.6,
                clo = 0.7,
                wme = 0.1,
                round_output = FALSE
            ),
            tolerance = 1e-4
        ),
        sitting_low_pressure = list(
            args = list(
                tdb = 25,
                tr = 26,
                v = 0.2,
                rh = 45,
                met = 1.1,
                clo = 0.6,
                body_surface_area = 1.7,
                p_atm = 90000,
                position = "sitting"
            ),
            tolerance = 1e-8
        ),
        limit_inputs_low_tdb = list(
            args = list(
                tdb = 5,
                tr = 25,
                v = 0.1,
                rh = 50,
                met = 1.2,
                clo = 0.5
            ),
            tolerance = 1e-8
        ),
        limit_inputs_false_low_tdb = list(
            args = list(
                tdb = 5,
                tr = 25,
                v = 0.1,
                rh = 50,
                met = 1.2,
                clo = 0.5,
                limit_inputs = FALSE,
                round_output = FALSE
            ),
            tolerance = 1e-4
        ),
        boundary_low = list(
            args = list(
                tdb = 10,
                tr = 10,
                v = 0,
                rh = 0,
                met = 1,
                clo = 0
            ),
            tolerance = 1e-8
        ),
        boundary_high = list(
            args = list(
                tdb = 35,
                tr = 35,
                v = 2,
                rh = 100,
                met = 4,
                clo = 1.5
            ),
            tolerance = 1e-8
        ),
        wme_unrounded = list(
            args = list(
                tdb = 26,
                tr = 27,
                v = 0.4,
                rh = 55,
                met = 2,
                clo = 0.6,
                wme = 0.4,
                round_output = FALSE
            ),
            tolerance = 1e-4
        )
    )

    for (case in names(cases)) {
        result <- do.call(comfort_set, cases[[case]]$args)
        expect_comfort_oracle(
            result,
            "set",
            case,
            "set",
            tolerance = cases[[case]]$tolerance
        )
    }
})
test_that("comfort adaptive models match fixed pythermalcomfort oracle values", {
    ashrae_cases <- list(
        default = list(
            args = list(tdb = 25, tr = 25, t_running = 20, v = 0.1),
            tolerance = 1e-8
        ),
        vector_mixed_inputs = list(
            args = list(
                tdb = c(24, 27, 30),
                tr = c(24, 28, 30),
                t_running = c(18, 24, 30),
                v = c(0.1, 1.0, 1.3)
            ),
            tolerance = 1e-8
        ),
        high_air_speed = list(
            args = list(tdb = 27, tr = 27, t_running = 24, v = 1.0),
            tolerance = 1e-8
        ),
        ip_default = list(
            args = list(
                tdb = 77,
                tr = 77,
                t_running = 68,
                v = 0.328084,
                units = "IP"
            ),
            tolerance = 0.05
        ),
        limit_inputs_false_low_running = list(
            args = list(
                tdb = 25,
                tr = 25,
                t_running = 5,
                v = 0.1,
                limit_inputs = FALSE,
                round_output = FALSE
            ),
            tolerance = 1e-8
        )
    )
    ashrae_metrics <- c(
        "tmp_cmf",
        "tmp_cmf_80_low",
        "tmp_cmf_80_up",
        "tmp_cmf_90_low",
        "tmp_cmf_90_up",
        "acceptability_80",
        "acceptability_90"
    )

    for (case in names(ashrae_cases)) {
        result <- do.call(
            comfort_adaptive,
            c(ashrae_cases[[case]]$args, list(standard = "ashrae55"))
        )
        for (metric in ashrae_metrics) {
            expect_comfort_oracle(
                result,
                "adaptive_ashrae",
                case,
                metric,
                tolerance = ashrae_cases[[case]]$tolerance
            )
        }
    }

    en_cases <- list(
        default = list(
            args = list(tdb = 25, tr = 25, t_running = 20, v = 0.1),
            tolerance = 1e-8
        ),
        vector_mixed_inputs = list(
            args = list(
                tdb = c(24, 27, 30),
                tr = c(24, 28, 30),
                t_running = c(18, 24, 30),
                v = c(0.1, 1.0, 1.3)
            ),
            tolerance = 1e-8
        ),
        high_air_speed = list(
            args = list(tdb = 27, tr = 27, t_running = 24, v = 1.0),
            tolerance = 1e-8
        ),
        ip_default = list(
            args = list(
                tdb = 77,
                tr = 77,
                t_running = 68,
                v = 0.328084,
                units = "IP"
            ),
            tolerance = 0.05
        ),
        limit_inputs_false_low_running = list(
            args = list(
                tdb = 25,
                tr = 25,
                t_running = 5,
                v = 0.1,
                limit_inputs = FALSE,
                round_output = FALSE
            ),
            tolerance = 1e-8
        )
    )
    en_metrics <- c(
        "tmp_cmf",
        "tmp_cmf_cat_i_low",
        "tmp_cmf_cat_i_up",
        "tmp_cmf_cat_ii_low",
        "tmp_cmf_cat_ii_up",
        "tmp_cmf_cat_iii_low",
        "tmp_cmf_cat_iii_up",
        "acceptability_cat_i",
        "acceptability_cat_ii",
        "acceptability_cat_iii"
    )

    for (case in names(en_cases)) {
        result <- do.call(
            comfort_adaptive,
            c(en_cases[[case]]$args, list(standard = "en16798"))
        )
        for (metric in en_metrics) {
            expect_comfort_oracle(
                result,
                "adaptive_en",
                case,
                metric,
                tolerance = en_cases[[case]]$tolerance
            )
        }
    }
})
test_that("comfort heat index matches Marsh and NOAA-style expected behavior", {
    hi <- comfort_heat_index(90, rh = 70, units = "IP")
    expect_equal(hi$heat_index, 105.9, tolerance = 0.05)
    expect_equal(hi$category, "danger")
    expect_equal(hi$category_id, 3)

    low <- comfort_heat_index(40, rh = 50, units = "IP")
    expect_equal(low$heat_index, 40)
    expect_equal(low$category, "none")

    exposed <- comfort_heat_index(90, rh = 70, solar_exposure = 1, units = "IP")
    expect_equal(exposed$heat_index - hi$heat_index, 8, tolerance = 0.05)

    exposure_vector <- comfort_heat_index(
        c(90, 90),
        rh = 70,
        solar_exposure = c(0, 1),
        units = "IP"
    )
    expect_equal(diff(exposure_vector$heat_index), 8, tolerance = 0.05)
    expect_true(is.na(
        comfort_heat_index(
            90,
            rh = 70,
            solar_exposure = NA_real_,
            units = "IP"
        )$heat_index
    ))
    expect_error(
        comfort_heat_index(90, rh = 70, solar_exposure = 2, units = "IP"),
        "solar_exposure"
    )
    expect_error(
        comfort_heat_index(90, rh = 70, solar_exposure = -0.1, units = "IP"),
        "solar_exposure"
    )

    si <- comfort_heat_index(unit__c_from_f(90), rh = 70, round_output = FALSE)
    ip <- comfort_heat_index(90, rh = 70, units = "IP", round_output = FALSE)
    expect_equal(unit__f_from_c(si$heat_index), ip$heat_index, tolerance = 1e-8)

    expect_true(is.na(
        comfort_heat_index(90, rh = 150, units = "IP")$heat_index
    ))
    expect_error(comfort_model_heat_index(solar_exposure = 2), "solar_exposure")
})
test_that("comfort calculations handle IP units and input limits", {
    si <- comfort_pmv(25, tr = 25, vr = 0.1, rh = 50, met = 1.4, clo = 0.5)
    ip <- comfort_pmv(
        77,
        tr = 77,
        vr = 0.3281,
        rh = 50,
        met = 1.4,
        clo = 0.5,
        units = "IP"
    )
    expect_equal(ip$pmv, si$pmv)
    expect_equal(ip$ppd, si$ppd)

    set_si <- comfort_set(
        25,
        tr = 25,
        v = 0.1,
        rh = 50,
        met = 1.2,
        clo = 0.5,
        round_output = FALSE
    )
    set_ip <- comfort_set(
        77,
        tr = 77,
        v = 0.3281,
        rh = 50,
        met = 1.2,
        clo = 0.5,
        units = "IP",
        round_output = FALSE
    )
    expect_equal(set_ip$set, unit__f_from_c(set_si$set), tolerance = 0.05)

    expect_true(is.na(comfort_pmv(5, rh = 50)$pmv[[1L]]))
    expect_true(is.na(comfort_set(5, rh = 50)$set[[1L]]))
    expect_true(is.na(comfort_pmv(25, rh = -10)$pmv[[1L]]))
    expect_true(is.na(comfort_set(25, rh = 150)$set[[1L]]))
    expect_true(is.na(comfort_adaptive(25, t_running = 5)$tmp_cmf[[1L]]))
    expect_true(is.na(comfort_adaptive(
        25,
        t_running = 20,
        v = -0.1,
        standard = "en16798"
    )$acceptability[[1L]]))
    expect_true(is.na(comfort_pmv(c(25, NA), rh = 50)$pmv[[2L]]))
})
