# Focused comfort calculation, model, and layer behavior tests.

test_that("comfort model objects validate inputs", {
    expect_s3_class(comfort_model_pmv(), "PsyComfortModel")
    expect_s3_class(comfort_model_set(), "PsyComfortModel")
    expect_s3_class(comfort_model_adaptive(t_running = 20), "PsyComfortModel")
    expect_s3_class(comfort_model_heat_index(), "PsyComfortModel")
    expect_error(comfort_model_pmv(model = "bad"))
    expect_error(comfort_model_pmv(vr = c(0.1, 0.2)), "`vr`")
    expect_error(comfort_model_pmv(tr = NA_real_), "`tr`")
    expect_error(comfort_model_set(p_atm = NA_real_), "`p_atm`")
    expect_error(comfort_model_adaptive(t_running = NA_real_), "`t_running`")
    expect_error(comfort_model_adaptive(t_running = c(20, 21)), "`t_running`")
    expect_error(comfort_model_heat_index(limit_inputs = NA), "`limit_inputs`")
    expect_error(comfort__model_type(list()), "comfort_model")
    expect_error(comfort_model_adaptive(t_running = 20, standard = "bad"))
    expect_error(comfort_pmv_ashrae55(edition = "2020"), "edition")
    expect_error(comfort_pmv_en15251(edition = "2019"), "edition")
    expect_error(
        comfort_pmv_ashrae55(range = c(0.5, -0.5)),
        "strictly increasing"
    )
    expect_error(
        comfort_pmv_en15251(breaks = c(-0.7, 0.2, -0.2, 0.7)),
        "strictly increasing"
    )
})
test_that("comfort layer internals are not exported", {
    expect_false(any(grepl("^StatComfort", getNamespaceExports("ggpsychro"))))
    rd <- testthat::test_path("..", "..", "man", "ggpsychro-extensions.Rd")
    if (file.exists(rd)) {
        expect_false(any(grepl("StatComfort", readLines(rd, warn = FALSE))))
    }
})
test_that("comfort zones and state stats build model-specific fields", {
    zone <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__zone(model = comfort_model_adaptive(t_running = 20))
    ))
    expect_equal(range(zone$x), c(20.5, 27.5), tolerance = 1e-8)
    expect_equal(range(zone$y), c(0, 0.02), tolerance = 1e-8)

    d <- data.frame(tdb = c(24, 26), relhum = c(50, 60))
    pmv_state <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(d) +
            stat_comfort_state(ggplot2::aes(tdb = tdb, relhum = relhum))
    ))
    expect_true(all(c("pmv", "ppd") %in% names(pmv_state)))

    set_state <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(d) +
            stat_comfort_state(
                ggplot2::aes(tdb = tdb, relhum = relhum),
                model = comfort_model_set()
            )
    ))
    expect_true("set" %in% names(set_state))

    adaptive_state <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(d) +
            stat_comfort_state(
                ggplot2::aes(tdb = tdb, relhum = relhum),
                model = comfort_model_adaptive(t_running = 20)
            )
    ))
    expect_true("acceptability" %in% names(adaptive_state))
})
