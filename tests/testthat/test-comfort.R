comfort_oracle <- function(model, case, metric) {
    path <- testthat::test_path("fixtures", "comfort-oracle.csv")
    oracle <- utils::read.csv(path, stringsAsFactors = FALSE)
    value <- oracle$value[
        oracle$model == model & oracle$case == case & oracle$metric == metric
    ]
    expect_length(value, 1L)
    value
}

test_that("comfort PMV and PPD match fixed pythermalcomfort oracle values", {
    result <- comfort_pmv(
        tdb = c(22, 25), tr = 25, vr = 0.1,
        rh = 50, met = 1.4, clo = 0.5
    )

    expect_equal(result$pmv[[1L]], comfort_oracle("pmv", "iso_vector_22", "pmv"))
    expect_equal(result$ppd[[1L]], comfort_oracle("pmv", "iso_vector_22", "ppd"))
    expect_equal(result$pmv[[2L]], comfort_oracle("pmv", "iso_vector_25", "pmv"))
    expect_equal(result$ppd[[2L]], comfort_oracle("pmv", "iso_vector_25", "ppd"))
    expect_equal(result$tsv, c("Neutral", "Neutral"))
})

test_that("comfort SET matches fixed pythermalcomfort oracle value", {
    result <- comfort_set(25, tr = 25, v = 0.1, rh = 50, met = 1.2, clo = 0.5)
    expect_equal(result$set[[1L]], comfort_oracle("set", "gagge_default", "set"))
})

test_that("comfort adaptive models match fixed pythermalcomfort oracle values", {
    ashrae <- comfort_adaptive(25, tr = 25, t_running = 20, v = 0.1,
        standard = "ashrae55")
    expect_equal(ashrae$tmp_cmf[[1L]],
        comfort_oracle("adaptive_ashrae", "default", "tmp_cmf"))
    expect_equal(ashrae$tmp_cmf_80_low[[1L]],
        comfort_oracle("adaptive_ashrae", "default", "tmp_cmf_80_low"))
    expect_equal(ashrae$tmp_cmf_80_up[[1L]],
        comfort_oracle("adaptive_ashrae", "default", "tmp_cmf_80_up"))
    expect_equal(ashrae$tmp_cmf_90_low[[1L]],
        comfort_oracle("adaptive_ashrae", "default", "tmp_cmf_90_low"))
    expect_equal(ashrae$tmp_cmf_90_up[[1L]],
        comfort_oracle("adaptive_ashrae", "default", "tmp_cmf_90_up"))
    expect_true(ashrae$acceptability[[1L]])

    en <- comfort_adaptive(25, tr = 25, t_running = 20, v = 0.1,
        standard = "en16798")
    expect_equal(en$tmp_cmf[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf"))
    expect_equal(en$tmp_cmf_cat_i_low[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf_cat_i_low"))
    expect_equal(en$tmp_cmf_cat_i_up[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf_cat_i_up"))
    expect_equal(en$tmp_cmf_cat_ii_low[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf_cat_ii_low"))
    expect_equal(en$tmp_cmf_cat_ii_up[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf_cat_ii_up"))
    expect_equal(en$tmp_cmf_cat_iii_low[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf_cat_iii_low"))
    expect_equal(en$tmp_cmf_cat_iii_up[[1L]],
        comfort_oracle("adaptive_en", "default", "tmp_cmf_cat_iii_up"))
    expect_true(en$acceptability[[1L]])
})

test_that("comfort calculations handle IP units and input limits", {
    si <- comfort_pmv(25, tr = 25, vr = 0.1, rh = 50, met = 1.4, clo = 0.5)
    ip <- comfort_pmv(77, tr = 77, vr = 0.3281, rh = 50, met = 1.4, clo = 0.5,
        units = "IP")
    expect_equal(ip$pmv, si$pmv)
    expect_equal(ip$ppd, si$ppd)

    set_si <- comfort_set(25, tr = 25, v = 0.1, rh = 50, met = 1.2, clo = 0.5,
        round_output = FALSE)
    set_ip <- comfort_set(77, tr = 77, v = 0.3281, rh = 50, met = 1.2, clo = 0.5,
        units = "IP", round_output = FALSE)
    expect_equal(set_ip$set, get_f_from_c(set_si$set), tolerance = 0.05)

    expect_true(is.na(comfort_pmv(5, rh = 50)$pmv[[1L]]))
    expect_true(is.na(comfort_set(5, rh = 50)$set[[1L]]))
    expect_true(is.na(comfort_adaptive(25, t_running = 5)$tmp_cmf[[1L]]))
    expect_true(is.na(comfort_pmv(c(25, NA), rh = 50)$pmv[[2L]]))
})

test_that("comfort model objects validate inputs", {
    expect_s3_class(comfort_model_pmv(), "PsyComfortModel")
    expect_s3_class(comfort_model_set(), "PsyComfortModel")
    expect_s3_class(comfort_model_adaptive(t_running = 20), "PsyComfortModel")
    expect_error(comfort_model_type(list()), "comfort_model")
    expect_error(comfort_model_adaptive(t_running = 20, standard = "bad"))
})

test_that("comfort overlay and contour build on psychrometric panel grids", {
    overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(n = c(24, 16), gap = 0) +
            scale_fill_comfort_pmv()
    )$data[[1L]]
    expect_gt(nrow(overlay), 0L)
    expect_true(all(is.finite(overlay$value)))
    expect_true(all(overlay$y >= 0))
    expect_true(all(c("level_low", "level_high", "edge_level") %in% names(overlay)))
    expect_false("width" %in% names(overlay))
    expect_equal(unique(overlay$alpha), 0.55)
    overlay_breaks <- sort(unique(c(
        overlay$level_low[is.finite(overlay$level_low)],
        overlay$level_high[is.finite(overlay$level_high)]
    )))
    expect_true(all(abs(diff(overlay_breaks) - 0.25) < 1e-8))

    overlay_alpha <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(n = c(24, 16), alpha = 0.35)
    )$data[[1L]]
    expect_equal(unique(overlay_alpha$alpha), 0.35)

    isoband <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(method = "isoband", n = c(24, 16))
    )$data[[1L]]
    expect_gt(nrow(isoband), 0L)
    expect_true("level_mid" %in% names(isoband))
    expect_equal(unique(isoband$alpha), 0.55)

    tile <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(method = "tile", n = c(24, 16))
    )$data[[1L]]
    expect_gt(nrow(tile), 0L)
    expect_equal(unique(tile$alpha), 0.55)

    set_overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(model = comfort_model_set(), n = c(24, 16))
    )$data[[1L]]
    expect_gt(nrow(set_overlay), 0L)
    expect_equal(unique(set_overlay$alpha), 0.55)

    adaptive_overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(
                model = comfort_model_adaptive(t_running = 20),
                n = c(24, 16)
            )
    )$data[[1L]]
    expect_gt(nrow(adaptive_overlay), 0L)
    expect_equal(unique(adaptive_overlay$alpha), 0.55)

    tile_alpha <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(method = "tile", n = c(24, 16), alpha = 0.35)
    )$data[[1L]]
    expect_equal(unique(tile_alpha$alpha), 0.35)

    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))
    grid <- comfort_grid_data(
        comfort_model_pmv(), NULL, c(16, 16), 0, "SI", pressure,
        FALSE, c(15, 30), c(0, 35)
    )
    saturation_left <- psychro_saturation_humratio(
        grid$tdb - grid$width / 2, "SI", pressure
    )
    saturation_right <- psychro_saturation_humratio(
        grid$tdb + grid$width / 2, "SI", pressure
    )
    expect_true(all(
        grid$humratio - grid$height / 2 <=
            pmax(saturation_left, saturation_right) + 1e-8
    ))

    contour <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_contour(n = c(32, 20), breaks = c(-1, 0, 1))
    )$data[[1L]]
    expect_gt(nrow(contour), 0L)
    expect_true(all(contour$level %in% c(-1, 0, 1)))
})

test_that("PMV root-traced curves solve requested levels", {
    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))
    curves <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-0.5, 0, 0.5), 96, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "none"
    )
    expect_gt(nrow(curves), 0L)
    expect_true(all(vapply(split(curves$humratio, curves$level),
        function(x) all(diff(x) >= 0), logical(1L))))

    rh <- comfort_relhum_from_humratio(curves$tdb, curves$humratio, "SI", pressure)
    pmv <- comfort_pmv(curves$tdb, rh = rh, limit_inputs = FALSE,
        round_output = FALSE)$pmv
    expect_lt(max(abs(pmv - curves$level), na.rm = TRUE), 0.02)

    saturated <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-1, 0, 1), 120, "SI", pressure,
        FALSE, c(0, 35), c(0, 35), label = "none"
    )
    sat_exists <- vapply(c(-1, 0, 1), function(level) {
        length(comfort_pmv_curve_saturation_roots(
            comfort_model_pmv(), level, c(0, 35), c(0, 35),
            "SI", pressure, 120
        )$tdb) > 0L
    }, logical(1L))
    reaches_saturation <- vapply(split(saturated, saturated$level), function(x) {
        sat <- psychro_saturation_humratio(x$tdb, "SI", pressure)
        min(abs(x$humratio - sat), na.rm = TRUE) < 1e-8
    }, logical(1L))
    expect_true(all(reaches_saturation[sat_exists]))

    labels <- comfort_pmv_curve_data(
        comfort_model_pmv(), -3:3, 120, "SI", pressure,
        FALSE, c(5, 40), c(0, 24), label = "sensation"
    )
    expect_true(all(c("COLD", "COOL", "SLIGHTLY COOL", "NEUTRAL",
        "SLIGHTLY WARM", "WARM", "HOT") %in%
        unique(labels$label)))
    expect_equal(unique(labels$linetype[labels$level == 0]), "dashed")
    expect_equal(unique(labels$vjust), 0.5)

    rootband <- comfort_pmv_rootband_data(
        comfort_model_pmv(), NULL, c(-0.5, 0, 0.5), c(120, 60),
        "SI", pressure, FALSE, c(15, 30), c(0, 20)
    )
    for (level in c(-0.5, 0, 0.5)) {
        edge <- rootband[rootband$edge_level == level &
            is.finite(rootband$edge_level), , drop = FALSE]
        rh <- comfort_relhum_from_humratio(edge$tdb, edge$humratio, "SI", pressure)
        pmv <- comfort_pmv(edge$tdb, rh = rh, limit_inputs = FALSE,
            round_output = FALSE)$pmv
        expect_lt(max(abs(pmv - level), na.rm = TRUE), 0.02)
    }

    standard_band <- comfort_pmv_band_data(
        comfort_model_pmv(), c(-0.5, 0.5), c(140, 90),
        "SI", pressure, FALSE, c(5, 35), c(0, 24)
    )
    top <- standard_band[which.max(standard_band$humratio), , drop = FALSE]
    sat <- psychro_saturation_humratio(top$tdb, "SI", pressure)
    expect_lt(abs(top$humratio - sat), 1e-6)
})

test_that("PMV comfort lines and PMV-based standard zones build", {
    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))

    pmv_lines <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv_lines(levels = c(-1, 0, 1), n = 80)
    )$data
    expect_equal(length(pmv_lines), 2L)
    expect_true(all(c("level", "linetype") %in% names(pmv_lines[[1L]])))
    expect_equal(sort(unique(pmv_lines[[1L]]$level)), c(-1, 0, 1))
    expect_equal(unique(pmv_lines[[1L]]$linetype[pmv_lines[[1L]]$level == 0]), "dashed")
    expect_true(all(c("-1.0", "0.0", "+1.0") %in% unique(pmv_lines[[2L]]$label)))
    expect_true(all(table(pmv_lines[[2L]]$group) >= 2L))
    axis_y <- vapply(split(pmv_lines[[2L]]$y, pmv_lines[[2L]]$group),
        min, numeric(1L))
    expect_equal(length(unique(round(axis_y, 6))), 1L)
    expect_gt(min(axis_y), 0.0003)

    axis_default <- comfort_pmv_axis_label_data(
        comfort_model_pmv(), c(-1, 0, 1), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20)
    )
    expect_equal(unique(axis_default$hjust), 0.95)
    expect_equal(unique(axis_default$vjust), 0.5)
    axis_vjust <- comfort_pmv_axis_label_text_vjust(ggplot2::waiver())
    expect_s3_class(axis_vjust, "unit")
    expect_equal(as.numeric(axis_vjust), 3.5)

    axis_labels <- comfort_pmv_axis_label_data(
        comfort_model_pmv(), c(-1, 0, 1), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), axis_label_hjust = 0.015
    )
    expect_true(all(table(axis_labels$group) >= 2L))
    axis_hum <- vapply(split(axis_labels$humratio, axis_labels$group),
        min, numeric(1L))
    expect_equal(length(unique(round(axis_hum, 8))), 1L)
    expect_equal(unique(axis_labels$hjust), 0.985)
    expect_equal(unique(axis_labels$vjust), 0.5)

    trimmed <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-1, 0, 1), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "none",
        trim_axis_hjust = comfort_pmv_line_trim_hjust(0.015)
    )
    expect_gt(min(trimmed$humratio), 0)

    pmv_axis <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-1, 0, 1), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "axis"
    )
    expect_equal(unique(pmv_axis$vjust), 0.5)
    pmv_axis_custom <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-1, 0, 1), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "axis", label_vjust = 1.2
    )
    expect_equal(unique(pmv_axis_custom$vjust), 1.2)
    pmv_boundary <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-0.5, 0.5), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "boundary"
    )
    expect_equal(unique(pmv_boundary$vjust[pmv_boundary$level < 0]), -0.25)
    expect_equal(unique(pmv_boundary$vjust[pmv_boundary$level > 0]), 1.25)

    pmv_sensation <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-1, 0, 1), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "sensation"
    )
    expect_equal(unique(pmv_sensation$vjust), 0.5)

    ashrae <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_standard_zone(comfort_standard_ashrae55_2017(), n = 90)
    )$data
    expect_gt(nrow(ashrae[[1L]]), 0L)
    expect_gt(length(unique(round(ashrae[[2L]]$x[ashrae[[2L]]$level == -0.5], 4))), 1L)
    expect_true("COMFORT" %in% unique(unlist(lapply(ashrae, `[[`, "label"))))

    en <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_standard_zone(comfort_standard_en15251_2007(), n = 90)
    )$data
    expect_equal(length(en), 6L)
    expect_equal(
        vapply(en[1:3], function(x) unique(x$level_low), numeric(1L)),
        c(-0.7, -0.2, 0.2),
        tolerance = 1e-8
    )
    expect_true(all(c("PMV -0.7", "PMV -0.2", "PMV +0.2", "PMV +0.7") %in%
        unique(en[[5L]]$label)))

    ip <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140), units = "IP") +
            geom_comfort_pmv_lines(levels = c(-0.5, 0.5), n = 60)
    )
    expect_gt(nrow(ip$data[[1L]]), 0L)

    mollier <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20), mollier = TRUE) +
            geom_comfort_standard_zone(comfort_standard_ashrae55_2017(), n = 60)
    )
    expect_gt(nrow(mollier$data[[1L]]), 0L)
    expect_true(all(mollier$data[[1L]]$x <= 0.02))

    ip_overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140), units = "IP") +
            geom_comfort_overlay(n = c(40, 24))
    )
    expect_gt(nrow(ip_overlay$data[[1L]]), 0L)

    mollier_overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20), mollier = TRUE) +
            geom_comfort_overlay(n = c(40, 24))
    )
    expect_gt(nrow(mollier_overlay$data[[1L]]), 0L)
    expect_true(all(mollier_overlay$data[[1L]]$x <= 0.02))
})

test_that("Marsh-style comfort overlays have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    base <- ggpsychro(tdb_lim = c(5, 35), hum_lim = c(0, 24)) +
        psychro_preset("minimal")
    pmv_base <- ggpsychro(tdb_lim = c(5, 40), hum_lim = c(0, 24)) +
        psychro_preset("minimal")

    vdiffr::expect_doppelganger(
        "comfort pmv marsh lines",
        pmv_base +
            geom_comfort_overlay(n = c(70, 48), gap = 0) +
            scale_fill_comfort_pmv() +
            geom_comfort_pmv_lines(levels = seq(-3, 3, by = 0.5), n = 140)
    )

    vdiffr::expect_doppelganger(
        "comfort ashrae55 2017 pmv zone",
        base +
            geom_comfort_standard_zone(comfort_standard_ashrae55_2017(), n = 140)
    )

    vdiffr::expect_doppelganger(
        "comfort en15251 2007 pmv zones",
        base +
            geom_comfort_standard_zone(comfort_standard_en15251_2007(), n = 140)
    )
})

test_that("comfort layer internals are not exported", {
    expect_false(any(grepl("^StatComfort", getNamespaceExports("ggpsychro"))))
})

test_that("comfort zones and state stats build model-specific fields", {
    zone <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_zone(model = comfort_model_adaptive(t_running = 20))
    )$data[[1L]]
    expect_equal(range(zone$x), c(20.5, 27.5), tolerance = 1e-8)
    expect_equal(range(zone$y), c(0, 0.02), tolerance = 1e-8)

    d <- data.frame(tdb = c(24, 26), relhum = c(50, 60))
    pmv_state <- ggplot2::ggplot_build(
        ggpsychro(d) +
            stat_comfort_state(ggplot2::aes(tdb = tdb, relhum = relhum))
    )$data[[1L]]
    expect_true(all(c("pmv", "ppd") %in% names(pmv_state)))

    set_state <- ggplot2::ggplot_build(
        ggpsychro(d) +
            stat_comfort_state(
                ggplot2::aes(tdb = tdb, relhum = relhum),
                model = comfort_model_set()
            )
    )$data[[1L]]
    expect_true("set" %in% names(set_state))

    adaptive_state <- ggplot2::ggplot_build(
        ggpsychro(d) +
            stat_comfort_state(
                ggplot2::aes(tdb = tdb, relhum = relhum),
                model = comfort_model_adaptive(t_running = 20)
            )
    )$data[[1L]]
    expect_true("acceptability" %in% names(adaptive_state))
})
