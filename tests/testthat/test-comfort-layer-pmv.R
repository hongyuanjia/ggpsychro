# Focused comfort calculation, model, and layer behavior tests.

test_that("PMV root-traced contours solve requested levels", {
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )
    model <- comfort_model_pmv()

    humratio <- seq(0, 0.02, length.out = 40)
    native_roots <- pmv__curve_roots(
        model,
        -0.5,
        humratio,
        c(15, 30),
        "SI",
        pressure
    )
    fallback_roots <- pmv__curve_roots_r(
        model,
        -0.5,
        humratio,
        c(15, 30),
        "SI",
        pressure
    )
    expect_equal(native_roots, fallback_roots, tolerance = 1e-7)

    native_sat_roots <- pmv__curve_saturation_roots(
        model,
        0,
        c(0, 35),
        c(0, 35),
        "SI",
        pressure,
        120
    )
    fallback_sat_roots <- pmv__curve_saturation_roots_r(
        model,
        0,
        c(0, 35),
        c(0, 35),
        "SI",
        pressure,
        120
    )
    expect_equal(native_sat_roots, fallback_sat_roots, tolerance = 1e-7)

    curves <- pmv__curve_data(
        comfort_model_pmv(),
        c(-0.5, 0, 0.5),
        96,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20),
        label = "none"
    )
    expect_gt(nrow(curves), 0L)
    expect_true(all(vapply(
        split(curves$humratio, curves$level),
        function(x) all(diff(x) >= 0),
        logical(1L)
    )))

    rh <- comfort_dispatch__relhum_from_humratio(
        curves$tdb,
        curves$humratio,
        "SI",
        pressure
    )
    pmv <- comfort_pmv(
        curves$tdb,
        rh = rh,
        limit_inputs = FALSE,
        round_output = FALSE
    )$pmv
    expect_lt(max(abs(pmv - curves$level), na.rm = TRUE), 0.02)

    curve_cache <- new.env(parent = emptyenv())
    cached_curves <- pmv__curve_data(
        comfort_model_pmv(),
        c(-0.5, 0, 0.5),
        96,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20),
        label = "none",
        curve_cache = curve_cache
    )
    cached_axis <- pmv__axis_label_data(
        comfort_model_pmv(),
        c(-0.5, 0, 0.5),
        96,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20),
        curve_cache = curve_cache
    )
    expect_equal(cached_curves, curves, tolerance = 1e-8)
    expect_gt(nrow(cached_axis), 0L)
    expect_equal(length(ls(curve_cache)), 3L)

    saturated <- pmv__curve_data(
        comfort_model_pmv(),
        c(-1, 0, 1),
        120,
        "SI",
        pressure,
        FALSE,
        c(0, 35),
        c(0, 35),
        label = "none"
    )
    sat_exists <- vapply(
        c(-1, 0, 1),
        function(level) {
            length(
                pmv__curve_saturation_roots(
                    comfort_model_pmv(),
                    level,
                    c(0, 35),
                    c(0, 35),
                    "SI",
                    pressure,
                    120
                )$tdb
            ) >
                0L
        },
        logical(1L)
    )
    reaches_saturation <- vapply(
        split(saturated, saturated$level),
        function(x) {
            sat <- zone__saturation_humratio(x$tdb, "SI", pressure)
            min(abs(x$humratio - sat), na.rm = TRUE) < 1e-8
        },
        logical(1L)
    )
    expect_true(all(reaches_saturation[sat_exists]))

    labels <- pmv__curve_data(
        comfort_model_pmv(),
        -3:3,
        120,
        "SI",
        pressure,
        FALSE,
        c(5, 40),
        c(0, 24),
        label = "sensation"
    )
    expect_true(all(
        c(
            "COLD",
            "COOL",
            "SLIGHTLY COOL",
            "NEUTRAL",
            "SLIGHTLY WARM",
            "WARM",
            "HOT"
        ) %in%
            unique(labels$label)
    ))
    expect_equal(unique(labels$linetype[labels$level == 0]), "dashed")
    expect_equal(unique(labels$vjust), 0.5)

    rootband <- pmv__root_band_data(
        comfort_model_pmv(),
        NULL,
        c(-0.5, 0, 0.5),
        c(120, 60),
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20)
    )
    for (level in c(-0.5, 0, 0.5)) {
        edge <- rootband[
            rootband$edge_level == level &
                is.finite(rootband$edge_level),
            ,
            drop = FALSE
        ]
        rh <- comfort_dispatch__relhum_from_humratio(
            edge$tdb,
            edge$humratio,
            "SI",
            pressure
        )
        pmv <- comfort_pmv(
            edge$tdb,
            rh = rh,
            limit_inputs = FALSE,
            round_output = FALSE
        )$pmv
        expect_lt(max(abs(pmv - level), na.rm = TRUE), 0.02)
    }

    marsh_rootband <- pmv__root_band_data(
        comfort_model_pmv(),
        NULL,
        NULL,
        c(70, 48),
        "SI",
        pressure,
        FALSE,
        c(5, 40),
        c(0, 24)
    )
    cap_points <- data.frame(
        x = c(18.05063, 18.16456, 18.16456, 18.16456),
        y = c(0.01292650, 0.01302172, 0.01295477, 0.01288782)
    )
    cap_polys <- split(marsh_rootband, marsh_rootband$group)
    cap_covered <- vapply(
        seq_len(nrow(cap_points)),
        function(i) {
            any(vapply(
                cap_polys,
                function(poly) {
                    util__inside_polygon(
                        cap_points$x[[i]],
                        cap_points$y[[i]],
                        poly$x,
                        poly$y
                    )
                },
                logical(1L)
            ))
        },
        logical(1L)
    )
    expect_true(all(cap_covered))

    standard_band <- pmv__band_data(
        comfort_model_pmv(),
        c(-0.5, 0.5),
        c(140, 90),
        "SI",
        pressure,
        FALSE,
        c(5, 35),
        c(0, 24)
    )
    top <- standard_band[which.max(standard_band$humratio), , drop = FALSE]
    sat <- zone__saturation_humratio(top$tdb, "SI", pressure)
    expect_lt(abs(top$humratio - sat), 1e-6)

    rootband_cache <- new.env(parent = emptyenv())
    uncached_first_band <- pmv__band_data(
        comfort_model_pmv(),
        c(-0.5, 0),
        c(140, 90),
        "SI",
        pressure,
        FALSE,
        c(5, 35),
        c(0, 24)
    )
    cached_standard_band <- pmv__band_data(
        comfort_model_pmv(),
        c(-0.5, 0),
        c(140, 90),
        "SI",
        pressure,
        FALSE,
        c(5, 35),
        c(0, 24),
        rootband_cache = rootband_cache
    )
    cached_adjacent_band <- pmv__band_data(
        comfort_model_pmv(),
        c(0, 0.5),
        c(140, 90),
        "SI",
        pressure,
        FALSE,
        c(5, 35),
        c(0, 24),
        rootband_cache = rootband_cache
    )
    expect_equal(cached_standard_band, uncached_first_band, tolerance = 1e-8)
    expect_gt(nrow(cached_adjacent_band), 0L)
    expect_gt(length(ls(rootband_cache)), 2L)
})
test_that("PMV comfort lines and PMV-based standard zones build", {
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )

    pmv_lines <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv(
                bands = FALSE,
                contour_levels = c(-1, 0, 1),
                n = 80
            )
    )$data
    expect_equal(length(pmv_lines), 2L)
    expect_true(all(c("level", "linetype") %in% names(pmv_lines[[1L]])))
    expect_equal(sort(unique(pmv_lines[[1L]]$level)), c(-1, 0, 1))
    expect_equal(
        unique(pmv_lines[[1L]]$linetype[pmv_lines[[1L]]$level == 0]),
        "dashed"
    )
    expect_true(all(
        c("-1.0", "0.0", "+1.0") %in% unique(pmv_lines[[2L]]$label)
    ))
    expect_true(all(table(pmv_lines[[2L]]$group) >= 2L))

    expect_error(
        geom_comfort_pmv(bands = FALSE, contours = FALSE),
        "At least one of `bands`, `contours`, or `standard`"
    )
    expect_error(
        geom_comfort_set(bands = FALSE, contours = FALSE),
        "At least one of `bands` or `contours`"
    )
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_comfort_pmv(n = 24.5)
        ),
        "`n`"
    )
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_comfort_pmv(band_levels = 3.5, contours = FALSE)
        ),
        "`band_levels`"
    )
    axis_y <- vapply(
        split(pmv_lines[[2L]]$y, pmv_lines[[2L]]$group),
        min,
        numeric(1L)
    )
    expect_equal(length(unique(round(axis_y, 6))), 1L)
    expect_gt(min(axis_y), 0.0003)

    axis_default <- pmv__axis_label_data(
        comfort_model_pmv(),
        c(-1, 0, 1),
        80,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20)
    )
    expect_equal(unique(axis_default$hjust), 0.95)
    expect_equal(unique(axis_default$vjust), 0.5)
    axis_vjust <- pmv__axis_label_text_vjust(ggplot2::waiver())
    expect_s3_class(axis_vjust, "unit")
    expect_equal(as.numeric(axis_vjust), 3.5)
    axis_large_vjust <- pmv__axis_label_text_vjust(ggplot2::waiver(), 6)
    expect_gt(as.numeric(axis_large_vjust), as.numeric(axis_vjust))

    axis_labels <- pmv__axis_label_data(
        comfort_model_pmv(),
        c(-1, 0, 1),
        80,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20),
        axis_label_hjust = 0.015
    )
    expect_true(all(table(axis_labels$group) >= 2L))
    axis_hum <- vapply(
        split(axis_labels$humratio, axis_labels$group),
        min,
        numeric(1L)
    )
    expect_equal(length(unique(round(axis_hum, 8))), 1L)
    expect_equal(unique(axis_labels$hjust), 0.985)
    expect_equal(unique(axis_labels$vjust), 0.5)

    pmv_boundary <- pmv__curve_data(
        comfort_model_pmv(),
        c(-0.5, 0.5),
        80,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20),
        label = "boundary"
    )
    expect_equal(unique(pmv_boundary$vjust[pmv_boundary$level < 0]), -0.25)
    expect_equal(unique(pmv_boundary$vjust[pmv_boundary$level > 0]), 1.25)

    pmv_boundary_mollier <- pmv__curve_data(
        comfort_model_pmv(),
        c(-0.5, 0.5),
        80,
        "SI",
        pressure,
        TRUE,
        c(15, 30),
        c(0, 20),
        label = "boundary"
    )
    expect_equal(
        unique(pmv_boundary_mollier$vjust[
            pmv_boundary_mollier$level < 0
        ]),
        1.25
    )
    expect_equal(
        unique(pmv_boundary_mollier$vjust[
            pmv_boundary_mollier$level > 0
        ]),
        -0.25
    )

    pmv_sensation <- pmv__curve_data(
        comfort_model_pmv(),
        c(-1, 0, 1),
        80,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 20),
        label = "sensation"
    )
    expect_equal(unique(pmv_sensation$vjust), 0.5)

    ashrae <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv(
                standard = comfort_pmv_ashrae55(),
                bands = FALSE,
                contours = FALSE,
                n = 90
            )
    )$data
    expect_gt(nrow(ashrae[[1L]]), 0L)
    expect_gt(
        length(unique(round(ashrae[[2L]]$x[ashrae[[2L]]$level == -0.5], 4))),
        1L
    )
    expect_true("COMFORT" %in% unique(unlist(lapply(ashrae, `[[`, "label"))))

    ashrae_alpha <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv(
                standard = comfort_pmv_ashrae55(),
                bands = FALSE,
                contours = FALSE,
                n = 90,
                alpha = 0.2
            )
    )$data
    expect_equal(unique(ashrae_alpha[[1L]]$alpha), 0.2)
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_comfort_pmv(
                    standard = comfort_pmv_ashrae55(),
                    bands = FALSE,
                    contours = FALSE,
                    alpha = NA_real_
                )
        ),
        "alpha"
    )

    en <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv(
                standard = comfort_pmv_en15251(),
                bands = FALSE,
                contours = FALSE,
                n = 90
            )
    )$data
    expect_equal(length(en), 6L)
    expect_equal(
        vapply(en[1:3], function(x) unique(x$level_low), numeric(1L)),
        c(-0.7, -0.2, 0.2),
        tolerance = 1e-8
    )
    expect_true(all(
        c("PMV -0.7", "PMV -0.2", "PMV +0.2", "PMV +0.7") %in%
            unique(en[[5L]]$label)
    ))

    ip <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140), units = "IP") +
            geom_comfort_pmv(
                bands = FALSE,
                contour_levels = c(-0.5, 0.5),
                n = 60
            )
    )
    expect_gt(nrow(first_built_data(ip)), 0L)

    ip_overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140), units = "IP") +
            comfort_layer__bands(n = c(40, 24)) +
            scale_fill_comfort_pmv()
    )
    expect_gt(nrow(first_built_data(ip_overlay)), 0L)
    expect_gt(length(unique(first_built_data(ip_overlay)$fill)), 1L)

    ip_heat <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(70, 115), hum_lim = c(0, 220), units = "IP") +
            geom_comfort_heat_index(n = c(32, 20))
    )
    expect_true(all(vapply(ip_heat$data[1:4], nrow, integer(1L)) > 0L))

    ip_givoni <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(40, 115), hum_lim = c(0, 220), units = "IP") +
            geom_comfort_givoni(comfort_strategy_givoni(66.2, units = "IP"))
    )
    expect_gte(length(ip_givoni$data), 13L)
})
