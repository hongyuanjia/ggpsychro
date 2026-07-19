# Focused comfort calculation, model, and layer behavior tests.

test_that("comfort overlay and contour build on psychrometric panel grids", {
    overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(n = c(24, 16), gap = 0) +
            scale_fill_comfort_pmv()
    ))
    expect_gt(nrow(overlay), 0L)
    expect_true(all(is.finite(overlay$value)))
    expect_true(all(overlay$y >= 0))
    expect_true(all(
        c("level_low", "level_high", "edge_level") %in% names(overlay)
    ))
    expect_false("width" %in% names(overlay))
    expect_equal(unique(overlay$alpha), 0.55)
    overlay_breaks <- sort(unique(c(
        overlay$level_low[is.finite(overlay$level_low)],
        overlay$level_high[is.finite(overlay$level_high)]
    )))
    expect_true(all(abs(diff(overlay_breaks) - 0.25) < 1e-8))

    pmv_layers <- built_data_layers(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv(
                contour_levels = c(-1, 0, 1),
                n = c(24, 16)
            )
    ))
    expect_gte(length(pmv_layers), 3L)
    expect_gt(nrow(pmv_layers[[1L]]), 0L)

    overlay_alpha <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(n = c(24, 16), alpha = 0.35)
    ))
    expect_equal(unique(overlay_alpha$alpha), 0.35)

    isoband <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(band_method = "isoband", n = c(24, 16))
    ))
    expect_gt(nrow(isoband), 0L)
    expect_true("level_mid" %in% names(isoband))
    expect_equal(unique(isoband$alpha), 0.55)

    tile <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(band_render = "tile", n = c(24, 16))
    ))
    expect_gt(nrow(tile), 0L)
    expect_equal(unique(tile$alpha), 0.55)

    pmv_tile <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_pmv(
                band_render = "tile",
                contours = FALSE,
                n = c(24, 16)
            )
    ))
    expect_gt(nrow(pmv_tile), 0L)
    expect_equal(unique(pmv_tile$alpha), 0.55)

    set_overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(model = comfort_model_set(), n = c(24, 16))
    ))
    expect_gt(nrow(set_overlay), 0L)
    expect_equal(unique(set_overlay$alpha), 0.55)
    expect_error(
        comfort_layer__bands(
            model = comfort_model_set(),
            band_method = "root"
        ),
        "`band_method = \"root\"` is only available for PMV"
    )

    set_layers <- built_data_layers(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_set(
                contours = TRUE,
                contour_levels = c(22, 24, 26),
                labels = TRUE,
                n = c(24, 16)
            )
    ))
    expect_equal(length(set_layers), 2L)
    expect_gt(nrow(set_layers[[1L]]), 0L)
    expect_true("label" %in% names(set_layers[[2L]]))

    set_tile <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_set(band_render = "tile", n = c(24, 16))
    ))
    expect_gt(nrow(set_tile), 0L)
    expect_equal(unique(set_tile$alpha), 0.55)

    adaptive_overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(
                model = comfort_model_adaptive(t_running = 20),
                n = c(24, 16)
            )
    ))
    expect_gt(nrow(adaptive_overlay), 0L)
    expect_equal(unique(adaptive_overlay$alpha), 0.55)

    adaptive_zone <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_adaptive(t_running = 20)
    ))
    expect_gt(nrow(adaptive_zone), 0L)
    expect_equal(unique(adaptive_zone$alpha), 0.3)
    # Default adaptive polygon bands should reuse the analytic zone boundary
    # rather than a grid-interpolated binary acceptability boundary.
    expect_equal(
        range(adaptive_overlay$tdb, finite = TRUE),
        range(adaptive_zone$tdb, finite = TRUE)
    )
    expect_equal(
        range(adaptive_overlay$humratio, finite = TRUE),
        range(adaptive_zone$humratio, finite = TRUE)
    )

    heat_overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            comfort_layer__bands(
                model = comfort_model_heat_index(),
                n = c(32, 24)
            )
    ))
    expect_gt(nrow(heat_overlay), 0L)
    expect_true("heat_index" %in% unique(heat_overlay$metric))
    expect_equal(unique(heat_overlay$alpha), 0.55)

    heat_index <- built_data_layers(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_heat_index(n = c(32, 24), alpha = 0.4)
    ))
    expect_equal(length(heat_index), 6L)
    expect_true(all(vapply(heat_index[1:4], nrow, integer(1L)) > 0L))
    expect_equal(unique(heat_index[[1L]]$alpha), 0.4)
    expect_true(all(
        c("CAUTION", "EXTREME CAUTION", "DANGER", "EXTREME DANGER") %in%
            unique(heat_index[[6L]]$label)
    ))
    expect_equal(unique(heat_index[[6L]]$alpha), 0)

    heat_index_no_labels <- built_data_layers(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_heat_index(
                n = c(32, 24),
                alpha = 0.4,
                labels = FALSE
            )
    ))
    expect_equal(length(heat_index_no_labels), 5L)
    heat_index_no_label_grobs <- collect_grobs(ggplot2::ggplotGrob(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_heat_index(
                n = c(32, 24),
                alpha = 0.4,
                labels = FALSE
            )
    ))
    expect_false(any(vapply(
        heat_index_no_label_grobs,
        function(grob) {
            identical(grob$name, "psychro-heat-index-labels")
        },
        logical(1L)
    )))

    heat_index_grobs <- collect_grobs(ggplot2::ggplotGrob(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_heat_index(n = c(32, 24), alpha = 0.4)
    ))
    expect_true(any(vapply(
        heat_index_grobs,
        function(grob) {
            identical(grob$name, "psychro-heat-index-labels")
        },
        logical(1L)
    )))

    tile_alpha <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__bands(
                band_render = "tile",
                n = c(24, 16),
                alpha = 0.35
            )
    ))
    expect_equal(unique(tile_alpha$alpha), 0.35)

    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )
    grid <- comfort_grid__data(
        comfort_model_pmv(),
        NULL,
        c(16, 16),
        0,
        "SI",
        pressure,
        FALSE,
        c(15, 30),
        c(0, 35)
    )
    saturation_left <- zone__saturation_humratio(
        grid$tdb - grid$width / 2,
        "SI",
        pressure
    )
    saturation_right <- zone__saturation_humratio(
        grid$tdb + grid$width / 2,
        "SI",
        pressure
    )
    expect_true(all(
        grid$humratio - grid$height / 2 <=
            pmax(saturation_left, saturation_right) + 1e-8
    ))

    contour <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__contour(n = c(32, 20), breaks = c(-1, 0, 1))
    ))
    expect_gt(nrow(contour), 0L)
    expect_true(all(contour$level %in% c(-1, 0, 1)))

    contour_labelled <- built_data_layers(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(32, 20),
                label = TRUE
            )
    ))
    expect_equal(length(contour_labelled), 1L)
    expect_true(all(c("22", "24", "26") %in% contour_labelled[[1L]]$label))
    expect_true(all(
        c("label", "level", "value") %in% names(contour_labelled[[1L]])
    ))

    empty_contour <- expect_warning(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(-50, -40), hum_lim = c(0, 5)) +
                comfort_layer__contour(
                    model = comfort_model_set(limit_inputs = TRUE),
                    metric = "set",
                    n = c(8, 8)
                )
        )$data[[1L]],
        NA
    )
    expect_equal(nrow(empty_contour), 0L)

    pmv_labelled <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__contour(
                breaks = c(-1, 0, 1),
                n = c(32, 20),
                label = TRUE
            )
    ))
    expect_true(all(c("-1.0", "0.0", "+1.0") %in% pmv_labelled$label))

    expect_error(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__contour(label = NA),
        "label"
    )
    expect_error(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            comfort_layer__contour(label = TRUE, label_size = -1),
        "label_size"
    )
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_comfort_set(band_levels = 3.5, contours = FALSE)
        ),
        "`band_levels`"
    )
    expect_error(
        geom_comfort_set(band_method = "isoband"),
        "`band_method` is not supported"
    )

    heat_contour <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            comfort_layer__contour(
                model = comfort_model_heat_index(),
                n = c(32, 24)
            )
    ))
    expect_true(all(
        sort(unique(round(heat_contour$level, 6))) %in%
            round(heat_index__thresholds("SI"), 6)
    ))

    heat_zones <- heat_index__zone_data(
        comfort_model_heat_index(),
        NULL,
        c(32, 24),
        "SI",
        pressure,
        FALSE,
        c(20, 45),
        c(0, 35)
    )
    heat_zone_parts <- lapply(
        seq_along(heat_index__zone_specs()),
        function(i) {
            heat_index__zone_data(
                comfort_model_heat_index(),
                i,
                c(32, 24),
                "SI",
                pressure,
                FALSE,
                c(20, 45),
                c(0, 35)
            )
        }
    )
    heat_zone_parts <- heat_zone_parts[
        vapply(heat_zone_parts, nrow, integer(1L)) > 0L
    ]
    heat_zone_parts <- do.call(rbind, heat_zone_parts)
    expect_equal(
        table(heat_zones$category_id),
        table(heat_zone_parts$category_id)
    )
    zone_fills <- vapply(
        heat_index__zone_specs(),
        `[[`,
        character(1L),
        "fill"
    )
    expect_equal(
        as.vector(tapply(heat_zones$fill, heat_zones$category_id, unique)),
        zone_fills[as.integer(names(table(heat_zones$category_id)))]
    )

    heat_cache <- new.env(parent = emptyenv())
    cached_heat_zones <- heat_index__zone_data(
        comfort_model_heat_index(),
        NULL,
        c(32, 24),
        "SI",
        pressure,
        FALSE,
        c(20, 45),
        c(0, 35),
        grid_cache = heat_cache
    )
    cached_heat_contour <- heat_index__contour_data(
        comfort_model_heat_index(),
        c(32, 24),
        "SI",
        pressure,
        FALSE,
        c(20, 45),
        c(0, 35),
        grid_cache = heat_cache
    )
    uncached_heat_contour <- comfort_contour__data(
        comfort_model_heat_index(),
        "heat_index",
        heat_index__thresholds("SI"),
        c(32, 24),
        "SI",
        pressure,
        FALSE,
        c(20, 45),
        c(0, 35)
    )
    expect_gt(nrow(cached_heat_zones), 0L)
    expect_equal(length(ls(heat_cache)), 1L)
    expect_equal(cached_heat_contour, uncached_heat_contour, tolerance = 1e-8)
})
test_that("comfort overlays build in Mollier coordinates", {
    expect_mollier_comfort <- function(plot) {
        layers <- ggplot2::ggplot_build(plot)$data
        expect_gt(length(layers), 0L)
        for (layer in layers) {
            if (!nrow(layer)) {
                next
            }
            expect_true(all(c("x", "y") %in% names(layer)))
            expect_true(all(is.finite(layer$x)))
            expect_true(all(is.finite(layer$y)))
            expect_true(all(layer$x >= -1e-10))
            expect_true(all(layer$x <= 0.02 + 1e-10))
        }
        invisible(layers)
    }

    base <- ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20), mollier = TRUE)

    expect_mollier_comfort(base + comfort_layer__bands(n = c(40, 24)))
    expect_mollier_comfort(
        base + comfort_layer__bands(band_method = "isoband", n = c(32, 20))
    )
    expect_mollier_comfort(
        base + comfort_layer__bands(band_render = "tile", n = c(24, 16))
    )
    expect_mollier_comfort(
        base +
            comfort_layer__bands(
                model = comfort_model_set(),
                n = c(24, 16)
            )
    )
    expect_mollier_comfort(
        base +
            comfort_layer__bands(
                model = comfort_model_adaptive(t_running = 20),
                n = c(24, 16)
            )
    )
    expect_mollier_comfort(
        base +
            comfort_layer__bands(
                model = comfort_model_heat_index(),
                n = c(32, 20)
            )
    )
    expect_mollier_comfort(base + geom_comfort_heat_index(n = c(32, 20)))

    expect_mollier_comfort(
        base +
            comfort_layer__contour(
                breaks = c(-1, 0, 1),
                n = c(32, 20)
            )
    )
    expect_mollier_comfort(
        base +
            comfort_layer__contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(32, 20)
            )
    )
    expect_mollier_comfort(
        base +
            comfort_layer__contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(32, 20),
                label = TRUE
            )
    )

    expect_mollier_comfort(
        base +
            geom_comfort_pmv(
                bands = FALSE,
                contour_levels = c(-1, 0, 1),
                n = 60
            )
    )
    expect_mollier_comfort(
        base +
            geom_comfort_pmv(
                standard = comfort_pmv_ashrae55(),
                bands = FALSE,
                contours = FALSE,
                n = 60
            )
    )
    expect_mollier_comfort(
        base +
            geom_comfort_pmv(
                standard = comfort_pmv_en15251(),
                bands = FALSE,
                contours = FALSE,
                n = 60
            )
    )
    expect_mollier_comfort(base + geom_comfort_givoni())

    expect_mollier_comfort(base + comfort_layer__zone(n = c(60, 40)))
    expect_mollier_comfort(
        base +
            comfort_layer__zone(
                model = comfort_model_set(),
                n = c(32, 20)
            )
    )
    expect_mollier_comfort(
        base +
            comfort_layer__zone(
                model = comfort_model_adaptive(t_running = 20)
            )
    )
})
test_that("comfort overlays have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    base <- ggpsychro(tdb_lim = c(5, 35), hum_lim = c(0, 24)) +
        psychro_preset("minimal")
    pmv_base <- ggpsychro(tdb_lim = c(5, 40), hum_lim = c(0, 24)) +
        psychro_preset("minimal")
    set_base <- ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
        psychro_preset("minimal")
    heat_base <- ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
        psychro_preset("minimal")
    givoni_base <- ggpsychro(tdb_lim = c(-10, 50), hum_lim = c(0, 35)) +
        psychro_preset("minimal")

    vdiffr::expect_doppelganger(
        "comfort pmv marsh lines",
        pmv_base +
            comfort_layer__bands(n = c(70, 48), gap = 0) +
            scale_fill_comfort_pmv() +
            geom_comfort_pmv(
                bands = FALSE,
                contour_levels = seq(-3, 3, by = 0.5),
                n = 140
            )
    )

    vdiffr::expect_doppelganger(
        "comfort ashrae55 2017 pmv zone",
        base +
            geom_comfort_pmv(
                standard = comfort_pmv_ashrae55(),
                bands = FALSE,
                contours = FALSE,
                n = 140
            )
    )

    vdiffr::expect_doppelganger(
        "comfort en15251 2007 pmv zones",
        base +
            geom_comfort_pmv(
                standard = comfort_pmv_en15251(),
                bands = FALSE,
                contours = FALSE,
                n = 140
            )
    )

    vdiffr::expect_doppelganger(
        "comfort set contour labels",
        set_base +
            comfort_layer__contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(70, 42),
                label = TRUE,
                colour = "#4A4A4A",
                linewidth = 0.7
            )
    )

    vdiffr::expect_doppelganger(
        "comfort heat index overlay",
        heat_base +
            geom_comfort_heat_index(n = c(64, 40), alpha = 0.5)
    )

    vdiffr::expect_doppelganger(
        "comfort givoni bioclimatic zones",
        givoni_base +
            geom_comfort_givoni(
                comfort_strategy_givoni(
                    variant = "adaptive",
                    mean_outdoor = 22
                ),
                alpha = 0.45
            )
    )

    vdiffr::expect_doppelganger(
        "comfort givoni styled zones",
        givoni_base +
            geom_comfort_givoni(
                comfort_strategy_givoni(
                    variant = "adaptive",
                    mean_outdoor = 22
                ),
                zone_style = list(
                    comfort = element_givoni_zone(
                        fill = "#66D27A",
                        colour = "#1F5F2D",
                        alpha = 0.35
                    ),
                    natural_ventilation = element_givoni_zone(
                        fill = "#B6E3FF",
                        colour = "#2F6FB0",
                        alpha = 0.25
                    ),
                    winter = element_givoni_zone(
                        colour = "#A14D00",
                        linetype = "dashed"
                    ),
                    air_conditioning = element_givoni_zone(
                        colour = "#8B1E3F",
                        linewidth = 1.2
                    )
                )
            )
    )
})
test_that("Mollier comfort overlays have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    base <- ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20), mollier = TRUE) +
        psychro_preset("minimal")

    vdiffr::expect_doppelganger(
        "comfort mollier pmv overlay",
        base +
            comfort_layer__bands(n = c(50, 30)) +
            scale_fill_comfort_pmv() +
            geom_comfort_pmv(
                bands = FALSE,
                contour_levels = seq(-2, 2, by = 1),
                n = 100
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier set overlay",
        base +
            comfort_layer__bands(model = comfort_model_set(), n = c(40, 24)) +
            comfort_layer__contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(40, 24),
                colour = "#4A4A4A"
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier set contour labels",
        base +
            comfort_layer__contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(70, 42),
                label = TRUE,
                colour = "#4A4A4A",
                linewidth = 0.7
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier adaptive overlay",
        base +
            comfort_layer__bands(
                model = comfort_model_adaptive(t_running = 20),
                n = c(40, 24)
            ) +
            comfort_layer__zone(
                model = comfort_model_adaptive(t_running = 20),
                fill = NA,
                colour = "#4A4A4A"
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier ashrae55 2017 pmv zone",
        base +
            geom_comfort_pmv(
                standard = comfort_pmv_ashrae55(),
                bands = FALSE,
                contours = FALSE,
                n = 100
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier en15251 2007 pmv zones",
        base +
            geom_comfort_pmv(
                standard = comfort_pmv_en15251(),
                bands = FALSE,
                contours = FALSE,
                n = 100
            )
    )
})
test_that("IP comfort overlays have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    base <- ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140), units = "IP") +
        psychro_preset("minimal")

    vdiffr::expect_doppelganger(
        "comfort ip pmv overlay",
        base +
            comfort_layer__bands(n = c(50, 30)) +
            scale_fill_comfort_pmv() +
            geom_comfort_pmv(
                bands = FALSE,
                contour_levels = seq(-2, 2, by = 1),
                n = 100
            )
    )
})
