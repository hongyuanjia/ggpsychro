# Focused psychrometric chart, coordinate, guide, and stat tests.

test_that("Psychrometric grids and stat layers are clipped to the valid panel", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50))
    grobs <- collect_grobs(ggplot2::ggplotGrob(p))
    names <- vapply(grobs, function(grob) grob$name %||% "", character(1))
    grid_grobs <- grobs[
        grepl("^(panel.grid|psychro.panel.grid\\.(minor|major))", names) &
            !grepl("saturation", names)
    ]

    expect_gt(length(grid_grobs), 0L)
    expect_true(all(vapply(grid_grobs, inherits, logical(1L), "polyclipgrob")))

    psychro_text <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        stat_vappres(
            ggplot2::aes(x = x, vappres = vappres, label = label),
            data = data.frame(
                x = c(20, 20),
                vappres = c(1000, 10000),
                label = c("in", "out")
            ),
            geom = "text"
        )
    built <- ggplot2::ggplot_build(psychro_text)
    filtered <- coord_clip__filter_data_to_panel(
        first_built_data(built),
        built$layout$panel_params[[1L]],
        built$layout$coord
    )

    expect_equal(filtered$label, "in")
})
test_that("Ordinary text can render in the psychrometric mask area", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        ggplot2::geom_text(
            ggplot2::aes(x = 5, y = 45, label = "mask area text"),
            inherit.aes = FALSE
        )
    labels <- unlist(lapply(
        collect_grobs(ggplot2::ggplotGrob(p)),
        function(grob) {
            if (is.null(grob$label)) {
                return(character())
            }
            as.character(grob$label)
        }
    ))

    expect_true("mask area text" %in% labels)

    testthat::skip_on_os(c("linux", "windows"))
    vdiffr::expect_doppelganger("text in mask area", p)
})
test_that("Saturation is drawn between psychro boundaries and markers", {
    process <- data.frame(tdb = c(20, 30), relhum = c(50, 70))
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
        geom_psychro_process(
            ggplot2::aes(tdb = tdb, relhum = relhum),
            data = process
        ) +
        stat_psychro_state(
            ggplot2::aes(tdb = tdb, relhum = relhum),
            data = process
        )
    panel <- panel_grob(p)
    process_index <- panel_child_index(p, "GRID.polyline")
    saturation_index <- panel_child_index(p, "psychro.panel.grid.saturation")
    point_index <- panel_child_index(p, "geom_point")

    expect_length(process_index, 1L)
    expect_length(saturation_index, 1L)
    expect_length(point_index, 1L)
    expect_gt(saturation_index, process_index)
    expect_gt(point_index, saturation_index)

    saturated_state <- ggpsychro(tdb_lim = c(5, 40), hum_lim = c(0, 24)) +
        stat_psychro_state(ggplot2::aes(tdb = c(15.6), relhum = 100))
    expect_gt(
        panel_child_index(saturated_state, "geom_point"),
        panel_child_index(saturated_state, "psychro.panel.grid.saturation")
    )

    heat_index <- panel_grob(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_heat_index(n = c(32, 24))
    )
    heat_fg <- heat_index$children[[length(heat_index$children)]]
    expect_gt(
        length(find_named_grobs_in(heat_fg, "psychro.panel.grid.saturation")),
        0L
    )
    expect_gt(
        length(find_named_grobs_in(heat_fg, "psychro-heat-index-labels")),
        0L
    )

    givoni <- panel_grob(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(comfort_strategy_givoni(
                variant = "adaptive",
                mean_outdoor = 19
            ))
    )
    givoni_fg <- givoni$children[[length(givoni$children)]]
    expect_gt(
        length(find_named_grobs_in(givoni_fg, "psychro.panel.grid.saturation")),
        0L
    )
    expect_gt(length(find_named_grobs_in(givoni_fg, "GRID.lines")), 0L)
})
test_that("coord_psychro keeps saturation sampling internal", {
    expect_false("n" %in% names(formals(coord_psychro)))
    expect_type(coord_psy__saturation_n(), "integer")
    expect_gte(coord_psy__saturation_n(), 2L)
})
test_that("Coordinate range helpers clip expanded ranges in native units", {
    p <- ggpsychro(tdb_lim = c(-50, 100), hum_lim = c(0, 60)) +
        coord_psychro(
            tdb_lim = c(-50, 100),
            hum_lim = c(0, 60),
            expand = TRUE
        )
    built <- ggplot2::ggplot_build(p)
    coord <- built$layout$coord
    panel_params <- built$layout$panel_params[[1L]]
    tdb_domain <- psychro__tdb_limits(coord$units)
    hum_domain <- unit__hum_from_chart(
        psychro__hum_limits(coord$units),
        coord$units
    )
    tdb_uncut <- coord$range_tdb(panel_params, cut = FALSE)
    hum_uncut <- coord$range_hum(panel_params, cut = FALSE)

    expect_true(tdb_uncut[[1L]] < tdb_domain[[1L]])
    expect_true(tdb_uncut[[2L]] > tdb_domain[[2L]])
    expect_true(hum_uncut[[1L]] < hum_domain[[1L]])
    expect_true(hum_uncut[[2L]] > hum_domain[[2L]])
    expect_equal(coord$range_tdb(panel_params, cut = TRUE), tdb_domain)
    expect_equal(coord$range_hum(panel_params, cut = TRUE), hum_domain)
})
test_that("Coordinate calculations inverse custom position transforms before psychrolib", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(1, 50)) +
        geom_psychro_grid_relhum() +
        scale_humratio_continuous(transform = "log10")

    built <- ggplot2::ggplot_build(p)
    coord <- built$layout$coord
    panel_params <- built$layout$panel_params[[1L]]
    hum_scale <- panel_params[[coord$pos_hum()]]$scale

    expect_equal(
        coord$range_hum_physical(panel_params),
        c(0.001, 0.05),
        tolerance = 1e-8
    )

    sat <- coord_psy__saturation_scaled(coord, panel_params)
    sat_hum <- unit__hum_from_chart(
        hum_scale$trans$inverse(sat$hum),
        coord$units
    )

    expect_true(all(is.finite(sat_hum)))
    expect_lte(max(sat_hum), 0.05 + 1e-8)
    expect_gt(max(sat_hum), 0.04)
    expect_no_error(ggplot2::ggplotGrob(p))
})
test_that("Psychrolib calculations inverse custom psychrometric scale transforms", {
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )

    wetbulb_plot <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        stat_wetbulb(
            ggplot2::aes(x = tdb, wetbulb = wetbulb),
            data = data.frame(tdb = 25, wetbulb = 20)
        ) +
        scale_wetbulb_continuous(transform = "log10")
    expect_equal(
        first_built_data(ggplot2::ggplot_build(wetbulb_plot))$y,
        psychrolib__with_units(
            "SI",
            psychrolib::GetHumRatioFromTWetBulb(25, 20, pressure)
        ),
        tolerance = 1e-8
    )

    state_plot <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        stat_psychro_state(
            ggplot2::aes(tdb = tdb, relhum = relhum),
            data = data.frame(tdb = 25, relhum = 50)
        ) +
        scale_relhum_continuous(transform = "identity")
    expect_equal(
        first_built_data(ggplot2::ggplot_build(state_plot))$y,
        psychrolib__with_units(
            "SI",
            psychrolib::GetHumRatioFromRelHum(25, 0.5, pressure)
        ),
        tolerance = 1e-8
    )

    grid_break_values <- function(plot, type) {
        built <- ggplot2::ggplot_build(plot)
        coord <- built$layout$coord
        panel_params <- built$layout$panel_params[[1L]]
        scale <- panel_params[[coord$pos_tdb()]]$scale
        limits <- scale$trans$inverse(
            panel_params[[coord$pos_tdb()]]$continuous_range
        )
        tdb <- scale$trans$breaks(limits, 100L)
        grid <- coord_psy__grid_lines(
            coord,
            panel_params,
            tdb,
            coord$range_tdb(panel_params),
            coord$range_hum(panel_params)
        )
        unique(grid[[type]]$major$value)
    }

    expect_equal(
        grid_break_values(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_wetbulb() +
                scale_wetbulb_continuous(
                    transform = "log10",
                    breaks = c(10, 20, 30)
                ),
            "wetbulb"
        ),
        c(10, 20, 30),
        tolerance = 1e-8
    )
    expect_equal(
        grid_break_values(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_vappres() +
                scale_vappres_continuous(
                    transform = "log10",
                    breaks = c(1000, 2000, 3000)
                ),
            "vappres"
        ),
        c(1000, 2000, 3000),
        tolerance = 1e-8
    )
    expect_equal(
        grid_break_values(
            ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
                geom_psychro_grid_relhum() +
                scale_relhum_continuous(
                    transform = "log10",
                    breaks = c(25, 50, 75)
                ),
            "relhum"
        ),
        c(0.25, 0.50, 0.75),
        tolerance = 1e-8
    )
})
test_that("Generated comfort stats return custom position scale coordinates", {
    tile_plot <- ggpsychro(tdb_lim = c(15, 35), hum_lim = c(1, 24)) +
        comfort_layer__bands(band_render = "tile", n = c(10, 8), gap = 0) +
        scale_humratio_continuous(transform = "log10")
    tile <- first_built_data(ggplot2::ggplot_build(tile_plot))
    hum_edges <- seq(1, 24, length.out = 9) / 1000
    hum_centers <- (hum_edges[-length(hum_edges)] + hum_edges[-1L]) / 2
    hum_height <- diff(log10(hum_edges * 1000))
    hum_index <- match(round(tile$humratio, 12L), round(hum_centers, 12L))

    expect_equal(tile$y, log10(tile$humratio * 1000), tolerance = 1e-8)
    expect_equal(tile$ymax - tile$ymin, hum_height[hum_index], tolerance = 1e-8)

    pmv_hum_plot <- ggpsychro(tdb_lim = c(15, 35), hum_lim = c(1, 24)) +
        geom_comfort_pmv(bands = FALSE, contour_levels = 0, n = 40) +
        scale_humratio_continuous(transform = "log10")
    pmv_hum <- first_built_data(ggplot2::ggplot_build(pmv_hum_plot))
    expect_equal(pmv_hum$y, log10(pmv_hum$humratio * 1000), tolerance = 1e-8)

    pmv_tdb_plot <- ggpsychro(tdb_lim = c(15, 35), hum_lim = c(1, 24)) +
        geom_comfort_pmv(bands = FALSE, contour_levels = 0, n = 40) +
        scale_drybulb_continuous(transform = "log10")
    pmv_tdb <- first_built_data(ggplot2::ggplot_build(pmv_tdb_plot))
    expect_equal(pmv_tdb$x, log10(pmv_tdb$tdb), tolerance = 1e-8)
})
test_that("building ggpsychro plots does not mutate source plot state", {
    p <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
        geom_psychro_tile(
            ggplot2::aes(x, y),
            data = data.frame(x = 20, y = 10)
        )

    # Guard against build-time psychro metadata leaking back onto the user-held plot.
    stat_param_names <- names(p@layers[[1L]]$stat_params)
    expect_null(p@layers[[1L]]$geom_params$psychro.theme)
    expect_null(p@coordinates$pressure)

    invisible(ggplot2::ggplot_build(p))

    expect_equal(names(p@layers[[1L]]$stat_params), stat_param_names)
    expect_false(any(
        c("units", "pres", "mollier", "tdb_lim", "hum_lim") %in%
            names(p@layers[[1L]]$stat_params)
    ))
    expect_null(p@coordinates$pressure)

    invisible(ggplot2::ggplotGrob(p))

    expect_equal(names(p@layers[[1L]]$stat_params), stat_param_names)
    expect_null(p@layers[[1L]]$geom_params$psychro.theme)
    expect_null(p@coordinates$pressure)
})
test_that("rebuilt plots do not reuse stale inherited psychro params", {
    d <- data.frame(tdb = 77, relhum = 50)
    p <- ggpsychro(d, tdb_lim = c(50, 100), hum_lim = c(0, 60)) +
        stat_psychro_state(ggplot2::aes(tdb = tdb, relhum = relhum))

    invisible(ggplot2::ggplot_build(p))
    suppressMessages(
        rebuilt <- p +
            coord_psychro(
                tdb_lim = c(50, 100),
                hum_lim = c(0, 140),
                units = "IP"
            )
    )
    fresh <- ggpsychro(
        d,
        tdb_lim = c(50, 100),
        hum_lim = c(0, 140),
        units = "IP"
    ) +
        stat_psychro_state(ggplot2::aes(tdb = tdb, relhum = relhum))

    rebuilt_data <- ggplot2::ggplot_build(rebuilt)$data[[1L]]
    fresh_data <- ggplot2::ggplot_build(fresh)$data[[1L]]

    expect_null(p@layers[[1L]]$stat_params$units)
    expect_equal(rebuilt_data$y, fresh_data$y, tolerance = 1e-8)
})
test_that("Enthalpy stat creates y output without an explicit y aesthetic", {
    d <- data.frame(
        dry_bulb_temperature = c(18, 24, 30),
        enthalpy = c(35000, 50000, 65000)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
            stat_enthalpy(
                ggplot2::aes(x = dry_bulb_temperature, enthalpy = enthalpy),
                data = d
            )
    )
    expected <- psychrolib__with_units(
        "SI",
        GetHumRatioFromEnthalpyAndTDryBulb(d$enthalpy, d$dry_bulb_temperature)
    )

    expect_equal(first_built_data(built)$y, expected, tolerance = 1e-8)
    expect_true(all(is.finite(first_built_data(built)$y)))
})
