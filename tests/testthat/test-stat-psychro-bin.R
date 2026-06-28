test_that("psychrometric tile bins relative humidity inputs", {
    d <- data.frame(
        dry_bulb = c(20.1, 20.4, 22.2),
        relative_humidity = c(50, 52, 60)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, relhum = relative_humidity),
                binwidth = c(2, 2),
                gap = 0
            )
    )
    tiles <- built$data[[1L]][order(built$data[[1L]]$x), ]

    expect_equal(tiles$count, c(2, 1))
    expect_equal(tiles$hours, tiles$count)
    expect_equal(tiles$x, c(21, 23), tolerance = 1e-8)
    expect_equal(tiles$y, c(0.007, 0.011), tolerance = 1e-8)
    expect_equal(sum(tiles$count), nrow(d))

    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))
    expected <- with_units(
        "SI",
        psychrolib::GetHumRatioFromRelHum(
            d$dry_bulb,
            d$relative_humidity / 100,
            pressure
        )
    )
    expect_true(all(expected >= min(tiles$ymin)))
    expect_true(all(expected <= max(tiles$ymax)))
})

test_that("psychrometric tile bins direct humidity ratio inputs and summaries", {
    d <- data.frame(
        dry_bulb = c(20.1, 20.8, 22.1),
        humidity_ratio = c(8.1, 8.5, 8.2),
        load = c(1, 3, 5)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(
                    dry_bulb, humidity_ratio,
                    value = load,
                    fill = ggplot2::after_stat(value)
                ),
                binwidth = c(2, 2),
                fun = "mean",
                gap = 0
            )
    )
    tiles <- built$data[[1L]][order(built$data[[1L]]$x), ]

    expect_equal(tiles$count, c(2, 1))
    expect_equal(tiles$hours, c(2, 1))
    expect_equal(tiles$value, c(2, 5))
    expect_equal(tiles$xmin, c(20, 22), tolerance = 1e-8)
    expect_equal(tiles$xmax, c(22, 24), tolerance = 1e-8)
    expect_equal(tiles$ymin, c(0.008, 0.008), tolerance = 1e-8)
    expect_equal(tiles$ymax, c(0.010, 0.010), tolerance = 1e-8)

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            stat_psychro_bin(
                ggplot2::aes(
                    dry_bulb, humidity_ratio,
                    fill = ggplot2::after_stat(hours)
                ),
                binwidth = c(2, 2),
                gap = 0
            )
    )
    expect_equal(sum(built$data[[1L]]$hours), nrow(d))
    expect_equal(built$data[[1L]]$xmin, c(20, 22), tolerance = 1e-8)
})

test_that("psychrometric tile defaults set gap and alpha", {
    d <- data.frame(
        dry_bulb = c(20.1, 20.8, 22.1),
        humidity_ratio = c(8.1, 8.5, 8.2)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, humidity_ratio),
                binwidth = c(2, 2)
            )
    )
    tiles <- built$data[[1L]][order(built$data[[1L]]$x), ]

    expect_equal(tiles$xmax - tiles$xmin, c(1.84, 1.84), tolerance = 1e-8)
    expect_equal(
        tiles$ymax - tiles$ymin,
        c(0.00184, 0.00184),
        tolerance = 1e-8
    )
    expect_equal(tiles$cell_xmin, c(20, 22), tolerance = 1e-8)
    expect_equal(tiles$cell_xmax, c(22, 24), tolerance = 1e-8)
    expect_equal(tiles$cell_ymin, c(0.008, 0.008), tolerance = 1e-8)
    expect_equal(tiles$cell_ymax, c(0.010, 0.010), tolerance = 1e-8)
    expect_equal(unique(tiles$alpha), 0.85)
    expect_equal(sum(tiles$count), nrow(d))

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, humidity_ratio),
                binwidth = c(2, 2),
                gap = 0,
                alpha = 1
            )
    )
    tiles <- built$data[[1L]][order(built$data[[1L]]$x), ]

    expect_equal(tiles$xmax - tiles$xmin, c(2, 2), tolerance = 1e-8)
    expect_equal(tiles$ymax - tiles$ymin, c(0.002, 0.002), tolerance = 1e-8)
    expect_equal(tiles$cell_xmax - tiles$cell_xmin, c(2, 2), tolerance = 1e-8)
    expect_equal(
        tiles$cell_ymax - tiles$cell_ymin,
        c(0.002, 0.002),
        tolerance = 1e-8
    )
    expect_equal(unique(tiles$alpha), 1)

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(
                    dry_bulb, humidity_ratio,
                    alpha = ggplot2::after_stat(hours)
                ),
                binwidth = c(2, 2)
            )
    )
    expect_gt(length(unique(built$data[[1L]]$alpha)), 1L)
    expect_false(any(built$data[[1L]]$alpha == 0.85))
})

test_that("psychrometric tile boundary controls bin alignment", {
    d <- data.frame(
        dry_bulb = c(20.1, 20.8, 22.1),
        humidity_ratio = c(8.1, 8.5, 8.2)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, humidity_ratio),
                binwidth = c(2, 2),
                boundary = c(1, 1),
                gap = 0.2
            )
    )
    tiles <- built$data[[1L]][order(built$data[[1L]]$x), ]

    expect_equal(tiles$cell_xmin, c(19, 21), tolerance = 1e-8)
    expect_equal(tiles$cell_xmax, c(21, 23), tolerance = 1e-8)
    expect_equal(tiles$cell_ymin, c(0.007, 0.007), tolerance = 1e-8)
    expect_equal(tiles$cell_ymax, c(0.009, 0.009), tolerance = 1e-8)
    expect_equal(tiles$x, c(20, 22), tolerance = 1e-8)
    expect_equal(tiles$y, c(0.008, 0.008), tolerance = 1e-8)
    expect_equal(tiles$xmax - tiles$xmin, c(1.6, 1.6), tolerance = 1e-8)
    expect_equal(
        tiles$ymax - tiles$ymin,
        c(0.0016, 0.0016),
        tolerance = 1e-8
    )
    expect_equal(tiles$cell_xmax - tiles$cell_xmin, c(2, 2), tolerance = 1e-8)
    expect_equal(
        tiles$cell_ymax - tiles$cell_ymin,
        c(0.002, 0.002),
        tolerance = 1e-8
    )
    expect_equal(sum(tiles$count), nrow(d))
})

test_that("psychrometric tile cell grid covers the chart area", {
    d <- data.frame(
        dry_bulb = c(20.1, 20.8, 22.1),
        humidity_ratio = c(8.1, 8.5, 8.2)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, humidity_ratio),
                binwidth = c(2, 2)
            )
    )

    segments <- psychro_tile_cell_segments(
        built$data[[1L]],
        built$layout$panel_params[[1L]],
        built$layout$coord
    )
    vertical <- segments[segments$x == segments$xend, , drop = FALSE]
    horizontal <- segments[segments$y == segments$yend, , drop = FALSE]

    expect_gt(nrow(segments), nrow(built$data[[1L]]) * 4L)
    expect_equal(min(vertical$x), 10, tolerance = 1e-8)
    expect_equal(max(vertical$x), 34, tolerance = 1e-8)
    expect_equal(min(horizontal$y), 0, tolerance = 1e-8)
    expect_equal(max(horizontal$y), 0.02, tolerance = 1e-8)
    expect_true(all(vertical$yend <= built$layout$panel_params[[1L]]$y.range[[2L]]))
    expect_true(any(vertical$yend < built$layout$panel_params[[1L]]$y.range[[2L]]))
    expect_true(all(horizontal$xend == built$layout$panel_params[[1L]]$x.range[[2L]]))
})

test_that("psychrometric tile bodies are clipped to saturation", {
    d <- data.frame(dry_bulb = 9.5, humidity_ratio = 7)
    plot <- ggpsychro(d, tdb_lim = c(0, 20), hum_lim = c(0, 15)) +
        geom_psychro_tile(
            ggplot2::aes(dry_bulb, humidity_ratio),
            binwidth = c(4, 2),
            gap = 0
        )
    built <- ggplot2::ggplot_build(plot)
    tile <- built$data[[1L]]
    polygons <- psychro_tile_polygon_data(tile, built$layout$coord, n = 32)
    saturation <- psychro_saturation_humratio(
        polygons$x,
        built$layout$coord$units,
        built$layout$coord$pressure
    )

    expect_gt(nrow(polygons), 4L)
    expect_true(all(polygons$y <= saturation + 1e-8))
    expect_equal(min(polygons$y), tile$ymin, tolerance = 1e-8)
    expect_lte(max(polygons$y), tile$ymax + 1e-8)
    expect_true(any(
        polygons$y > tile$ymin + 1e-8 &
            polygons$y < tile$ymax - 1e-8
    ))
    expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("psychrometric tile gap is validated", {
    d <- data.frame(dry_bulb = 20.1, humidity_ratio = 8.1)
    p <- function(gap) {
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, humidity_ratio),
                binwidth = c(2, 2),
                gap = gap
            )
    }

    expect_error(ggplot2::ggplot_build(p(-0.1)), "`gap` must")
    expect_error(ggplot2::ggplot_build(p(1)), "`gap` must")
    expect_error(ggplot2::ggplot_build(p(c(0, 0.1))), "`gap` must")
    expect_error(ggplot2::ggplot_build(p(Inf)), "`gap` must")
})

test_that("psychrometric tile boundary is validated", {
    d <- data.frame(dry_bulb = 20.1, humidity_ratio = 8.1)
    p <- function(boundary) {
        ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, humidity_ratio),
                binwidth = c(2, 2),
                boundary = boundary
            )
    }

    expect_error(ggplot2::ggplot_build(p("0")), "`boundary` must")
    expect_error(ggplot2::ggplot_build(p(c(0, 0, 0))), "`boundary` must")
    expect_error(ggplot2::ggplot_build(p(Inf)), "`boundary` must")
    expect_error(ggplot2::ggplot_build(p(NA_real_)), "`boundary` must")

    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_psychro_tile(
                    ggplot2::aes(dry_bulb, humidity_ratio),
                    bins = c(2, 2),
                    boundary = NA_real_
                )
        )
    )
})

test_that("psychrometric tile stats inherit IP units and pressure", {
    d <- data.frame(
        dry_bulb = c(70, 72, 74),
        relative_humidity = c(40, 50, 60)
    )

    built <- ggplot2::ggplot_build(
        ggpsychro(d, tdb_lim = c(50, 90), hum_lim = c(0, 120), units = "IP") +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, relhum = relative_humidity),
                binwidth = c(5, 10)
            )
    )
    tiles <- built$data[[1L]]

    expect_equal(sum(tiles$count), nrow(d))
    expect_gt(max(tiles$y), 0)
    expect_gt(max(tiles$ymax), max(tiles$ymin))
})

test_that("psychrometric tile stats handle missing values", {
    d <- data.frame(
        dry_bulb = c(20, NA, 22),
        humidity_ratio = c(8, 9, NA)
    )

    expect_warning(
        ggplot2::ggplot_build(
            ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_psychro_tile(
                    ggplot2::aes(dry_bulb, humidity_ratio),
                    binwidth = c(2, 2)
        )
    ),
        "Removed 2 rows containing non-finite"
    )

    expect_no_warning(
        built <- ggplot2::ggplot_build(
            ggpsychro(d, tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_psychro_tile(
                    ggplot2::aes(dry_bulb, humidity_ratio),
                    binwidth = c(2, 2),
                    na.rm = TRUE
                )
        )
    )
    expect_equal(sum(built$data[[1L]]$count), 1)
})

test_that("psychrometric tiles build with grids, fill scales, and facets", {
    d <- data.frame(
        dry_bulb = c(18, 20, 22, 24, 26, 28),
        relative_humidity = c(45, 50, 55, 60, 65, 70),
        period = rep(c("spring", "summer"), each = 3)
    )

    expect_no_error(
        ggplot2::ggplot_build(
            ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 30)) +
                geom_grid_relhum() +
                geom_psychro_tile(
                    ggplot2::aes(dry_bulb, relhum = relative_humidity),
                    binwidth = c(5, 2)
                ) +
                ggplot2::scale_fill_continuous() +
                ggplot2::facet_wrap(~period)
        )
    )

    expect_no_error(
        ggplot2::ggplotGrob(
            ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 30)) +
                geom_grid_relhum() +
                geom_psychro_tile(
                    ggplot2::aes(dry_bulb, relhum = relative_humidity),
                    binwidth = c(5, 2)
                ) +
                ggplot2::facet_wrap(~period)
        )
    )

    expect_no_error(
        ggplot2::ggplotGrob(
            ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 30)) +
                geom_psychro_tile(
                    ggplot2::aes(dry_bulb, relhum = relative_humidity),
                    binwidth = c(5, 2),
                    cell.grid = FALSE
                )
        )
    )
})

test_that("psychrometric tile distribution is stable", {
    d <- data.frame(
        dry_bulb = c(18, 19, 20, 20.5, 22, 24, 26, 27),
        relative_humidity = c(45, 50, 55, 58, 62, 65, 70, 72)
    )

    vdiffr::expect_doppelganger(
        "psychro tile distribution",
        ggpsychro(d, tdb_lim = c(10, 35), hum_lim = c(0, 30)) +
            geom_psychro_tile(
                ggplot2::aes(dry_bulb, relhum = relative_humidity),
                binwidth = c(2, 2)
            ) +
            ggplot2::scale_fill_gradient(low = "#dbeafe", high = "#1d4ed8")
    )
})
