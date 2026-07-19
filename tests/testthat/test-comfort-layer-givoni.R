# Focused comfort calculation, model, and layer behavior tests.

test_that("Givoni strategy zones build and stay below saturation", {
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )
    expect_s3_class(comfort_strategy_givoni(), "PsyComfortGivoniStrategy")
    expect_equal(comfort_strategy_givoni()$variant, "adaptive")
    expect_error(comfort_strategy_givoni(mean_outdoor = NA), "mean_outdoor")
    expect_error(
        comfort_strategy_givoni(mean_outdoor = NULL),
        "`mean_outdoor` must be supplied"
    )
    expect_error(
        comfort_strategy_givoni(tdb_range = c(20, 26)),
        "`tdb_range` is only used"
    )
    expect_error(
        comfort_strategy_givoni(variant = "fixed", relhum_range = c(70, 30)),
        "`relhum_range` must be strictly increasing"
    )
    expect_s3_class(element_givoni_zone(), "PsyComfortZoneElement")

    cool <- givoni__zone_data(
        comfort_strategy_givoni(mean_outdoor = 15),
        "comfort",
        "SI",
        pressure,
        FALSE,
        c(0, 45),
        c(0, 35)
    )
    warm <- givoni__zone_data(
        comfort_strategy_givoni(mean_outdoor = 25),
        "comfort",
        "SI",
        pressure,
        FALSE,
        c(0, 45),
        c(0, 35)
    )
    expect_gt(mean(warm$tdb), mean(cool$tdb))

    zones <- givoni__zone_data(
        comfort_strategy_givoni(),
        NULL,
        "SI",
        pressure,
        FALSE,
        c(0, 50),
        c(0, 35)
    )
    drawable_zones <- givoni__zone_specs()
    drawable_zones <- drawable_zones$zone[drawable_zones$draw_zone]
    expect_true(all(drawable_zones %in% unique(zones$zone)))
    expect_false(any(
        c("air_conditioning_dehumidification", "humidification") %in%
            unique(zones$zone)
    ))
    sat <- zone__saturation_humratio(zones$tdb, "SI", pressure)
    expect_true(all(zones$humratio <= sat + 1e-8))

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(alpha = 0.35)
    )$data
    expect_equal(length(built), 14L)
    comfort_layer <- which(vapply(
        built,
        function(x) {
            "zone" %in% names(x) && any(x$zone == "comfort")
        },
        logical(1L)
    ))[[1L]]
    expect_equal(unique(built[[comfort_layer]]$alpha), 0.2)
    path_label_layer <- length(built) - 2L
    expect_true(
        "NATURAL VENTILATION" %in%
            unique(built[[path_label_layer]]$label)
    )
    expect_true("MASS COOLING" %in% unique(built[[path_label_layer]]$label))
    expect_true(
        "AIR-CONDITIONING" %in%
            unique(built[[path_label_layer]]$label)
    )
    label_layer <- length(built) - 1L
    expect_true("COMFORT\nZONE" %in% unique(built[[label_layer]]$label))
    expect_true("HEATING" %in% unique(built[[label_layer]]$label))
    expect_true(
        "AIR-CONDITIONING &\nDEHUMIDIFICATION" %in%
            unique(built[[label_layer]]$label)
    )
    expect_true(any(grepl(
        "\u00b0C",
        built[[length(built)]]$label,
        fixed = TRUE
    )))

    unlabelled <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(alpha = 0.35, labels = FALSE)
    )$data
    expect_equal(length(unlabelled), length(built) - 3L)

    path_labels <- givoni__label_data(
        comfort_strategy_givoni(),
        "path",
        "SI",
        pressure,
        FALSE,
        c(0, 50),
        c(0, 35)
    )
    expect_equal(
        unique(path_labels$vjust[path_labels$zone == "natural_ventilation"]),
        1.8
    )
    top_to_bottom <- c(
        "internal_gains",
        "passive_solar_heating",
        "active_solar_heating",
        "mass_cooling",
        "mass_cooling_night_ventilation",
        "winter",
        "air_conditioning"
    )
    for (zone in top_to_bottom) {
        zone_data <- path_labels[path_labels$zone == zone, , drop = FALSE]
        expect_gt(
            zone_data$humratio[[1L]],
            zone_data$humratio[[nrow(zone_data)]]
        )
    }

    point_labels <- givoni__label_data(
        comfort_strategy_givoni(),
        "point",
        "SI",
        pressure,
        FALSE,
        c(0, 50),
        c(0, 35)
    )
    heating_label <- point_labels[
        point_labels$zone == "heating",
        ,
        drop = FALSE
    ]
    expect_equal(heating_label$angle, 270)
    heating_sat <- givoni__humratio(heating_label$tdb, 100, pressure)
    expect_equal(heating_label$humratio, heating_sat / 2, tolerance = 1e-8)

    air_label <- path_labels[
        path_labels$zone == "air_conditioning",
        ,
        drop = FALSE
    ]
    expect_lt(unique(air_label$tdb), 50)
    expect_gt(unique(air_label$tdb), 45)

    mean_line <- givoni__mean_outdoor_data(
        comfort_strategy_givoni(mean_outdoor = 17.5),
        "SI",
        pressure,
        FALSE,
        c(-10, 50),
        c(0, 35)
    )
    mean_label <- givoni__mean_outdoor_label_data(
        comfort_strategy_givoni(mean_outdoor = 17.5),
        "SI",
        pressure,
        FALSE,
        c(-10, 50),
        c(0, 35)
    )
    mean_sat <- givoni__humratio(17.5, 100, pressure)
    expect_gt(max(mean_line$humratio), mean_sat)
    expect_gt(mean_label$humratio[[1L]], mean_sat)
    expect_equal(mean_label$angle[[1L]], 270)
    expect_equal(mean_label$vjust[[1L]], -0.25)

    with_pmv <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(alpha = 0.35, show_pmv = TRUE)
    )$data
    expect_gt(length(with_pmv), length(built))
})
test_that("fixed Givoni-Milne variant uses the 1979 comfort anchor", {
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )
    fixed <- comfort_strategy_givoni(
        variant = "fixed",
        mean_outdoor = NULL
    )
    expect_equal(fixed$variant, "fixed")
    expect_false(givoni__is_adaptive(fixed))

    comfort <- givoni__zone_data(
        fixed,
        "comfort",
        "SI",
        pressure,
        FALSE,
        c(0, 50),
        c(0, 35)
    )
    expect_equal(range(comfort$tdb), c(20, 25.5))
    expect_true(all(comfort$humratio <= 0.016 + 1e-8))

    shifted_fixed <- givoni__zone_data(
        comfort_strategy_givoni(variant = "fixed", mean_outdoor = 30),
        "comfort",
        "SI",
        pressure,
        FALSE,
        c(0, 50),
        c(0, 35)
    )
    expect_equal(shifted_fixed$tdb, comfort$tdb)
    expect_equal(shifted_fixed$humratio, comfort$humratio)

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(fixed)
    )$data
    expect_equal(length(built), 12L)
    expect_false(any(vapply(
        built,
        function(x) {
            "metric" %in% names(x) && any(x$metric == "givoni_mean_outdoor")
        },
        logical(1L)
    )))
})
test_that("custom fixed Givoni-Milne anchors drive comfort zone geometry", {
    pressure <- psychrolib__with_units(
        "SI",
        psychrolib::GetStandardAtmPressure(0)
    )
    custom <- comfort_strategy_givoni(
        variant = "fixed",
        mean_outdoor = NULL,
        tdb_range = c(22, 27),
        relhum_range = c(30, 70)
    )
    comfort <- givoni__zone_data(
        custom,
        "comfort",
        "SI",
        pressure,
        FALSE,
        c(0, 50),
        c(0, 35)
    )
    relhum <- psychrolib__with_units(
        "SI",
        psychrolib::GetRelHumFromHumRatio(
            comfort$tdb,
            comfort$humratio,
            pressure
        )
    ) *
        100

    expect_equal(range(comfort$tdb), c(22, 27))
    expect_equal(range(relhum), c(30, 70), tolerance = 1e-8)

    custom_ip <- comfort_strategy_givoni(
        variant = "fixed",
        mean_outdoor = NULL,
        units = "IP",
        tdb_range = c(71.6, 80.6),
        relhum_range = c(30, 70)
    )
    custom_ip_comfort <- givoni__zone_data(
        custom_ip,
        "comfort",
        "IP",
        pressure,
        FALSE,
        c(32, 120),
        c(0, 250)
    )
    expect_equal(range(custom_ip_comfort$tdb), c(71.6, 80.6))
})
test_that("Givoni zone styles can be overridden per zone", {
    styled <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(
                zone_style = list(
                    comfort = element_givoni_zone(
                        fill = "#00AA55",
                        colour = "#123456",
                        linewidth = 1.4,
                        alpha = 0.4
                    ),
                    winter = ggplot2::element_polygon(
                        colour = "#AA0000",
                        linetype = "dotdash",
                        linewidth = 1.2
                    ),
                    natural_ventilation = list(
                        fill = "#99CCFF",
                        colour = "#0033AA"
                    )
                )
            )
    )$data

    comfort_layer <- which(vapply(
        styled,
        function(x) {
            "zone" %in% names(x) && any(x$zone == "comfort")
        },
        logical(1L)
    ))[[1L]]
    expect_equal(unique(styled[[comfort_layer]]$fill), "#00AA55")
    expect_equal(unique(styled[[comfort_layer]]$colour), "#123456")
    expect_equal(unique(styled[[comfort_layer]]$linewidth), 1.4)
    expect_equal(unique(styled[[comfort_layer]]$alpha), 0.4)

    winter_layer <- which(vapply(
        styled,
        function(x) {
            "zone" %in% names(x) && any(x$zone == "winter")
        },
        logical(1L)
    ))[[1L]]
    expect_equal(unique(styled[[winter_layer]]$colour), "#AA0000")
    expect_equal(unique(styled[[winter_layer]]$linetype), "dotdash")
    expect_equal(unique(styled[[winter_layer]]$linewidth), 1.2)

    natural_layer <- which(vapply(
        styled,
        function(x) {
            "zone" %in% names(x) && any(x$zone == "natural_ventilation")
        },
        logical(1L)
    ))[[1L]]
    expect_equal(unique(styled[[natural_layer]]$fill), "#99CCFF")
    expect_equal(unique(styled[[natural_layer]]$colour), "#0033AA")
    expect_equal(unique(styled[[natural_layer]]$alpha), 0.2)

    expect_error(
        ggplot2::ggplot_build(
            ggpsychro() +
                geom_comfort_givoni(
                    zone_style = list(not_a_zone = element_givoni_zone())
                )
        ),
        "Unknown Givoni zone"
    )
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro() +
                geom_comfort_givoni(
                    zone_style = list(comfort = list(stroke_color = "red"))
                )
        ),
        "Unknown comfort zone style field"
    )
})
