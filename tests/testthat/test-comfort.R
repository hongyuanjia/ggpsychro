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

test_that("native PMV kernel matches R scalar fallback", {
    tdb <- c(20, 25, 30, NA)
    tr <- c(21, 25, 32, 25)
    vr <- c(0.1, 0.2, 0.4, 0.1)
    rh <- c(40, 50, 70, 50)
    met <- c(1.0, 1.2, 1.6, 1.2)
    clo <- c(0.5, 0.7, 0.3, 0.5)
    wme <- c(0, 0, 0.1, 0)

    native <- comfort_pmv_vec(tdb, tr, vr, rh, met, clo, wme)
    fallback <- mapply(
        comfort_pmv_one,
        tdb, tr, vr, rh, met, clo, wme,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
    )

    expect_equal(native, fallback, tolerance = 1e-10)
})

test_that("comfort SET matches fixed pythermalcomfort oracle value", {
    result <- comfort_set(25, tr = 25, v = 0.1, rh = 50, met = 1.2, clo = 0.5)
    expect_equal(result$set[[1L]], comfort_oracle("set", "gagge_default", "set"))
})

test_that("native SET kernel matches R scalar fallback", {
    tdb <- c(22, 25, 28, NA)
    tr <- c(22, 26, 30, 25)
    v <- c(0.1, 0.2, 0.6, 0.1)
    rh <- c(40, 50, 70, 50)
    met <- c(1.1, 1.2, 1.6, 1.2)
    clo <- c(0.4, 0.5, 0.7, 0.5)
    wme <- c(0, 0, 0.1, 0)

    native <- comfort_set_vec(
        tdb, tr, v, rh, met, clo, wme,
        body_surface_area = 1.8258, p_atm = 101325,
        position = "standing"
    )
    fallback <- mapply(
        comfort_set_one,
        tdb, tr, v, rh, met, clo, wme,
        MoreArgs = list(
            body_surface_area = 1.8258,
            p_atm = 101325,
            position = "standing"
        ),
        SIMPLIFY = TRUE, USE.NAMES = FALSE
    )

    expect_equal(native, fallback, tolerance = 1e-7)
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
        c(90, 90), rh = 70, solar_exposure = c(0, 1), units = "IP"
    )
    expect_equal(diff(exposure_vector$heat_index), 8, tolerance = 0.05)
    expect_true(is.na(comfort_heat_index(
        90, rh = 70, solar_exposure = NA_real_, units = "IP"
    )$heat_index))
    expect_error(
        comfort_heat_index(90, rh = 70, solar_exposure = 2, units = "IP"),
        "solar_exposure"
    )
    expect_error(
        comfort_heat_index(90, rh = 70, solar_exposure = -0.1, units = "IP"),
        "solar_exposure"
    )

    si <- comfort_heat_index(get_c_from_f(90), rh = 70, round_output = FALSE)
    ip <- comfort_heat_index(90, rh = 70, units = "IP", round_output = FALSE)
    expect_equal(get_f_from_c(si$heat_index), ip$heat_index, tolerance = 1e-8)

    expect_true(is.na(comfort_heat_index(90, rh = 150, units = "IP")$heat_index))
    expect_error(comfort_model_heat_index(solar_exposure = 2), "solar_exposure")
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
    expect_true(is.na(comfort_pmv(25, rh = -10)$pmv[[1L]]))
    expect_true(is.na(comfort_set(25, rh = 150)$set[[1L]]))
    expect_true(is.na(comfort_adaptive(25, t_running = 5)$tmp_cmf[[1L]]))
    expect_true(is.na(comfort_adaptive(25, t_running = 20, v = -0.1,
        standard = "en16798")$acceptability[[1L]]))
    expect_true(is.na(comfort_pmv(c(25, NA), rh = 50)$pmv[[2L]]))
})

test_that("comfort model objects validate inputs", {
    expect_s3_class(comfort_model_pmv(), "PsyComfortModel")
    expect_s3_class(comfort_model_set(), "PsyComfortModel")
    expect_s3_class(comfort_model_adaptive(t_running = 20), "PsyComfortModel")
    expect_s3_class(comfort_model_heat_index(), "PsyComfortModel")
    expect_error(comfort_model_pmv(model = "bad"))
    expect_error(comfort_model_type(list()), "comfort_model")
    expect_error(comfort_model_adaptive(t_running = 20, standard = "bad"))
    expect_error(
        comfort_standard_ashrae55_2017(c(0.5, -0.5)),
        "strictly increasing"
    )
    expect_error(
        comfort_standard_en15251_2007(c(-0.7, 0.2, -0.2, 0.7)),
        "strictly increasing"
    )
})

test_that("comfort overlay and contour build on psychrometric panel grids", {
    overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(n = c(24, 16), gap = 0) +
            scale_fill_comfort_pmv()
    ))
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

    overlay_alpha <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(n = c(24, 16), alpha = 0.35)
    ))
    expect_equal(unique(overlay_alpha$alpha), 0.35)

    isoband <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(method = "isoband", n = c(24, 16))
    ))
    expect_gt(nrow(isoband), 0L)
    expect_true("level_mid" %in% names(isoband))
    expect_equal(unique(isoband$alpha), 0.55)

    tile <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(method = "tile", n = c(24, 16))
    ))
    expect_gt(nrow(tile), 0L)
    expect_equal(unique(tile$alpha), 0.55)

    set_overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(model = comfort_model_set(), n = c(24, 16))
    ))
    expect_gt(nrow(set_overlay), 0L)
    expect_equal(unique(set_overlay$alpha), 0.55)

    adaptive_overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(
                model = comfort_model_adaptive(t_running = 20),
                n = c(24, 16)
            )
    ))
    expect_gt(nrow(adaptive_overlay), 0L)
    expect_equal(unique(adaptive_overlay$alpha), 0.55)

    heat_overlay <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_overlay(
                model = comfort_model_heat_index(), n = c(32, 24)
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
    expect_true(all(c("CAUTION", "EXTREME CAUTION", "DANGER",
        "EXTREME DANGER") %in% unique(heat_index[[6L]]$label)))
    expect_equal(unique(heat_index[[6L]]$alpha), 0)

    collect_grobs <- function(grob) {
        children <- c(
            if (!is.null(grob$grobs)) grob$grobs else list(),
            if (!is.null(grob$children)) as.list(grob$children) else list()
        )

        c(list(grob), unlist(lapply(children, collect_grobs), recursive = FALSE))
    }
    heat_index_grobs <- collect_grobs(ggplot2::ggplotGrob(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_heat_index(n = c(32, 24), alpha = 0.4)
    ))
    expect_true(any(vapply(heat_index_grobs, function(grob) {
        identical(grob$name, "psychro-heat-index-labels")
    }, logical(1L))))

    tile_alpha <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_overlay(method = "tile", n = c(24, 16), alpha = 0.35)
    ))
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

    contour <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_contour(n = c(32, 20), breaks = c(-1, 0, 1))
    ))
    expect_gt(nrow(contour), 0L)
    expect_true(all(contour$level %in% c(-1, 0, 1)))

    contour_labelled <- built_data_layers(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_contour(
                model = comfort_model_set(),
                metric = "set",
                breaks = c(22, 24, 26),
                n = c(32, 20),
                label = TRUE
            )
    ))
    expect_equal(length(contour_labelled), 1L)
    expect_true(all(c("22", "24", "26") %in% contour_labelled[[1L]]$label))
    expect_true(all(c("label", "level", "value") %in% names(contour_labelled[[1L]])))

    pmv_labelled <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_contour(
                breaks = c(-1, 0, 1),
                n = c(32, 20),
                label = TRUE
            )
    ))
    expect_true(all(c("-1.0", "0.0", "+1.0") %in% pmv_labelled$label))

    expect_error(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_contour(label = NA),
        "label"
    )
    expect_error(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_contour(label = TRUE, label_size = -1),
        "label_size"
    )

    heat_contour <- first_built_data(ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(20, 45), hum_lim = c(0, 35)) +
            geom_comfort_contour(
                model = comfort_model_heat_index(), n = c(32, 24)
            )
    ))
    expect_true(all(sort(unique(round(heat_contour$level, 6))) %in%
        round(comfort_heat_index_thresholds("SI"), 6)))

    heat_zones <- comfort_heat_index_zone_data(
        comfort_model_heat_index(), NULL, c(32, 24), "SI", pressure,
        FALSE, c(20, 45), c(0, 35)
    )
    heat_zone_parts <- lapply(seq_along(comfort_heat_index_zone_specs()), function(i) {
        comfort_heat_index_zone_data(
            comfort_model_heat_index(), i, c(32, 24), "SI", pressure,
            FALSE, c(20, 45), c(0, 35)
        )
    })
    heat_zone_parts <- heat_zone_parts[vapply(heat_zone_parts, nrow, integer(1L)) > 0L]
    heat_zone_parts <- do.call(rbind, heat_zone_parts)
    expect_equal(
        table(heat_zones$category_id),
        table(heat_zone_parts$category_id)
    )
    zone_fills <- vapply(comfort_heat_index_zone_specs(), `[[`,
        character(1L), "fill")
    expect_equal(
        as.vector(tapply(heat_zones$fill, heat_zones$category_id, unique)),
        zone_fills[as.integer(names(table(heat_zones$category_id)))]
    )
})

test_that("Givoni strategy zones build and stay below saturation", {
    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))
    expect_s3_class(comfort_strategy_givoni(), "PsyComfortGivoniStrategy")
    expect_error(comfort_strategy_givoni(mean_outdoor = NA), "mean_outdoor")
    expect_s3_class(element_comfort_zone(), "PsyComfortZoneElement")

    cool <- comfort_givoni_zone_data(
        comfort_strategy_givoni(mean_outdoor = 15), "comfort", "SI",
        pressure, FALSE, c(0, 45), c(0, 35)
    )
    warm <- comfort_givoni_zone_data(
        comfort_strategy_givoni(mean_outdoor = 25), "comfort", "SI",
        pressure, FALSE, c(0, 45), c(0, 35)
    )
    expect_gt(mean(warm$tdb), mean(cool$tdb))

    zones <- comfort_givoni_zone_data(
        comfort_strategy_givoni(), NULL, "SI", pressure,
        FALSE, c(0, 50), c(0, 35)
    )
    drawable_zones <- comfort_givoni_zone_specs()
    drawable_zones <- drawable_zones$zone[drawable_zones$draw_zone]
    expect_true(all(drawable_zones %in% unique(zones$zone)))
    expect_false(any(c("air_conditioning_dehumidification", "humidification") %in%
        unique(zones$zone)))
    sat <- psychro_saturation_humratio(zones$tdb, "SI", pressure)
    expect_true(all(zones$humratio <= sat + 1e-8))

    built <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(alpha = 0.35)
    )$data
    expect_equal(length(built), 14L)
    comfort_layer <- which(vapply(built, function(x) {
        "zone" %in% names(x) && any(x$zone == "comfort")
    }, logical(1L)))[[1L]]
    expect_equal(unique(built[[comfort_layer]]$alpha), 0.2)
    path_label_layer <- length(built) - 2L
    expect_true("NATURAL VENTILATION" %in%
        unique(built[[path_label_layer]]$label))
    expect_true("MASS COOLING" %in% unique(built[[path_label_layer]]$label))
    expect_true("AIR-CONDITIONING" %in%
        unique(built[[path_label_layer]]$label))
    label_layer <- length(built) - 1L
    expect_true("COMFORT\nZONE" %in% unique(built[[label_layer]]$label))
    expect_true("HEATING" %in% unique(built[[label_layer]]$label))
    expect_true("AIR-CONDITIONING &\nDEHUMIDIFICATION" %in%
        unique(built[[label_layer]]$label))
    expect_true(any(grepl("\u00b0C", built[[length(built)]]$label,
        fixed = TRUE)))

    path_labels <- comfort_givoni_label_data(
        comfort_strategy_givoni(), "path", "SI", pressure,
        FALSE, c(0, 50), c(0, 35)
    )
    expect_equal(
        unique(path_labels$vjust[path_labels$zone == "natural_ventilation"]),
        1.8
    )
    top_to_bottom <- c(
        "internal_gains", "passive_solar_heating", "active_solar_heating",
        "mass_cooling", "mass_cooling_night_ventilation", "winter",
        "air_conditioning"
    )
    for (zone in top_to_bottom) {
        zone_data <- path_labels[path_labels$zone == zone, , drop = FALSE]
        expect_gt(zone_data$humratio[[1L]], zone_data$humratio[[nrow(zone_data)]])
    }

    point_labels <- comfort_givoni_label_data(
        comfort_strategy_givoni(), "point", "SI", pressure,
        FALSE, c(0, 50), c(0, 35)
    )
    heating_label <- point_labels[point_labels$zone == "heating", ,
        drop = FALSE]
    expect_equal(heating_label$angle, 270)
    heating_sat <- comfort_givoni_humratio(heating_label$tdb, 100, pressure)
    expect_equal(heating_label$humratio, heating_sat / 2, tolerance = 1e-8)

    air_label <- path_labels[path_labels$zone == "air_conditioning", ,
        drop = FALSE]
    expect_lt(unique(air_label$tdb), 50)
    expect_gt(unique(air_label$tdb), 45)

    mean_line <- comfort_givoni_mean_outdoor_data(
        comfort_strategy_givoni(mean_outdoor = 17.5), "SI", pressure,
        FALSE, c(-10, 50), c(0, 35)
    )
    mean_label <- comfort_givoni_mean_outdoor_label_data(
        comfort_strategy_givoni(mean_outdoor = 17.5), "SI", pressure,
        FALSE, c(-10, 50), c(0, 35)
    )
    mean_sat <- comfort_givoni_humratio(17.5, 100, pressure)
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

test_that("Givoni zone styles can be overridden per zone", {
    styled <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35)) +
            geom_comfort_givoni(
                zone_style = list(
                    comfort = element_comfort_zone(
                        fill = "#00AA55", colour = "#123456",
                        linewidth = 1.4, alpha = 0.4
                    ),
                    winter = ggplot2::element_polygon(
                        colour = "#AA0000", linetype = "dotdash",
                        linewidth = 1.2
                    ),
                    natural_ventilation = list(
                        fill = "#99CCFF", colour = "#0033AA"
                    )
                )
            )
    )$data

    comfort_layer <- which(vapply(styled, function(x) {
        "zone" %in% names(x) && any(x$zone == "comfort")
    }, logical(1L)))[[1L]]
    expect_equal(unique(styled[[comfort_layer]]$fill), "#00AA55")
    expect_equal(unique(styled[[comfort_layer]]$colour), "#123456")
    expect_equal(unique(styled[[comfort_layer]]$linewidth), 1.4)
    expect_equal(unique(styled[[comfort_layer]]$alpha), 0.4)

    winter_layer <- which(vapply(styled, function(x) {
        "zone" %in% names(x) && any(x$zone == "winter")
    }, logical(1L)))[[1L]]
    expect_equal(unique(styled[[winter_layer]]$colour), "#AA0000")
    expect_equal(unique(styled[[winter_layer]]$linetype), "dotdash")
    expect_equal(unique(styled[[winter_layer]]$linewidth), 1.2)

    natural_layer <- which(vapply(styled, function(x) {
        "zone" %in% names(x) && any(x$zone == "natural_ventilation")
    }, logical(1L)))[[1L]]
    expect_equal(unique(styled[[natural_layer]]$fill), "#99CCFF")
    expect_equal(unique(styled[[natural_layer]]$colour), "#0033AA")
    expect_equal(unique(styled[[natural_layer]]$alpha), 0.2)

    expect_error(
        ggplot2::ggplot_build(
            ggpsychro() +
                geom_comfort_givoni(
                    zone_style = list(not_a_zone = element_comfort_zone())
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

test_that("PMV root-traced curves solve requested levels", {
    pressure <- with_units("SI", psychrolib::GetStandardAtmPressure(0))
    model <- comfort_model_pmv()

    humratio <- seq(0, 0.02, length.out = 40)
    native_roots <- comfort_pmv_curve_roots(
        model, -0.5, humratio, c(15, 30), "SI", pressure
    )
    fallback_roots <- comfort_pmv_curve_roots_r(
        model, -0.5, humratio, c(15, 30), "SI", pressure
    )
    expect_equal(native_roots, fallback_roots, tolerance = 1e-7)

    native_sat_roots <- comfort_pmv_curve_saturation_roots(
        model, 0, c(0, 35), c(0, 35), "SI", pressure, 120
    )
    fallback_sat_roots <- comfort_pmv_curve_saturation_roots_r(
        model, 0, c(0, 35), c(0, 35), "SI", pressure, 120
    )
    expect_equal(native_sat_roots, fallback_sat_roots, tolerance = 1e-7)

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

    marsh_rootband <- comfort_pmv_rootband_data(
        comfort_model_pmv(), NULL, NULL, c(70, 48),
        "SI", pressure, FALSE, c(5, 40), c(0, 24)
    )
    cap_points <- data.frame(
        x = c(18.05063, 18.16456, 18.16456, 18.16456),
        y = c(0.01292650, 0.01302172, 0.01295477, 0.01288782)
    )
    cap_polys <- split(marsh_rootband, marsh_rootband$group)
    cap_covered <- vapply(seq_len(nrow(cap_points)), function(i) {
        any(vapply(cap_polys, function(poly) {
            psychro_inside_polygon(
                cap_points$x[[i]], cap_points$y[[i]], poly$x, poly$y
            )
        }, logical(1L)))
    }, logical(1L))
    expect_true(all(cap_covered))

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
    axis_large_vjust <- comfort_pmv_axis_label_text_vjust(ggplot2::waiver(), 6)
    expect_gt(as.numeric(axis_large_vjust), as.numeric(axis_vjust))

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

    pmv_boundary <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-0.5, 0.5), 80, "SI", pressure,
        FALSE, c(15, 30), c(0, 20), label = "boundary"
    )
    expect_equal(unique(pmv_boundary$vjust[pmv_boundary$level < 0]), -0.25)
    expect_equal(unique(pmv_boundary$vjust[pmv_boundary$level > 0]), 1.25)

    pmv_boundary_mollier <- comfort_pmv_curve_data(
        comfort_model_pmv(), c(-0.5, 0.5), 80, "SI", pressure,
        TRUE, c(15, 30), c(0, 20), label = "boundary"
    )
    expect_equal(unique(pmv_boundary_mollier$vjust[
        pmv_boundary_mollier$level < 0
    ]), 1.25)
    expect_equal(unique(pmv_boundary_mollier$vjust[
        pmv_boundary_mollier$level > 0
    ]), -0.25)

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

    ashrae_alpha <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
            geom_comfort_standard_zone(comfort_standard_ashrae55_2017(), n = 90,
                alpha = 0.2)
    )$data
    expect_equal(unique(ashrae_alpha[[1L]]$alpha), 0.2)
    expect_error(
        ggplot2::ggplot_build(
            ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
                geom_comfort_standard_zone(alpha = NA_real_)
        ),
        "alpha"
    )

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
    expect_gt(nrow(first_built_data(ip)), 0L)

    ip_overlay <- ggplot2::ggplot_build(
        ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140), units = "IP") +
            geom_comfort_overlay(n = c(40, 24)) +
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

    expect_mollier_comfort(base + geom_comfort_overlay(n = c(40, 24)))
    expect_mollier_comfort(base + geom_comfort_overlay(method = "isoband",
        n = c(32, 20)))
    expect_mollier_comfort(base + geom_comfort_overlay(method = "tile",
        n = c(24, 16)))
    expect_mollier_comfort(base + geom_comfort_overlay(
        model = comfort_model_set(), n = c(24, 16)
    ))
    expect_mollier_comfort(base + geom_comfort_overlay(
        model = comfort_model_adaptive(t_running = 20), n = c(24, 16)
    ))
    expect_mollier_comfort(base + geom_comfort_overlay(
        model = comfort_model_heat_index(), n = c(32, 20)
    ))
    expect_mollier_comfort(base + geom_comfort_heat_index(n = c(32, 20)))

    expect_mollier_comfort(base + geom_comfort_contour(
        breaks = c(-1, 0, 1), n = c(32, 20)
    ))
    expect_mollier_comfort(base + geom_comfort_contour(
        model = comfort_model_set(), metric = "set", breaks = c(22, 24, 26),
        n = c(32, 20)
    ))
    expect_mollier_comfort(base + geom_comfort_contour(
        model = comfort_model_set(), metric = "set", breaks = c(22, 24, 26),
        n = c(32, 20), label = TRUE
    ))

    expect_mollier_comfort(base + geom_comfort_pmv_lines(
        levels = c(-1, 0, 1), n = 60
    ))
    expect_mollier_comfort(base + geom_comfort_standard_zone(
        comfort_standard_ashrae55_2017(), n = 60
    ))
    expect_mollier_comfort(base + geom_comfort_standard_zone(
        comfort_standard_en15251_2007(), n = 60
    ))
    expect_mollier_comfort(base + geom_comfort_givoni())

    expect_mollier_comfort(base + geom_comfort_zone(n = c(60, 40)))
    expect_mollier_comfort(base + geom_comfort_zone(
        model = comfort_model_set(), n = c(32, 20)
    ))
    expect_mollier_comfort(base + geom_comfort_zone(
        model = comfort_model_adaptive(t_running = 20)
    ))
})

test_that("Marsh-style comfort overlays have visual regressions", {
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

    vdiffr::expect_doppelganger(
        "comfort set contour labels",
        set_base +
            geom_comfort_contour(
                model = comfort_model_set(), metric = "set",
                breaks = c(22, 24, 26), n = c(70, 42),
                label = TRUE, colour = "#4A4A4A", linewidth = 0.7
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
                comfort_strategy_givoni(mean_outdoor = 22),
                alpha = 0.45
            )
    )

    vdiffr::expect_doppelganger(
        "comfort givoni styled zones",
        givoni_base +
            geom_comfort_givoni(
                comfort_strategy_givoni(mean_outdoor = 22),
                zone_style = list(
                    comfort = element_comfort_zone(
                        fill = "#66D27A", colour = "#1F5F2D", alpha = 0.35
                    ),
                    natural_ventilation = element_comfort_zone(
                        fill = "#B6E3FF", colour = "#2F6FB0", alpha = 0.25
                    ),
                    winter = element_comfort_zone(
                        colour = "#A14D00", linetype = "dashed"
                    ),
                    air_conditioning = element_comfort_zone(
                        colour = "#8B1E3F", linewidth = 1.2
                    )
                )
            )
    )
})

test_that("Mollier comfort overlays have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    base <- ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20),
        mollier = TRUE) +
        psychro_preset("minimal")

    vdiffr::expect_doppelganger(
        "comfort mollier pmv overlay",
        base +
            geom_comfort_overlay(n = c(50, 30)) +
            scale_fill_comfort_pmv() +
            geom_comfort_pmv_lines(levels = seq(-2, 2, by = 1), n = 100)
    )

    vdiffr::expect_doppelganger(
        "comfort mollier set overlay",
        base +
            geom_comfort_overlay(model = comfort_model_set(), n = c(40, 24)) +
            geom_comfort_contour(
                model = comfort_model_set(), metric = "set",
                breaks = c(22, 24, 26), n = c(40, 24),
                colour = "#4A4A4A"
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier set contour labels",
        base +
            geom_comfort_contour(
                model = comfort_model_set(), metric = "set",
                breaks = c(22, 24, 26), n = c(70, 42),
                label = TRUE, colour = "#4A4A4A", linewidth = 0.7
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier adaptive overlay",
        base +
            geom_comfort_overlay(
                model = comfort_model_adaptive(t_running = 20),
                n = c(40, 24)
            ) +
            geom_comfort_zone(
                model = comfort_model_adaptive(t_running = 20),
                fill = NA, colour = "#4A4A4A"
            )
    )

    vdiffr::expect_doppelganger(
        "comfort mollier ashrae55 2017 pmv zone",
        base +
            geom_comfort_standard_zone(comfort_standard_ashrae55_2017(), n = 100)
    )

    vdiffr::expect_doppelganger(
        "comfort mollier en15251 2007 pmv zones",
        base +
            geom_comfort_standard_zone(comfort_standard_en15251_2007(), n = 100)
    )
})

test_that("IP comfort overlays have visual regressions", {
    testthat::skip_on_os(c("linux", "windows"))

    base <- ggpsychro(tdb_lim = c(50, 90), hum_lim = c(0, 140),
        units = "IP") +
        psychro_preset("minimal")

    vdiffr::expect_doppelganger(
        "comfort ip pmv overlay",
        base +
            geom_comfort_overlay(n = c(50, 30)) +
            scale_fill_comfort_pmv() +
            geom_comfort_pmv_lines(levels = seq(-2, 2, by = 1), n = 100)
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
            geom_comfort_zone(model = comfort_model_adaptive(t_running = 20))
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
