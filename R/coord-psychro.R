#' Psychrometric coordinates
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritParams ggpsychro
#' @examples
#' ggpsychro() +
#'     coord_psychro(tdb_lim = c(10, 35), hum_lim = c(0, 25))
#'
#' ggpsychro(units = "IP", altitude = 1000) +
#'     coord_psychro(
#'         tdb_lim = c(50, 100),
#'         hum_lim = c(0, 140),
#'         units = "IP",
#'         altitude = 1000
#'     )
#'
#' ggpsychro(mollier = TRUE) +
#'     coord_psychro(
#'         tdb_lim = c(0, 50),
#'         hum_lim = c(0, 30),
#'         mollier = TRUE
#'     )
#' @export
coord_psychro <- function(tdb_lim = NULL, hum_lim = NULL,
                          altitude = NULL, units = NULL, mollier = NULL,
                          expand = FALSE, default = TRUE, clip = "on") {
    # TODO: add a `n` param to specify the number of points used to draw
    # saturation line
    ggproto(NULL, CoordPsychro,
        limits = list(tdb = tdb_lim, hum = hum_lim),
        altitude = altitude,
        units = units,
        mollier = mollier,
        expand = expand,
        default = default,
        clip = clip
    )
}

valid_relhum_grid_breaks <- function(breaks) {
    breaks <- remove_na(breaks)
    breaks[breaks > 0 & breaks < 1]
}

psychro_grid_label_spec <- function(labels, type, breaks, scale, units) {
    label <- labels[[type]]
    if (!is.list(label) || !isTRUE(label$show) || !length(breaks)) {
        return(NULL)
    }

    text <- psychro_grid_label_text(label$label, type, breaks, scale, units)
    if (is.null(text) || !length(text)) return(NULL)

    list(
        show = TRUE,
        labels = text,
        label_loc = label$label_loc,
        label_parse = label$label_parse,
        style = label$style
    )
}

psychro_grid_label_text <- function(label, type, breaks, scale, units) {
    if (!isTRUE(label)) return(NULL)

    if (is.null(scale$scale$labels)) return(NULL)

    if (identical(type, "relhum") && is.waive(scale$scale$labels)) {
        return(label_relhum(units = units)(breaks))
    }

    all_labels <- scale$get_labels()
    if (is.null(all_labels)) return(NULL)

    all_breaks <- scale$get_breaks()
    loc <- match_psychro_breaks(breaks, all_breaks)
    all_labels[loc]
}

match_psychro_breaks <- function(x, table, tolerance = 1e-8) {
    vapply(x, function(value) {
        match <- which(abs(table - value) <= tolerance)
        if (length(match)) match[[1L]] else NA_integer_
    }, integer(1))
}

#' @rdname ggpsychro-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordPsychro <- ggproto("CoordPsychro", CoordCartesian,
    setup_params = function(self, data) {
        self$grids <- merge_psychro_grids(self$grids)

        # all parameters will inherit from ggpsychro() if not specified during
        # the construction process
        assert_flag(self$mollier, .var.name = "mollier")
        assert_number(self$altitude, .var.name = "altitude")
        assert_choice(self$units, c("SI", "IP"), .var.name = "units")

        assert_numeric(self$limits$tdb, any.missing = FALSE, all.missing = FALSE,
            len = 2L, unique = TRUE, sorted = TRUE, null.ok = TRUE,
            lower = get_tdb_limits(self$units)[1L], upper = get_tdb_limits(self$units)[2L],
            .var.name = "tdb_lim"
        )
        assert_numeric(self$limits$hum, any.missing = FALSE, all.missing = FALSE,
            len = 2, unique = TRUE, sorted = TRUE, null.ok = TRUE,
            lower = get_hum_limits(self$units)[1], upper = get_hum_limits(self$units)[2],
            .var.name = "hum_lim"
        )

        if (self$mollier) {
            pos_tdb <- "y"
            pos_hum <- "x"
        } else {
            pos_tdb <- "x"
            pos_hum <- "y"
        }

        # calculate pressure
        self$pressure <- with_units(self$units, GetStandardAtmPressure(self$altitude))

        self$limits[pos_tdb] <- list(self$limits$tdb)
        self$limits[pos_hum] <- list(self$limits$hum)

        limits <- self$limits[!(names(self$limits) %in% c("tdb", "hum"))]

        list(pos_tdb = pos_tdb, pos_hum = pos_hum, pressure = self$pressure, limits = limits, units = self$units)
    },

    setup_data = function(self, data, params = list()) {
        return(data)
        # If there is no data specified, this is the case for an empty
        # psychrometric chart. In this case, use the coordinate limits to build
        # a fake data for each layer
        lapply(data, function(d) {
            if (is.waive(d)) {
                len <- lengths(params$limits[c("x", "y")])
                if (all(!len)) return(d)
                d <- as.data.frame(params$limits[c("x", "y")[len > 0]])
            }
            d
        })
    },

    setup_panel_params = function(self, scale_x, scale_y,
                                  scale_rh, scale_wb, scale_vp, scale_sv, scale_en,
                                  params = list()) {
        default_limits <- default_psychro_limits(self$units)
        default_x <- if (self$mollier) default_limits$hum else default_limits$tdb
        default_y <- if (self$mollier) default_limits$tdb else default_limits$hum

        empty_range <- function(range) {
            is.null(range) || length(range) == 0L || anyNA(range)
        }
        choose_range <- function(scale, limit, default) {
            if (!is.null(limit)) {
                return(scale$transform(limit))
            }
            if (!empty_range(scale$range$range)) {
                return(scale$range$range)
            }
            scale$transform(default)
        }

        # When training tdb and hum, the range should be shrunk based on
        # dewpoint and corresponding hum ratio. Missing limits are filled with
        # display defaults so empty psychrometric charts still render.
        lim_x <- choose_range(scale_x, self$limits$x, default_x)
        lim_y <- choose_range(scale_y, self$limits$y, default_y)

        if (self$mollier) {
            lim_tdb <- lim_y
            lim_hum <- lim_x
        } else {
            lim_tdb <- lim_x
            lim_hum <- lim_y
        }

        tdp <- with_units(self$units,
            psychrolib::GetTDewPointFromHumRatio(lim_tdb[1L], lim_hum[1L], self$pressure)
        )
        lim_tdb <- c(max(lim_tdb[1L], tdp), lim_tdb[2L])

        hum <- with_units(self$units,
            psychrolib::GetHumRatioFromTDewPoint(lim_tdb[2L], self$pressure)
        )
        lim_hum <- c(lim_hum[1L], min(lim_hum[2L], hum))

        if (self$mollier) {
            lim_x <- lim_hum
            lim_y <- lim_tdb
        } else {
            lim_x <- lim_tdb
            lim_y <- lim_hum
        }

        if (scale_x$is_empty()) {
            scale_x$train(lim_x)
        }
        if (scale_y$is_empty()) {
            scale_y$train(lim_y)
        }

        if (!is.null(lim_tdb) && !is.null(lim_hum)) {
            # TODO: should handle transform properly
            lim_rh <- cut_oob(with_units(self$units,
                psychrolib::GetRelHumFromHumRatio(rev(lim_tdb), lim_hum, params$pressure)
            ), c(0, 1))
            lim_wb <- with_units(self$units, psychrolib::GetTWetBulbFromHumRatio(lim_tdb, lim_hum, params$pressure))
            lim_vp <- with_units(self$units, psychrolib::GetVapPresFromHumRatio(lim_hum, params$pressure))
            lim_sv <- with_units(self$units, psychrolib::GetMoistAirVolume(lim_tdb, lim_hum, params$pressure))
            lim_en <- with_units(self$units, psychrolib::GetMoistAirEnthalpy(lim_tdb, lim_hum))

            # train scales
            scale_rh$train(lim_rh * 100)
            scale_wb$train(lim_wb)
            scale_vp$train(lim_vp)
            scale_sv$train(lim_sv)
            scale_en$train(lim_en)
        }

        c(
            ggplot2_view_scales_from_scale(scale_x, self$limits$x, self$expand),
            ggplot2_view_scales_from_scale(scale_y, self$limits$y, self$expand),
            ggplot2_view_scales_from_scale(scale_rh, NULL, self$expand),
            ggplot2_view_scales_from_scale(scale_wb, NULL, self$expand),
            ggplot2_view_scales_from_scale(scale_vp, NULL, self$expand),
            ggplot2_view_scales_from_scale(scale_sv, NULL, self$expand),
            ggplot2_view_scales_from_scale(scale_en, NULL, self$expand)
        )
    },

    pos_tdb = function(self) c("x", "y")[c(!self$mollier, self$mollier)],
    pos_hum = function(self) c("x", "y")[c(self$mollier, !self$mollier)],

    range_tdb = function(self, panel_params, cut = FALSE) {
        rng <- panel_params[[paste(self$pos_tdb(), "range", sep = ".")]]
        if (cut) cut_oob(rng, get_tdb_limits(self$units))
        rng
    },

    range_hum = function(self, panel_params, cut = FALSE) {
        rng <- panel_params[[paste(self$pos_hum(), "range", sep = ".")]]
        if (cut) cut_oob(rng, get_hum_limits(self$units))
        rng
    },

    trans_grid_vert = function(self, tdb, type, breaks, range_tdb = NULL, range_hum = NULL, cut = FALSE) {
        n <- length(breaks)
        if (n == 0L) return(NULL)

        if (type != "wetbulb") {
            len <- length(tdb)
            line_breaks <- breaks
            tdb <- rep(tdb, n)
            breaks <- rep(breaks, each = len)
            group <- rep(seq_len(n), each = len)
        } else {
            # make sure wetbulb is lower than drybulb
            lst <- lapply(breaks, function(twb) tdb[tdb >= twb])
            len <- lengths(lst)
            not_empty <- len > 0L
            # make sure twb itself is included
            lst <- lapply(seq_along(lst), function(i) c(lst[[i]], breaks[[i]]))

            # only use the range
            line_breaks <- breaks[not_empty]
            n <- length(breaks[not_empty])
            if (n == 0L) return(NULL)
            tdb <- unlist(lapply(lst[not_empty], base::range), FALSE)
            breaks <- rep(breaks[not_empty], each = 2L)
            group <- rep(seq_len(n), each = 2L)
            len <- 2L
        }

        no_hum_limit <- function(expr) with_units(self$units, with_no_hum_limit(expr))

        hum <- switch(type,
            relhum = no_hum_limit(psychrolib::GetHumRatioFromRelHum(tdb, breaks, self$pressure)),
            wetbulb = no_hum_limit(psychrolib::GetHumRatioFromTWetBulb(tdb, breaks, self$pressure)),
            vappres = no_hum_limit(psychrolib::GetHumRatioFromVapPres(breaks, self$pressure)),
            specvol = no_hum_limit(GetHumRatioFromMoistAirVolumeAndTDryBulb(breaks, tdb, self$pressure)),
            enthalpy = no_hum_limit(GetHumRatioFromEnthalpyAndTDryBulb(breaks, tdb)),
            stop("Invalid grid type found")
        )

        if (cut) hum <- cut_oob(hum, range_hum)

        tdb <- rescale01(tdb, range_tdb)
        hum <- rescale01(hum, range_hum)

        list(
            tdb = tdb, hum = hum, len = len, n = n,
            breaks = line_breaks, value = breaks, group = group
        )
    },

    render_bg = function(self, panel_params, theme) {
        # only process if both tdb and hum ranges have been trained
        if (is.null(panel_params$x$scale$range$range) || is.null(panel_params$y$scale$range$range)) {
            return(ggplot2::ggproto_parent(CoordCartesian, self)$render_bg(panel_params, theme))
        }

        range_tdb <- self$range_tdb(panel_params)
        range_hum <- self$range_hum(panel_params)

        # get initial tdb for grid line
        # NOTE: here we should use the continuous_range instead of the limits
        scale <- panel_params[[self$pos_tdb()]]$scale
        limits <- scale$trans$inverse(panel_params[[self$pos_tdb()]]$continuous_range)
        tdb <- scale$trans$breaks(limits, 100L)

        # calculate corresponding hum ratio at saturation
        hum <- with_units(self$units, psychrolib::GetHumRatioFromRelHum(tdb, 1.0, self$pressure))

        # SATURATION LINE
        # remove points that are above the max hum ratio
        sat_tdb <- tdb[hum <= range_hum[2L]]
        sat_hum <- hum[hum <= range_hum[2L]]
        # make sure line extend to the end of the panel
        sat_tdb <- c(
            if (range_hum[1L] > 0.0) {
                with_units(self$units, GetTDewPointFromHumRatioOnly(range_hum[1L], self$pressure))
            },
            sat_tdb,
            if (range_hum[2L] > 0.0) {
                sat_tdb_max <- with_units(self$units, GetTDewPointFromHumRatioOnly(range_hum[2L], self$pressure))
                sat_tdb_max[(sat_app_end <- sat_tdb_max > max(sat_tdb))]
            }
        )
        sat_hum <- c(
            if(range_hum[1L] > 0.0) range_hum[1L],
            sat_hum,
            if (range_hum[2L] > 0.0) range_hum[2L][sat_app_end]
        )
        sat_tdb <- rescale01(sat_tdb, range_tdb)
        sat_hum <- rescale01(sat_hum, range_hum)
        sat <- list(tdb = sat_tdb, hum = sat_hum, len = length(sat_tdb), n = 1L)

        # RELHUM GRID LINE
        grid_labels <- self$grid_labels
        if (is.null(grid_labels)) grid_labels <- list()

        bk_rh_major <- valid_relhum_grid_breaks(panel_params$relhum$get_breaks())
        bk_rh_minor <- setdiff(valid_relhum_grid_breaks(panel_params$relhum$get_breaks_minor()), bk_rh_major)
        rh_major <- if (psychro_grid_enabled(self$grids, "relhum")) {
            self$trans_grid_vert(tdb, "relhum", bk_rh_major, range_tdb, range_hum)
        }
        rh_minor <- if (psychro_grid_enabled(self$grids, "relhum")) {
            self$trans_grid_vert(tdb, "relhum", bk_rh_minor, range_tdb, range_hum)
        }

        # WETBULB GRID LINE
        bk_twb_major <- remove_na(panel_params$wetbulb$get_breaks())
        bk_twb_minor <- setdiff(remove_na(panel_params$wetbulb$get_breaks_minor()), bk_twb_major)
        twb_major <- if (psychro_grid_enabled(self$grids, "wetbulb")) {
            self$trans_grid_vert(tdb, "wetbulb", bk_twb_major, range_tdb, range_hum)
        }
        twb_minor <- if (psychro_grid_enabled(self$grids, "wetbulb")) {
            self$trans_grid_vert(tdb, "wetbulb", bk_twb_minor, range_tdb, range_hum)
        }

        # VAPPRES GRID LINE
        bk_vap_major <- remove_na(panel_params$vappres$get_breaks())
        bk_vap_minor <- setdiff(remove_na(panel_params$vappres$get_breaks_minor()), bk_vap_major)
        vap_major <- if (psychro_grid_enabled(self$grids, "vappres")) {
            self$trans_grid_vert(tdb, "vappres", bk_vap_major, range_tdb, range_hum)
        }
        vap_minor <- if (psychro_grid_enabled(self$grids, "vappres")) {
            self$trans_grid_vert(tdb, "vappres", bk_vap_minor, range_tdb, range_hum)
        }

        # SPECVOL GRID LINE
        bk_vol_major <- remove_na(panel_params$specvol$get_breaks())
        bk_vol_minor <- setdiff(remove_na(panel_params$specvol$get_breaks_minor()), bk_vol_major)
        vol_major <- if (psychro_grid_enabled(self$grids, "specvol")) {
            self$trans_grid_vert(tdb, "specvol", bk_vol_major, range_tdb, range_hum)
        }
        vol_minor <- if (psychro_grid_enabled(self$grids, "specvol")) {
            self$trans_grid_vert(tdb, "specvol", bk_vol_minor, range_tdb, range_hum)
        }

        # ENTHALPY GRID LINE
        bk_enth_major <- remove_na(panel_params$enthalpy$get_breaks())
        bk_enth_minor <- setdiff(remove_na(panel_params$enthalpy$get_breaks_minor()), bk_enth_major)
        enth_major <- if (psychro_grid_enabled(self$grids, "enthalpy")) {
            self$trans_grid_vert(tdb, "enthalpy", bk_enth_major, range_tdb, range_hum)
        }
        enth_minor <- if (psychro_grid_enabled(self$grids, "enthalpy")) {
            self$trans_grid_vert(tdb, "enthalpy", bk_enth_minor, range_tdb, range_hum)
        }

        labels <- list(
            relhum = psychro_grid_label_spec(grid_labels, "relhum", bk_rh_major, panel_params$relhum, self$units),
            wetbulb = psychro_grid_label_spec(grid_labels, "wetbulb", bk_twb_major, panel_params$wetbulb, self$units),
            vappres = psychro_grid_label_spec(grid_labels, "vappres", bk_vap_major, panel_params$vappres, self$units),
            specvol = psychro_grid_label_spec(grid_labels, "specvol", bk_vol_major, panel_params$specvol, self$units),
            enthalpy = psychro_grid_label_spec(grid_labels, "enthalpy", bk_enth_major, panel_params$enthalpy, self$units)
        )

        guide_grid_psychro(
            theme,
            panel_params[[self$pos_tdb()]]$break_positions_minor(),
            panel_params[[self$pos_tdb()]]$break_positions(),
            panel_params[[self$pos_hum()]]$break_positions_minor(),
            panel_params[[self$pos_hum()]]$break_positions(),
            sat, rh_minor, rh_major, twb_minor, twb_major,
            vap_minor, vap_major, vol_minor, vol_major,
            enth_minor, enth_major, labels, self$mollier
        )
    }
)
