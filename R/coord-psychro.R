#' Psychrometric coordinates
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritParams ggpsychro
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

#' @rdname ggpsychro-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordPsychro <- ggproto("CoordPsychro", CoordCartesian,
    setup_params = function(self, data) {
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
        # when training tdb and hum, the range should be shrinked based on
        # dewpoint and corresponding hum ratio
        if (self$mollier) {
            lim_tdb <- scale_y$range$range
            lim_hum <- scale_x$range$range
        } else {
            lim_tdb <- scale_x$range$range
            lim_hum <- scale_y$range$range
        }

        if (!is.null(self$limits$x) && !is.null(self$limits$y)) {
            lim_x <- scale_x$transform(self$limits$x)
            lim_y <- scale_y$transform(self$limits$y)

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
            scale_rh$train(lim_rh)
            scale_wb$train(lim_wb)
            scale_vp$train(lim_vp)
            scale_sv$train(lim_sv)
            scale_en$train(lim_en)
        }

        c(
            ggplot2:::view_scales_from_scale(scale_x, self$limits$x, self$expand),
            ggplot2:::view_scales_from_scale(scale_y, self$limits$y, self$expand),
            ggplot2:::view_scales_from_scale(scale_rh, NULL, self$expand),
            ggplot2:::view_scales_from_scale(scale_wb, NULL, self$expand),
            ggplot2:::view_scales_from_scale(scale_vp, NULL, self$expand),
            ggplot2:::view_scales_from_scale(scale_sv, NULL, self$expand),
            ggplot2:::view_scales_from_scale(scale_en, NULL, self$expand)
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
            tdb <- rep(tdb, n)
            breaks <- rep(breaks, each = len)
        } else {
            # make sure wetbulb is lower than drybulb
            lst <- lapply(breaks, function(twb) tdb[tdb >= twb])
            len <- lengths(lst)
            not_empty <- len > 0L
            # make sure twb itself is included
            lst <- lapply(seq_along(lst), function(i) c(lst[[i]], breaks[[i]]))

            # only use the range
            n <- length(breaks[not_empty])
            tdb <- unlist(lapply(lst[not_empty], base::range), FALSE)
            breaks <- rep(breaks[not_empty], each = 2L)
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

        list(tdb = tdb, hum = hum, len = len, n = n)
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
        bk_rh_major <- remove_na(panel_params$relhum$get_breaks())
        bk_rh_minor <- setdiff(remove_na(panel_params$relhum$get_breaks_minor()), bk_rh_major)
        rh_major <- self$trans_grid_vert(tdb, "relhum", bk_rh_major, range_tdb, range_hum)
        rh_minor <- self$trans_grid_vert(tdb, "relhum", bk_rh_minor, range_tdb, range_hum)

        # WETBULB GRID LINE
        bk_twb_major <- remove_na(panel_params$wetbulb$get_breaks())
        bk_twb_minor <- setdiff(remove_na(panel_params$wetbulb$get_breaks_minor()), bk_twb_major)
        twb_major <- self$trans_grid_vert(tdb, "wetbulb", bk_twb_major, range_tdb, range_hum)
        twb_minor <- self$trans_grid_vert(tdb, "wetbulb", bk_twb_minor, range_tdb, range_hum)

        # VAPPRES GRID LINE
        bk_vap_major <- remove_na(panel_params$vappres$get_breaks())
        bk_vap_minor <- setdiff(remove_na(panel_params$vappres$get_breaks_minor()), bk_vap_major)
        vap_major <- self$trans_grid_vert(tdb, "vappres", bk_vap_major, range_tdb, range_hum)
        vap_minor <- self$trans_grid_vert(tdb, "vappres", bk_vap_minor, range_tdb, range_hum)

        # SPECVOL GRID LINE
        bk_vol_major <- remove_na(panel_params$specvol$get_breaks())
        bk_vol_minor <- setdiff(remove_na(panel_params$specvol$get_breaks_minor()), bk_vol_major)
        vol_major <- self$trans_grid_vert(tdb, "specvol", bk_vol_major, range_tdb, range_hum)
        vol_minor <- self$trans_grid_vert(tdb, "specvol", bk_vol_minor, range_tdb, range_hum)

        # ENTHALPY GRID LINE
        bk_enth_major <- remove_na(panel_params$enthalpy$get_breaks())
        bk_enth_minor <- setdiff(remove_na(panel_params$enthalpy$get_breaks_minor()), bk_enth_major)
        enth_major <- self$trans_grid_vert(tdb, "enthalpy", bk_enth_major, range_tdb, range_hum)
        enth_minor <- self$trans_grid_vert(tdb, "enthalpy", bk_enth_minor, range_tdb, range_hum)

        guide_grid_psychro(
            theme,
            panel_params[[self$pos_tdb()]]$break_positions_minor(),
            panel_params[[self$pos_tdb()]]$break_positions(),
            panel_params[[self$pos_hum()]]$break_positions_minor(),
            panel_params[[self$pos_hum()]]$break_positions(),
            sat, rh_minor, rh_major, twb_minor, twb_major,
            vap_minor, vap_major, vol_minor, vol_major,
            enth_minor, enth_major, self$mollier
        )
    }
)
