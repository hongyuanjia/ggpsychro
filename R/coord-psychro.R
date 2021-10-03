#' Psychrometric coordinates
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritParams ggpsychro
#' @export
coord_psychro <- function(tdb_lim = NULL, hum_lim = NULL,
                          altitude = NULL, units = NULL, mollier = NULL,
                          expand = TRUE, default = TRUE, clip = "on") {
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
        self$limits[[pos_tdb]] <- self$limits$tdb
        self$limits[[pos_hum]] <- self$limits$hum

        data
    },

    render_bg = function(self, panel_params, theme) {
        if (panel_params$x$scale$is_empty() || panel_params$y$scale$is_empty()) {
            return(CoordCartesian$render_bg(panel_params = panel_params, theme = theme))
        }

        if (self$mollier) {
            pos_tdb <- "y"
            pos_hum <- "x"
        } else {
            pos_tdb <- "x"
            pos_hum <- "y"
        }

        # NOTE: the saturation line should extend to the expanded scales while
        # the grid lines shouldn't
        range_tdb <- panel_params[[paste(pos_tdb, "range", sep = ".")]]
        range_hum <- panel_params[[paste(pos_hum, "range", sep = ".")]]
        # NOTE: if expand is set to TRUE, it is possible that the limits are
        # invalid exclude invalid values here
        range_tdb_cut <- cut_oob(range_tdb, get_tdb_limits(self$units))
        range_hum_cut <- cut_oob(range_hum, get_hum_limits(self$units))

        # the limits value should have already been validated when calling
        # 'ggpsychro()'
        lim_tdb <- panel_params[[pos_tdb]]$limits
        lim_hum <- panel_params[[pos_hum]]$limits

        # get breaks based on limits
        tdb_minor <- remove_na(panel_params[[pos_tdb]]$minor_breaks)
        tdb_major <- remove_na(panel_params[[pos_tdb]]$breaks)
        hum_minor <- remove_na(panel_params[[pos_hum]]$minor_breaks)
        hum_major <- remove_na(panel_params[[pos_hum]]$breaks)

        # remove duplicates from minor breaks
        tdb_minor <- setdiff(tdb_minor, tdb_major)
        hum_minor <- setdiff(hum_minor, hum_major)

        # temporarily set major break numbers to the max of 200 and number of
        # breaks
        old_bks_tdb <- panel_params[[pos_tdb]]$scale$n.breaks
        n_breaks <- max(c(200L, length(tdb_minor), length(tdb_major), length(hum_minor), length(hum_major)))
        panel_params[[pos_tdb]]$scale$n.breaks <- n_breaks

        # get 200 breaks for tdb for drawing satuation line
        # NOTE: Normal breaks are generated based on the limits NOT the range (i.e.
        # limits with expansion). Here we use the range of drybulb
        if (!length(tdb <- remove_na(panel_params[[pos_tdb]]$scale$get_breaks(limits = range_tdb_cut)))) {
            # in case 'breaks_minor' is set to NULL
            tdb <- remove_na(panel_params[[pos_tdb]]$scale$trans$breaks(range_tdb_cut, 200))
        }

        # make sure all drybulb on the grids have corresponding dewpoints
        tdb <- unique(c(tdb_minor, tdb_major, tdb))
        tdb <- sort(tdb)

        # reset to the original n.breaks
        panel_params[[pos_tdb]]$scale$n.breaks <- old_bks_tdb

        # calculate pressure
        pressure <- with_units(self$units, GetStandardAtmPressure(self$altitude))

        # calculate corresponding hum ratio at satuation
        sat_minor <- with_units(self$units, psychrolib::GetHumRatioFromRelHum(tdb_minor, 1.0, pressure))
        sat_major <- with_units(self$units, psychrolib::GetHumRatioFromRelHum(tdb_major, 1.0, pressure))
        sat       <- with_units(self$units, psychrolib::GetHumRatioFromRelHum(tdb, 1.0, pressure))
        # cut out-of-bounds for grid lines
        sat_minor <- cut_oob(amplify_hum(sat_minor, self$units), range_hum_cut)
        sat_major <- cut_oob(amplify_hum(sat_major, self$units), range_hum_cut)
        # remove out-of-bounds for saturation line
        sat <- amplify_hum(sat, self$units)

        # make sure the satuation line starts from the lower limit and extends to
        # the upper limit
        #
        # use the 1/10 of the min hum ratio step as tolerance
        TOL <- min(diff(sat[!is_oob(sat, range_hum_cut)])) * 0.1

        # make sure the satuation line starts from the beginning
        if (min(sat) <= range_hum_cut[1L]) {
            dif <- sat - range_hum_cut[1L]
            # select the closest one if there are points that satisfy the tolerance
            if (length(ind <- which(abs(dif) < TOL))) {
                ind <- ind[which.min(abs(dif[ind]))]
                tdb_first <- tdb[ind]
            } else {
                # select the nearest two points
                tdb_start <- tdb[which(dif < 0)[length(which(dif < 0))]]
                tdb_end <- tdb[which(dif > 0)[1L]]

                # generate 10 drybulb points
                tdb_in <- seq(tdb_start, tdb_end, length.out = 10)
                sat_out <- with_units(self$units, psychrolib::GetHumRatioFromRelHum(tdb_in, 1.0, pressure))
                sat_out <- amplify_hum(sat_out, self$units)

                # directly select the closest one
                tdb_first <- tdb_in[which.min(abs(sat_out - range_hum[1L]))]
            }

            # include the calculated last point
            tdb <- c(tdb_first, tdb)
            sat <- c(range_hum_cut[1L], sat)
        }

        # make sure the satuation line extend to the end
        if (max(sat) >= range_hum[2L]) {
            dif <- sat - range_hum[2L]
            # select the closest one if there are points that satisfy the tolerance
            if (length(ind <- which(abs(dif) < TOL))) {
                ind <- ind[which.min(abs(dif[ind]))]
                tdb_last <- tdb[ind]
            } else {
                # select the nearest two points
                tdb_start <- tdb[which(dif < 0)[length(which(dif < 0))]]
                tdb_end <- tdb[which(dif > 0)[1L]]

                # generate 50 drybulb points
                tdb_in <- seq(tdb_start, tdb_end, length.out = 50)
                sat_out <- with_units(self$units, psychrolib::GetHumRatioFromRelHum(tdb_in, 1.0, pressure))
                sat_out <- amplify_hum(sat_out, self$units)

                # directly select the closest one
                tdb_last <- tdb_in[which.min(abs(sat_out - range_hum[2L]))]
            }

            # include the calculated last point
            tdb <- c(tdb, tdb_last)
            sat <- c(sat, range_hum[2L])
        }

        # remove out-of-bounds
        tdb <- tdb[!is_oob(sat, range_hum)]
        sat <- sat[!is_oob(sat, range_hum)]

        # calculate corresponding dewpoint for drybulb lines
        tdp_minor <- with_units(
            self$units,
            psychrolib::GetTDewPointFromHumRatio(
                lim_tdb[2L], narrow_hum(hum_minor, self$units), pressure
            )
        )
        tdp_major <- with_units(
            self$units,
            psychrolib::GetTDewPointFromHumRatio(
                lim_tdb[2L], narrow_hum(hum_major, self$units), pressure
            )
        )
        # cut out-of-bounds for grid lines
        tdp_minor <- cut_oob(tdp_minor, range_tdb)
        tdp_major <- cut_oob(tdp_major, range_tdb)

        # rescale to native units
        l <- list()
        l$tdb.minor  <- rescale01(tdb_minor, range_tdb)
        l$sat.minor  <- rescale01(sat_minor, range_hum)

        l$tdb.major  <- rescale01(tdb_major, range_tdb)
        l$sat.major  <- rescale01(sat_major, range_hum)

        l$tdb        <- rescale01(tdb, range_tdb)
        l$sat        <- rescale01(sat, range_hum)

        l$hum.minor  <- rescale01(hum_minor, range_hum)
        l$tdp.minor  <- rescale01(tdp_minor, range_tdb)

        l$hum.major  <- rescale01(hum_major, range_hum)
        l$tdp.major  <- rescale01(tdp_major, range_tdb)

        if (pos_tdb == "x") {
            names(l)[names(l) == "tdb"] <- "x"
            names(l)[names(l) == "sat"] <- "xy"
            names(l) <- gsub("^tdb", "x", names(l))
            names(l) <- gsub("^sat", "xy", names(l))
            names(l) <- gsub("^hum", "y", names(l))
            names(l) <- gsub("^tdp", "yx", names(l))
        } else {
            names(l)[names(l) == "sat"] <- "x"
            names(l)[names(l) == "tdb"] <- "xy"
            names(l) <- gsub("^tdb", "y", names(l))
            names(l) <- gsub("^sat", "yx", names(l))
            names(l) <- gsub("^hum", "x", names(l))
            names(l) <- gsub("^tdp", "xy", names(l))
        }

        guide_grid_psychro(theme,
                  x = l$x,               y = l$xy,
            x.minor = l$x.minor,  xy.minor = l$xy.minor,
            x.major = l$x.major,  xy.major = l$xy.major,
            y.minor = l$y.minor,  yx.minor = l$yx.minor,
            y.major = l$y.major,  yx.major = l$yx.major,
            self$mollier
        )
    }
)
