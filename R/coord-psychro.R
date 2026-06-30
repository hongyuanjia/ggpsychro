#' @include comfort-givoni.R
NULL

#' Psychrometric coordinates
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritParams ggpsychro
#' @return A ggplot2 coordinate system object for psychrometric charts.
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
        draw_saturation_fg = TRUE,
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

psychro_format_shr_labels <- function(x) {
    labels <- sprintf("%.1f", x)
    labels[abs(x) <= 1e-8] <- "0"
    labels
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

        sat <- psychro_coord_saturation(self, panel_params)
        if (is.null(sat)) {
            return(ggplot2::ggproto_parent(CoordCartesian, self)$render_bg(panel_params, theme))
        }

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
    },

    render_fg = function(self, panel_params, theme) {
        sat <- if (isFALSE(self$draw_saturation_fg)) {
            NULL
        } else {
            psychro_coord_saturation(self, panel_params)
        }
        border <- ggplot2::ggproto_parent(CoordCartesian, self)$render_fg(
            panel_params, theme
        )
        range_tdb <- self$range_tdb(panel_params)
        range_hum <- self$range_hum(panel_params)

        if (!is.null(sat) && self$mollier) {
            line_x <- sat$hum
            line_y <- sat$tdb
        } else if (!is.null(sat)) {
            line_x <- sat$tdb
            line_y <- sat$hum
        }

        grid::grobTree(
            psychro_protractor_grob(
                self$protractor, theme, self$mollier, range_tdb, range_hum,
                self$units
            ),
            if (!is.null(sat)) {
                ggplot2::element_render(
                    theme, "psychro.panel.grid.saturation",
                    x = line_x, y = line_y
                )
            },
            psychro_coord_extra_fg(self, panel_params, theme),
            border
        )
    }
)

GeomPsychroSaturation <- ggplot2::ggproto(
    "GeomPsychroSaturation", ggplot2::Geom,
    required_aes = character(),
    default_aes = ggplot2::aes(),
    draw_key = ggplot2::draw_key_blank,
    extra_params = c("na.rm", "psychro.theme"),

    draw_panel = function(data, panel_params, coord, psychro.theme = NULL, ...) {
        sat <- psychro_coord_saturation(coord, panel_params)
        if (is.null(sat)) {
            return(grid::nullGrob())
        }

        if (coord$mollier) {
            line_x <- sat$hum
            line_y <- sat$tdb
        } else {
            line_x <- sat$tdb
            line_y <- sat$hum
        }

        ggplot2::element_render(
            psychro.theme %||% coord$psychro_theme %||% ggplot2::theme_get(),
            "psychro.panel.grid.saturation",
            x = line_x, y = line_y
        )
    }
)

psychro_coord_extra_fg <- function(coord, panel_params, theme) {
    foreground <- coord$comfort_foreground
    if (!length(foreground)) {
        return(grid::nullGrob())
    }

    grobs <- lapply(foreground, function(spec) {
        switch(spec$type,
            givoni_mean_outdoor = psychro_coord_givoni_mean_outdoor_grob(
                coord, panel_params, spec
            ),
            heat_index_labels = psychro_coord_heat_index_label_grob(
                coord, panel_params, spec
            ),
            grid::nullGrob()
        )
    })
    do.call(grid::grobTree, grobs)
}

psychro_coord_panel_polygon <- function(coord, panel_params) {
    sat <- psychro_coord_saturation(coord, panel_params)
    if (is.null(sat)) {
        return(NULL)
    }
    psychro_panel_polygon(sat, coord$mollier)
}

psychro_coord_panel_grob <- function(coord, panel_params) {
    panel <- psychro_coord_panel_polygon(coord, panel_params)
    if (is.null(panel)) {
        return(NULL)
    }
    grid::polygonGrob(
        panel$x, panel$y, gp = grid::gpar(col = NA, fill = NA),
        name = "psychro-panel-clip"
    )
}

psychro_clip_grob_to_panel <- function(grob, coord, panel_params) {
    panel <- psychro_coord_panel_grob(coord, panel_params)
    if (is.null(panel) || inherits(grob, c("nullGrob", "zeroGrob"))) {
        return(grob)
    }
    split <- psychro_split_styled_grob(grob)
    if (length(split) > 1L) {
        clipped <- lapply(split, function(child) {
            psychro_polyclip_grob(child, panel)
        })
        return(do.call(grid::grobTree, clipped))
    }
    psychro_polyclip_grob(grob, panel)
}

psychro_clip_textpath_lines_to_panel <- function(grob, coord, panel_params) {
    panel <- psychro_coord_panel_grob(coord, panel_params)
    if (is.null(panel) || inherits(grob, c("nullGrob", "zeroGrob"))) {
        return(grob)
    }
    grob$psychro_panel <- panel
    class(grob) <- c("psychro_textpath_clip", class(grob))
    grob
}

#' @method makeContent psychro_textpath_clip
#' @importFrom grid makeContent
#' @export
makeContent.psychro_textpath_clip <- function(x) {
    panel <- x$psychro_panel
    x$psychro_panel <- NULL
    class(x) <- setdiff(class(x), "psychro_textpath_clip")

    # Textpath must lay out glyphs in its normal grid drawing context. Clipping
    # the input data, or forcing the grob from a wrapper, can flip contour labels;
    # therefore we expand the textpath first and only clip its stroke children.
    x <- grid::makeContent(x)
    class(x) <- setdiff(class(x), "textpath")
    if (is.null(panel)) {
        return(x)
    }
    psychro_clip_textpath_lines_grob(x, panel)
}

psychro_clip_textpath_lines_grob <- function(grob, panel) {
    if (inherits(grob, "textpath")) {
        grob <- grid::makeContent(grob)
        class(grob) <- setdiff(class(grob), "textpath")
    }
    if (psychro_line_grob(grob)) {
        split <- psychro_split_styled_grob(grob)
        clipped <- lapply(split, psychro_polyclip_grob, panel = panel)
        if (length(clipped) == 1L) {
            return(clipped[[1L]])
        }
        return(do.call(grid::grobTree, clipped))
    }
    if (!is.null(grob$children)) {
        children <- as.list(grob$children)
        children <- lapply(
            children, psychro_clip_textpath_lines_grob, panel = panel
        )
        grob$children <- do.call(grid::gList, children)
        grob$childrenOrder <- names(children)
    }
    if (!is.null(grob$grobs)) {
        grob$grobs <- lapply(
            grob$grobs, psychro_clip_textpath_lines_grob, panel = panel
        )
    }
    grob
}

psychro_polyclip_grob <- function(grob, panel) {
    if (psychro_line_grob(grob)) {
        return(gridGeometry::polyclipGrob(
            grob, panel, "intersection",
            closedFn = psychro_xy_list_to_null,
            name = grob$name,
            gp = grob$gp %||% grid::gpar()
        ))
    }
    gridGeometry::polyclipGrob(grob, panel, "intersection", name = grob$name)
}

psychro_line_grob <- function(grob) {
    inherits(grob, c("polyline", "segments", "lines"))
}

psychro_xy_list_to_null <- function(...) {
    grid::nullGrob()
}

psychro_split_styled_grob <- function(grob) {
    if (inherits(grob, "pathgrob")) {
        return(psychro_split_path_grob(grob))
    }
    if (inherits(grob, "polyline")) {
        return(psychro_split_polyline_grob(grob))
    }
    if (inherits(grob, "polygon")) {
        return(psychro_split_polygon_grob(grob))
    }
    list(grob)
}

psychro_split_path_grob <- function(grob) {
    path_id <- grob$pathId %||% grob$id
    if (is.null(path_id)) {
        return(list(grob))
    }
    path_ids <- unique(path_id)
    n <- length(path_ids)
    if (n <= 1L) {
        return(list(grob))
    }

    lapply(seq_along(path_ids), function(i) {
        keep <- path_id == path_ids[[i]]
        id <- grob$id[keep]
        id <- match(id, unique(id))
        grid::pathGrob(
            grob$x[keep], grob$y[keep],
            id = id,
            pathId = rep(1L, sum(keep)),
            rule = grob$rule %||% "winding",
            name = paste0(grob$name %||% "path", "-", i),
            gp = psychro_gpar_at(grob$gp, i, n)
        )
    })
}

psychro_split_polyline_grob <- function(grob) {
    id <- psychro_grob_id(grob)
    if (is.null(id)) {
        return(list(grob))
    }
    ids <- unique(id)
    n <- length(ids)
    if (n <= 1L) {
        return(list(grob))
    }

    lapply(seq_along(ids), function(i) {
        keep <- id == ids[[i]]
        grid::polylineGrob(
            grob$x[keep], grob$y[keep],
            id = rep(1L, sum(keep)),
            arrow = grob$arrow,
            name = paste0(grob$name %||% "polyline", "-", i),
            gp = psychro_gpar_at(grob$gp, i, n)
        )
    })
}

psychro_split_polygon_grob <- function(grob) {
    id <- psychro_grob_id(grob)
    if (is.null(id)) {
        return(list(grob))
    }
    ids <- unique(id)
    n <- length(ids)
    if (n <= 1L) {
        return(list(grob))
    }

    lapply(seq_along(ids), function(i) {
        keep <- id == ids[[i]]
        grid::polygonGrob(
            grob$x[keep], grob$y[keep],
            id = rep(1L, sum(keep)),
            name = paste0(grob$name %||% "polygon", "-", i),
            gp = psychro_gpar_at(grob$gp, i, n)
        )
    })
}

psychro_grob_id <- function(grob) {
    if (!is.null(grob$id)) {
        return(grob$id)
    }
    if (!is.null(grob$id.lengths)) {
        return(rep(seq_along(grob$id.lengths), grob$id.lengths))
    }
    NULL
}

psychro_gpar_at <- function(gp, i, n) {
    if (is.null(gp)) {
        return(grid::gpar())
    }
    args <- lapply(as.list(gp), function(value) {
        if (length(value) == n) value[[i]] else value
    })
    do.call(grid::gpar, args)
}

psychro_filter_data_to_panel <- function(data, panel_params, coord) {
    if (!nrow(data) || !all(c("x", "y") %in% names(data))) {
        return(data)
    }
    panel <- psychro_coord_panel_polygon(coord, panel_params)
    if (is.null(panel)) {
        return(data)
    }
    transformed <- coord$transform(data, panel_params)
    keep <- psychro_inside_polygon(transformed$x, transformed$y, panel$x, panel$y)
    data[keep, , drop = FALSE]
}

psychro_clip_polygon_data_to_panel <- function(data, panel_params, coord) {
    if (!nrow(data) || !all(c("x", "y", "group") %in% names(data))) {
        return(data)
    }
    panel <- psychro_coord_panel_polygon_data(coord, panel_params)
    if (is.null(panel)) {
        return(data)
    }

    group <- data$group
    if ("subgroup" %in% names(data)) {
        group <- interaction(group, data$subgroup, drop = TRUE, lex.order = TRUE)
    }
    pieces <- split(data, group)

    out <- list()
    group_id <- 0L
    for (piece in pieces) {
        if (nrow(piece) < 3L) {
            next
        }
        clipped <- polyclip::polyclip(
            list(list(x = piece$x, y = piece$y)),
            list(panel),
            op = "intersection",
            fillA = "evenodd",
            fillB = "nonzero",
            closed = TRUE
        )
        if (!length(clipped)) {
            next
        }
        for (poly in clipped) {
            if (length(poly$x) < 3L || length(poly$y) < 3L) {
                next
            }
            group_id <- group_id + 1L
            clipped_piece <- piece[rep(1L, length(poly$x)), , drop = FALSE]
            clipped_piece$x <- poly$x
            clipped_piece$y <- poly$y
            clipped_piece$group <- group_id
            if ("subgroup" %in% names(clipped_piece)) {
                clipped_piece$subgroup <- 1L
            }
            out[[length(out) + 1L]] <- clipped_piece
        }
    }

    if (!length(out)) {
        return(data[0L, , drop = FALSE])
    }
    do.call(rbind, out)
}

psychro_coord_panel_polygon_data <- function(coord, panel_params) {
    sat <- psychro_coord_saturation_native(coord, panel_params)
    if (is.null(sat)) {
        return(NULL)
    }
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)

    if (coord$mollier) {
        return(list(
            x = c(range_hum[1L], range_hum[1L], range_hum[2L],
                rev(sat$hum), sat$hum[1L]),
            y = c(range_tdb[1L], range_tdb[2L], range_tdb[2L],
                rev(sat$tdb), range_tdb[1L])
        ))
    }

    list(
        x = c(range_tdb[1L], range_tdb[1L], sat$tdb,
            range_tdb[2L], range_tdb[2L]),
        y = c(range_hum[1L], sat$hum[1L], sat$hum,
            range_hum[2L], range_hum[1L])
    )
}

psychro_coord_saturation_native <- function(coord, panel_params) {
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)

    scale <- panel_params[[coord$pos_tdb()]]$scale
    limits <- scale$trans$inverse(panel_params[[coord$pos_tdb()]]$continuous_range)
    tdb <- scale$trans$breaks(limits, 100L)
    hum <- with_units(
        coord$units,
        psychrolib::GetHumRatioFromRelHum(tdb, 1.0, coord$pressure)
    )

    sat_tdb <- tdb[hum <= range_hum[2L]]
    sat_hum <- hum[hum <= range_hum[2L]]
    if (!length(sat_tdb)) {
        return(NULL)
    }

    sat_app_end <- FALSE
    sat_tdb <- c(
        if (range_hum[1L] > 0.0) {
            with_units(coord$units, GetTDewPointFromHumRatioOnly(
                range_hum[1L], coord$pressure
            ))
        },
        sat_tdb,
        if (range_hum[2L] > 0.0) {
            sat_tdb_max <- with_units(coord$units, GetTDewPointFromHumRatioOnly(
                range_hum[2L], coord$pressure
            ))
            sat_app_end <- sat_tdb_max > max(sat_tdb)
            sat_tdb_max[sat_app_end]
        }
    )
    sat_hum <- c(
        if (range_hum[1L] > 0.0) range_hum[1L],
        sat_hum,
        if (range_hum[2L] > 0.0) range_hum[2L][sat_app_end]
    )

    list(tdb = sat_tdb, hum = sat_hum)
}

psychro_coord_givoni_mean_outdoor_grob <- function(coord, panel_params, spec) {
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)
    mean_si <- comfort_to_si_temp(spec$strategy$mean_outdoor,
        spec$strategy$units)
    tdb <- comfort_from_si_temp(mean_si, coord$units)
    if (!is.finite(tdb) || tdb < range_tdb[[1L]] || tdb > range_tdb[[2L]]) {
        return(grid::nullGrob())
    }

    hum_sat <- with_units(coord$units,
        psychrolib::GetHumRatioFromRelHum(tdb, 1, coord$pressure)
    )
    if (!is.finite(hum_sat) || hum_sat >= range_hum[[2L]]) {
        return(grid::nullGrob())
    }
    hum_extension <- max(diff(range_hum) * 0.08, diff(range_hum) / 25)
    hum_top <- min(range_hum[[2L]], hum_sat + hum_extension)
    if (!is.finite(hum_top) || hum_top <= hum_sat) {
        return(grid::nullGrob())
    }
    hum_label <- min(hum_top, hum_sat + (hum_top - hum_sat) * 0.65)

    if (coord$mollier) {
        line_x <- rescale01(c(hum_sat, hum_top), range_hum)
        line_y <- rep(rescale01(tdb, range_tdb), 2L)
        label_x <- rescale01(hum_label, range_hum)
        label_y <- line_y[[1L]]
        label_rot <- comfort_givoni_mean_outdoor_label_angle(TRUE)
        label_vjust <- comfort_givoni_mean_outdoor_label_vjust(TRUE)
    } else {
        line_x <- rep(rescale01(tdb, range_tdb), 2L)
        line_y <- rescale01(c(hum_sat, hum_top), range_hum)
        label_x <- line_x[[1L]]
        label_y <- rescale01(hum_label, range_hum)
        label_rot <- comfort_givoni_mean_outdoor_label_angle(FALSE)
        label_vjust <- comfort_givoni_mean_outdoor_label_vjust(FALSE)
    }

    label_temp <- comfort_from_si_temp(mean_si, coord$units)
    unit_label <- if (coord$units == "IP") "\u00b0F" else "\u00b0C"
    label <- sprintf("%.1f %s", label_temp, unit_label)
    colour <- spec$colour %||% "#444444"
    linewidth <- spec$linewidth %||% 0.8
    label_size <- spec$label_size %||% 2.7

    grid::grobTree(
        grid::linesGrob(
            x = line_x, y = line_y,
            gp = grid::gpar(
                col = colour,
                lwd = linewidth * ggplot2::.pt,
                lty = spec$linetype %||% "dotted"
            )
        ),
        if (isTRUE(spec$show_label)) {
            grid::textGrob(
                label, x = label_x, y = label_y, rot = label_rot,
                hjust = 0.5, vjust = label_vjust,
                gp = grid::gpar(
                    col = colour,
                    fontsize = label_size * ggplot2::.pt,
                    fontface = spec$fontface %||% "bold"
                )
            )
        } else {
            grid::nullGrob()
        }
    )
}

psychro_coord_heat_index_label_grob <- function(coord, panel_params, spec) {
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)
    data <- comfort_heat_index_label_data(
        spec$model, comfort_grid_n(spec$n), coord$units, coord$pressure,
        coord$mollier, range_tdb, amplify_hum(range_hum, coord$units)
    )
    if (!nrow(data)) {
        return(grid::nullGrob())
    }

    data <- coord$transform(data, panel_params)
    colour <- psychro_grid_alpha(spec$colour %||% "#444444", spec$alpha)
    grid::textGrob(
        data$label, x = data$x, y = data$y, rot = data$angle,
        hjust = spec$hjust %||% 0.5,
        vjust = spec$vjust %||% 0.5,
        gp = grid::gpar(
            col = colour,
            fontsize = (spec$size %||% 3) * ggplot2::.pt,
            fontfamily = spec$family %||% "",
            fontface = spec$fontface %||% "bold",
            lineheight = spec$lineheight %||% 1.2
        ),
        name = "psychro-heat-index-labels"
    )
}

psychro_coord_saturation <- function(coord, panel_params) {
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)
    sat <- psychro_coord_saturation_native(coord, panel_params)
    if (is.null(sat)) return(NULL)

    list(
        tdb = rescale01(sat$tdb, range_tdb),
        hum = rescale01(sat$hum, range_hum),
        len = length(sat$tdb),
        n = 1L
    )
}
