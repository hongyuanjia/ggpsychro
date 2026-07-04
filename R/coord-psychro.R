#' @include comfort-givoni.R
NULL

#' Psychrometric coordinates
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritParams ggpsychro
#' @param altitude A single number of altitude in m \[SI\] or ft \[IP\]. If
#'   `NULL`, inherits the altitude from the parent [ggpsychro()] plot.
#' @param units Unit system, either `"SI"` or `"IP"`. If `NULL`, inherits the
#'   unit system from the parent [ggpsychro()] plot.
#' @param mollier If `TRUE`, use Mollier chart coordinates. If `NULL`, inherits
#'   the chart type from the parent [ggpsychro()] plot.
#' @param expand If `TRUE`, add a small expansion factor to the limits. Defaults
#'   to `FALSE` for psychrometric charts.
#' @param default Is this the default coordinate system? Defaults to `TRUE` so
#'   replacing the coordinate system created by [ggpsychro()] does not emit a
#'   ggplot2 replacement message.
#' @return A ggplot2 coordinate system object for psychrometric charts.
#' @details
#' `coord_psychro()` is normally used with a [ggpsychro()] plot. When
#' `altitude`, `units`, or `mollier` is `NULL`, the value is inherited from the
#' parent plot. Supply these arguments explicitly when using the coordinate
#' system outside that path.
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
coord_psychro <- function(
    tdb_lim = NULL,
    hum_lim = NULL,
    altitude = NULL,
    units = NULL,
    mollier = NULL,
    expand = FALSE,
    default = TRUE,
    clip = "on"
) {
    # TODO: add a `n` param to specify the number of points used to draw
    # saturation line
    ggproto(
        NULL,
        CoordPsychro,
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

# Keep relative-humidity guide breaks inside the drawable psychrometric field.
coord_psy__relhum_grid_breaks <- function(breaks) {
    breaks <- remove_na(breaks)
    breaks[breaks > 0 & breaks < 1]
}

# Build the label specification consumed by the psychrometric grid guide.
coord_psy__grid_label_spec <- function(labels, type, breaks, scale, units) {
    label <- labels[[type]]
    if (!is.list(label) || !isTRUE(label$show) || !length(breaks)) {
        return(NULL)
    }

    text <- coord_psy__grid_label_text(label$label, type, breaks, scale, units)
    if (is.null(text) || !length(text)) {
        return(NULL)
    }

    list(
        show = TRUE,
        labels = text,
        label_loc = label$label_loc,
        label_parse = label$label_parse,
        style = label$style
    )
}

# Resolve grid labels against the scale breaks that survived coord filtering.
coord_psy__grid_label_text <- function(label, type, breaks, scale, units) {
    if (!isTRUE(label)) {
        return(NULL)
    }

    if (is.null(scale$scale$labels)) {
        return(NULL)
    }

    if (identical(type, "relhum") && is.waive(scale$scale$labels)) {
        return(label_relhum(units = units)(breaks))
    }

    all_labels <- scale$get_labels()
    if (is.null(all_labels)) {
        return(NULL)
    }

    all_breaks <- scale$get_breaks()
    loc <- guide__match_break_values(breaks, all_breaks)
    all_labels[loc]
}

# Build major/minor grid data through one path so every psychrometric grid
# family uses the same break filtering, enable flag, and label-break source.
coord_psy__grid_lines <- function(
    coord,
    panel_params,
    tdb,
    range_tdb,
    range_hum
) {
    grid_types <- c("relhum", "wetbulb", "vappres", "specvol", "enthalpy")
    stats::setNames(
        lapply(grid_types, function(type) {
            breaks <- coord_psy__grid_breaks(panel_params, type)
            list(
                minor = if (psychro_grid_enabled(coord$grids, type)) {
                    coord$trans_grid_vert(
                        tdb,
                        type,
                        breaks$minor,
                        range_tdb,
                        range_hum,
                        panel_params = panel_params
                    )
                },
                major = if (psychro_grid_enabled(coord$grids, type)) {
                    coord$trans_grid_vert(
                        tdb,
                        type,
                        breaks$major,
                        range_tdb,
                        range_hum,
                        panel_params = panel_params
                    )
                },
                major_breaks = breaks$major_breaks
            )
        }),
        grid_types
    )
}

# Convert internally generated psychrometric coordinates into the active scale
# space before ggplot2's coordinate transform sees them.
coord_psy__scale_xy <- function(coord, panel_params, data) {
    pos_tdb <- coord$pos_tdb()
    pos_hum <- coord$pos_hum()
    data[[pos_tdb]] <- coord$scale_tdb(panel_params, data[[pos_tdb]])
    data[[pos_hum]] <- coord$scale_hum(panel_params, data[[pos_hum]])
    data
}

# Grid breaks are trained in scale space, but psychrolib needs physical values;
# keep both forms so geometry and labels cannot drift apart.
coord_psy__grid_break_data <- function(scale, type, breaks) {
    scale_breaks <- remove_na(breaks)
    if (!length(scale_breaks)) {
        return(list(input = numeric(), scale = numeric()))
    }

    input <- psychro_scale_inverse(scale, scale_breaks)
    if (identical(type, "relhum")) {
        input <- input / 100
    }

    # Relative-humidity grid lines exclude the saturation and zero curves; other
    # psychrometric variables only need missing-value removal.
    keep <- !is.na(input)
    if (identical(type, "relhum")) {
        keep <- keep & input > 0 & input < 1
    }

    list(input = input[keep], scale = scale_breaks[keep])
}

# Keep scale-space breaks for labels while returning psychrolib-ready inputs for
# grid geometry generation.
coord_psy__grid_breaks <- function(panel_params, type) {
    scale <- panel_params[[type]]
    major <- coord_psy__grid_break_data(scale, type, scale$get_breaks())
    minor <- coord_psy__grid_break_data(scale, type, scale$get_breaks_minor())
    minor_keep <- is.na(match(minor$scale, major$scale))

    list(
        major = major$input,
        minor = minor$input[minor_keep],
        major_breaks = major$scale
    )
}

# Label specs are derived from the same major breaks used to draw grid lines so
# labels cannot drift from the visible guide geometry.
coord_psy__grid_labels <- function(grid, labels, panel_params, units) {
    grid_types <- names(grid)
    stats::setNames(
        lapply(grid_types, function(type) {
            coord_psy__grid_label_spec(
                labels,
                type,
                grid[[type]]$major_breaks,
                panel_params[[type]],
                units
            )
        }),
        grid_types
    )
}

#' @noRd
CoordPsychro <- ggproto(
    "CoordPsychro",
    CoordCartesian,
    setup_params = function(self, data) {
        self$grids <- merge_psychro_grids(self$grids)

        # all parameters will inherit from ggpsychro() if not specified during
        # the construction process
        assert_flag(self$mollier, .var.name = "mollier")
        assert_number(self$altitude, .var.name = "altitude")
        assert_choice(self$units, c("SI", "IP"), .var.name = "units")

        assert_numeric(
            self$limits$tdb,
            any.missing = FALSE,
            all.missing = FALSE,
            len = 2L,
            unique = TRUE,
            sorted = TRUE,
            null.ok = TRUE,
            lower = get_tdb_limits(self$units)[1L],
            upper = get_tdb_limits(self$units)[2L],
            .var.name = "tdb_lim"
        )
        assert_numeric(
            self$limits$hum,
            any.missing = FALSE,
            all.missing = FALSE,
            len = 2,
            unique = TRUE,
            sorted = TRUE,
            null.ok = TRUE,
            lower = get_hum_limits(self$units)[1],
            upper = get_hum_limits(self$units)[2],
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
        self$pressure <- with_units(
            self$units,
            GetStandardAtmPressure(self$altitude)
        )

        self$limits[pos_tdb] <- list(self$limits$tdb)
        self$limits[pos_hum] <- list(self$limits$hum)

        limits <- self$limits[!(names(self$limits) %in% c("tdb", "hum"))]

        list(
            pos_tdb = pos_tdb,
            pos_hum = pos_hum,
            pressure = self$pressure,
            limits = limits,
            units = self$units
        )
    },

    setup_data = function(self, data, params = list()) {
        return(data)
        # If there is no data specified, this is the case for an empty
        # psychrometric chart. In this case, use the coordinate limits to build
        # a fake data for each layer
        lapply(data, function(d) {
            if (is.waive(d)) {
                len <- lengths(params$limits[c("x", "y")])
                if (all(!len)) {
                    return(d)
                }
                d <- as.data.frame(params$limits[c("x", "y")[len > 0]])
            }
            d
        })
    },

    setup_panel_params = function(
        self,
        scale_x,
        scale_y,
        scale_rh,
        scale_wb,
        scale_vp,
        scale_sv,
        scale_en,
        params = list()
    ) {
        default_limits <- default_psychro_limits(self$units)
        default_x <- if (self$mollier) {
            default_limits$hum
        } else {
            default_limits$tdb
        }
        default_y <- if (self$mollier) {
            default_limits$tdb
        } else {
            default_limits$hum
        }

        empty_range <- function(range) {
            is.null(range) || length(range) == 0L || anyNA(range)
        }
        choose_range_display <- function(scale, limit, default) {
            if (!is.null(limit)) {
                return(limit)
            }
            if (!empty_range(scale$range$range)) {
                return(scale$trans$inverse(scale$range$range))
            }
            default
        }

        # When training tdb and hum, the range should be shrunk based on
        # dewpoint and corresponding hum ratio. Missing limits are filled with
        # display defaults so empty psychrometric charts still render.
        lim_x <- choose_range_display(scale_x, self$limits$x, default_x)
        lim_y <- choose_range_display(scale_y, self$limits$y, default_y)

        if (self$mollier) {
            lim_tdb <- lim_y
            lim_hum <- narrow_hum(lim_x, self$units)
        } else {
            lim_tdb <- lim_x
            lim_hum <- narrow_hum(lim_y, self$units)
        }

        tdp <- with_units(
            self$units,
            psychrolib::GetTDewPointFromHumRatio(
                lim_tdb[1L],
                lim_hum[1L],
                self$pressure
            )
        )
        lim_tdb <- c(max(lim_tdb[1L], tdp), lim_tdb[2L])

        hum <- with_units(
            self$units,
            psychrolib::GetHumRatioFromTDewPoint(lim_tdb[2L], self$pressure)
        )
        lim_hum <- c(lim_hum[1L], min(lim_hum[2L], hum))

        if (self$mollier) {
            lim_x <- scale_x$transform(amplify_hum(lim_hum, self$units))
            lim_y <- scale_y$transform(lim_tdb)
        } else {
            lim_x <- scale_x$transform(lim_tdb)
            lim_y <- scale_y$transform(amplify_hum(lim_hum, self$units))
        }

        if (scale_x$is_empty()) {
            scale_x$train(lim_x)
        }
        if (scale_y$is_empty()) {
            scale_y$train(lim_y)
        }

        if (!is.null(lim_tdb) && !is.null(lim_hum)) {
            lim_rh <- cut_oob(
                with_units(
                    self$units,
                    psychrolib::GetRelHumFromHumRatio(
                        rev(lim_tdb),
                        lim_hum,
                        params$pressure
                    )
                ),
                c(0, 1)
            )
            lim_wb <- with_units(
                self$units,
                psychrolib::GetTWetBulbFromHumRatio(
                    lim_tdb,
                    lim_hum,
                    params$pressure
                )
            )
            lim_vp <- with_units(
                self$units,
                psychrolib::GetVapPresFromHumRatio(lim_hum, params$pressure)
            )
            lim_sv <- with_units(
                self$units,
                psychrolib::GetMoistAirVolume(lim_tdb, lim_hum, params$pressure)
            )
            lim_en <- with_units(
                self$units,
                psychrolib::GetMoistAirEnthalpy(lim_tdb, lim_hum)
            )

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
        if (cut) {
            # `cut` trims expanded panel ranges to the psychrolib-supported
            # dry-bulb domain; it is not a second user-limit application.
            rng <- self$scale_tdb(
                panel_params,
                self$range_tdb_physical(panel_params, cut = TRUE)
            )
        }
        rng
    },

    range_hum = function(self, panel_params, cut = FALSE) {
        rng <- panel_params[[paste(self$pos_hum(), "range", sep = ".")]]
        if (cut) {
            # `cut` trims expanded panel ranges to the psychrolib-supported
            # humidity domain in physical units, then returns the result in the
            # active scale space used by the panel.
            rng <- self$scale_hum(
                panel_params,
                self$range_hum_physical(panel_params, cut = TRUE)
            )
        }
        rng
    },

    # Range helpers invert the active position scales before psychrolib math, so
    # custom user transforms remain visual transforms rather than physical input.
    range_tdb_physical = function(self, panel_params, cut = FALSE) {
        scale <- panel_params[[self$pos_tdb()]]$scale
        rng <- scale$trans$inverse(self$range_tdb(panel_params))
        if (cut) {
            rng <- cut_oob(rng, get_tdb_limits(self$units))
        }
        rng
    },

    # Humidity-ratio position scales expose display units; psychrolib needs the
    # native kg/kg or lb/lb ratio.
    range_hum_physical = function(self, panel_params, cut = FALSE) {
        scale <- panel_params[[self$pos_hum()]]$scale
        hum <- narrow_hum(
            scale$trans$inverse(self$range_hum(panel_params)),
            self$units
        )
        if (cut) {
            hum <- cut_oob(
                hum,
                narrow_hum(get_hum_limits(self$units), self$units)
            )
        }
        hum
    },

    # Dry-bulb values are transformed only at the final drawing boundary.
    scale_tdb = function(self, panel_params, tdb) {
        psychro_scale_transform(panel_params[[self$pos_tdb()]]$scale, tdb)
    },

    # Humidity values leave psychrolib in native ratio units and re-enter the
    # scale as user-facing display units.
    scale_hum = function(self, panel_params, hum) {
        scale <- panel_params[[self$pos_hum()]]$scale
        psychro_scale_transform(scale, amplify_hum(hum, self$units))
    },

    trans_grid_vert = function(
        self,
        tdb,
        type,
        breaks,
        range_tdb = NULL,
        range_hum = NULL,
        panel_params = NULL,
        cut = FALSE
    ) {
        n <- length(breaks)
        if (n == 0L) {
            return(NULL)
        }

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
            if (n == 0L) {
                return(NULL)
            }
            tdb <- unlist(lapply(lst[not_empty], base::range), FALSE)
            breaks <- rep(breaks[not_empty], each = 2L)
            group <- rep(seq_len(n), each = 2L)
            len <- 2L
        }

        no_hum_limit <- function(expr) {
            with_units(self$units, with_no_hum_limit(expr))
        }

        hum <- switch(
            type,
            relhum = no_hum_limit(psychrolib::GetHumRatioFromRelHum(
                tdb,
                breaks,
                self$pressure
            )),
            wetbulb = no_hum_limit(psychrolib::GetHumRatioFromTWetBulb(
                tdb,
                breaks,
                self$pressure
            )),
            vappres = no_hum_limit(psychrolib::GetHumRatioFromVapPres(
                breaks,
                self$pressure
            )),
            specvol = no_hum_limit(GetHumRatioFromMoistAirVolumeAndTDryBulb(
                breaks,
                tdb,
                self$pressure
            )),
            enthalpy = no_hum_limit(GetHumRatioFromEnthalpyAndTDryBulb(
                breaks,
                tdb
            )),
            stop("Invalid grid type found")
        )

        if (cut) {
            hum_range_physical <- if (is.null(panel_params)) {
                range_hum
            } else {
                self$range_hum_physical(panel_params)
            }
            hum <- cut_oob(hum, hum_range_physical)
        }

        if (!is.null(panel_params)) {
            # Psychrolib results are physical values; rescale only after moving
            # them back through the active position scales.
            tdb <- self$scale_tdb(panel_params, tdb)
            hum <- self$scale_hum(panel_params, hum)
        }

        tdb <- rescale01(tdb, range_tdb)
        hum <- rescale01(hum, range_hum)

        list(
            tdb = tdb,
            hum = hum,
            len = len,
            n = n,
            breaks = line_breaks,
            value = breaks,
            group = group
        )
    },

    render_bg = function(self, panel_params, theme) {
        # only process if both tdb and hum ranges have been trained
        if (
            is.null(panel_params$x$scale$range$range) ||
                is.null(panel_params$y$scale$range$range)
        ) {
            return(ggplot2::ggproto_parent(CoordCartesian, self)$render_bg(
                panel_params,
                theme
            ))
        }

        range_tdb <- self$range_tdb(panel_params)
        range_hum <- self$range_hum(panel_params)

        # get initial tdb for grid line
        # NOTE: here we should use the continuous_range instead of the limits
        scale <- panel_params[[self$pos_tdb()]]$scale
        limits <- scale$trans$inverse(
            panel_params[[self$pos_tdb()]]$continuous_range
        )
        tdb <- scale$trans$breaks(limits, 100L)

        sat <- coord_psy__saturation_npc(self, panel_params)
        if (is.null(sat)) {
            return(ggplot2::ggproto_parent(CoordCartesian, self)$render_bg(
                panel_params,
                theme
            ))
        }

        grid_labels <- self$grid_labels %||% list()
        grid <- coord_psy__grid_lines(
            self,
            panel_params,
            tdb,
            range_tdb,
            range_hum
        )
        labels <- coord_psy__grid_labels(
            grid,
            grid_labels,
            panel_params,
            self$units
        )

        guide_grid_psychro(
            theme,
            list(
                tdb = list(
                    minor = panel_params[[self$pos_tdb()]]$break_positions_minor(),
                    major = panel_params[[self$pos_tdb()]]$break_positions()
                ),
                hum = list(
                    minor = panel_params[[self$pos_hum()]]$break_positions_minor(),
                    major = panel_params[[self$pos_hum()]]$break_positions()
                )
            ),
            sat,
            grid,
            labels,
            self$mollier
        )
    },

    render_fg = function(self, panel_params, theme) {
        sat <- if (isFALSE(self$draw_saturation_fg)) {
            NULL
        } else {
            coord_psy__saturation_npc(self, panel_params)
        }
        border <- ggplot2::ggproto_parent(CoordCartesian, self)$render_fg(
            panel_params,
            theme
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
                self$protractor,
                theme,
                self$mollier,
                range_tdb,
                range_hum,
                self$units
            ),
            if (!is.null(sat)) {
                ggplot2::element_render(
                    theme,
                    "psychro.panel.grid.saturation",
                    x = line_x,
                    y = line_y
                )
            },
            coord_fg__extra_foreground(self, panel_params, theme),
            border
        )
    }
)

GeomPsychroSaturation <- ggplot2::ggproto(
    "GeomPsychroSaturation",
    ggplot2::Geom,
    required_aes = character(),
    default_aes = ggplot2::aes(),
    draw_key = ggplot2::draw_key_blank,
    extra_params = c("na.rm", "psychro.theme"),

    draw_panel = function(
        data,
        panel_params,
        coord,
        psychro.theme = NULL,
        ...
    ) {
        sat <- coord_psy__saturation_npc(coord, panel_params)
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
            x = line_x,
            y = line_y
        )
    }
)

# Return the valid psychrometric panel polygon in normalized panel coordinates.
coord_psy__panel_polygon_npc <- function(coord, panel_params) {
    sat <- coord_psy__saturation_npc(coord, panel_params)
    if (is.null(sat)) {
        return(NULL)
    }
    psychro_panel_polygon(sat, coord$mollier)
}

# Build an invisible polygon grob used as the clipping boundary.
coord_psy__panel_grob <- function(coord, panel_params) {
    panel <- coord_psy__panel_polygon_npc(coord, panel_params)
    if (is.null(panel)) {
        return(NULL)
    }
    grid::polygonGrob(
        panel$x,
        panel$y,
        gp = grid::gpar(col = NA, fill = NA),
        name = "psychro-panel-clip"
    )
}

# Return the valid panel polygon in active scale space for data clipping.
coord_psy__panel_polygon_scaled <- function(coord, panel_params) {
    sat <- coord_psy__saturation_scaled(coord, panel_params)
    if (is.null(sat)) {
        return(NULL)
    }
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)

    if (coord$mollier) {
        return(list(
            x = c(
                range_hum[1L],
                range_hum[1L],
                range_hum[2L],
                rev(sat$hum),
                sat$hum[1L]
            ),
            y = c(
                range_tdb[1L],
                range_tdb[2L],
                range_tdb[2L],
                rev(sat$tdb),
                range_tdb[1L]
            )
        ))
    }

    list(
        x = c(
            range_tdb[1L],
            range_tdb[1L],
            sat$tdb,
            range_tdb[2L],
            range_tdb[2L]
        ),
        y = c(range_hum[1L], sat$hum[1L], sat$hum, range_hum[2L], range_hum[1L])
    )
}

# Compute the saturation curve and return it in active scale space.
coord_psy__saturation_scaled <- function(coord, panel_params) {
    range_hum <- coord$range_hum_physical(panel_params)

    # The saturation curve samples dry-bulb values from the trained scale
    # interval; the caller still uses range_tdb() to close the panel polygon.
    scale <- panel_params[[coord$pos_tdb()]]$scale
    limits <- scale$trans$inverse(
        panel_params[[coord$pos_tdb()]]$continuous_range
    )
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
            with_units(
                coord$units,
                GetTDewPointFromHumRatioOnly(
                    range_hum[1L],
                    coord$pressure
                )
            )
        },
        sat_tdb,
        if (range_hum[2L] > 0.0) {
            sat_tdb_max <- with_units(
                coord$units,
                GetTDewPointFromHumRatioOnly(
                    range_hum[2L],
                    coord$pressure
                )
            )
            sat_app_end <- sat_tdb_max > max(sat_tdb)
            sat_tdb_max[sat_app_end]
        }
    )
    sat_hum <- c(
        if (range_hum[1L] > 0.0) range_hum[1L],
        sat_hum,
        if (range_hum[2L] > 0.0) range_hum[2L][sat_app_end]
    )

    list(
        tdb = coord$scale_tdb(panel_params, sat_tdb),
        hum = coord$scale_hum(panel_params, sat_hum)
    )
}

# Convert the scaled saturation curve into normalized panel coordinates.
coord_psy__saturation_npc <- function(coord, panel_params) {
    range_tdb <- coord$range_tdb(panel_params)
    range_hum <- coord$range_hum(panel_params)
    sat <- coord_psy__saturation_scaled(coord, panel_params)
    if (is.null(sat)) {
        return(NULL)
    }

    list(
        tdb = rescale01(sat$tdb, range_tdb),
        hum = rescale01(sat$hum, range_hum),
        len = length(sat$tdb),
        n = 1L
    )
}
