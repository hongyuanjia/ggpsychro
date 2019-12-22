#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 zeroGrob draw_key_text
#' @export
# GeomGridTextRelHum {{{
GeomGridTextRelHum <- ggproto("GeomGridTextRelHum", Geom,
    required_aes = c("step", "pres", "units"),

    default_aes = aes(
        colour = "#633F87", size = 3.88, angle = 0, hjust = 0,
        vjust = -0.3, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),

    draw_panel = function(self, data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                          check_overlap = FALSE, label = NULL, label_loc = NULL,
                          label_fun = NULL, label_parse = FALSE, label_style = NULL) {
        if (is.null(label_loc)) return(zeroGrob())

        label <- validate_text_label(label, c(0.0, 100.0), unique(data$step))
        if (is.null(label_fun)) {
            label_fun <- function (label) {
                label[1L] <- paste(label[1L], "% RH")
                label[-1L] <- paste(label[-1L], "%")
                label
            }
            label_parse <- FALSE
        }

        compute_gridtext_panel_data("RelHum", self, data, panel_params, coord, na.rm, check_overlap,
            label, label_loc, label_fun, label_parse, label_style
        )
    },

    draw_key = draw_key_text
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# GeomGridTextWetBulb {{{
GeomGridTextWetBulb <- ggproto("GeomGridTextWetBulb", Geom,
    required_aes = c("step", "pres", "units"),

    default_aes = aes(
        colour = "#015756", size = 3.88, angle = 0, hjust = 0,
        vjust = -0.3, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),

    draw_panel = function(self, data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                          check_overlap = FALSE, label = NULL, label_loc = NULL,
                          label_fun = NULL, label_parse = FALSE, label_style = NULL) {
        if (is.null(label_loc)) return(zeroGrob())

        units <- get_units(data)
        pres <- get_pres(data)
        ranges <- coord$limits

        # calculate wetbulb at the min drybulb at the origin
        vmin <- with_units(units, GetTWetBulbFromHumRatio(ranges$x[[1L]], ranges$y[[1L]], pres))

        # get the break step of drybulb
        xstep <- diff(panel_params$x.minor_source)[[1L]]

        # calculate min wetbulb with whole x steps away
        vmin <- ranges$x[[1L]] - ceiling((ranges$x[[1L]] - vmin) / xstep) * xstep

        vlim <- c(vmin, ranges$x[[2L]])

        label <- validate_text_label(label, vlim, unique(data$step))
        # above make sure wet-bulb seq is the same as grid, but for labelling
        # wet-bulb should also smaller than min dry-bulb
        label <- label[label > ranges$x[[1L]]]

        if (is.null(label_fun)) {
            label_fun <- function (label) {
                symbol <- switch(units, SI = expression(degree*C), IP = expression(degree*F))
                label[1L] <- paste0("Wet-bulb ~", label[1L], "*", symbol)
                label[-1L] <- paste0(label[-1L], "*", symbol)
                label
            }
            label_parse <- TRUE
        }

        compute_gridtext_panel_data("WetBulb", self, data, panel_params, coord, na.rm, check_overlap,
            label, label_loc, label_fun, label_parse, label_style
        )
    },

    draw_key = draw_key_text
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# GeomGridTextVapPres {{{
GeomGridTextVapPres <- ggproto("GeomGridTextVapPres", Geom,
    required_aes = c("step", "pres", "units"),

    default_aes = aes(
        colour = "#941919", size = 3.88, angle = 0, hjust = 1.1,
        vjust = -0.3, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),

    draw_panel = function(self, data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                          check_overlap = FALSE, label = NULL, label_loc = NULL,
                          label_fun = NULL, label_parse = FALSE, label_style = NULL) {
        if (is.null(label_loc)) return(zeroGrob())
        units <- get_units(data)
        pres <- get_pres(data)

        # calculate vapor pressure range at the y axis range
        yrange <- narrow_hum(coord$limits$y, units)
        vlim <- round(GetVapPresFromHumRatio(yrange, pres))

        label <- validate_text_label(label, vlim, unique(data$step))

        if (is.null(label_fun)) {
            label_fun <- function (label) {
                if (units == "SI") {
                    label[1L] <- paste("Vapor Pressure", label[1L], "Pa")
                    label[-1L] <- paste(label[-1L], "Pa")
                } else {
                    label[1L] <- paste(label[1L], "Psi")
                    label[-1L] <- paste(label[-1L], "Psi")
                }
                label
            }
            label_parse <- FALSE
        }

        compute_gridtext_panel_data("VapPres", self, data, panel_params, coord, na.rm, check_overlap,
            label, label_loc, label_fun, label_parse, label_style
        )
    },

    draw_key = draw_key_text
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# GeomGridTextSpecVol {{{
GeomGridTextSpecVol <- ggproto("GeomGridTextSpecVol", Geom,
    required_aes = c("step", "pres", "units"),

    default_aes = aes(
        colour = "#015756", size = 3.88, angle = 0, hjust = 1.1,
        vjust = -0.3, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),

    draw_panel = function(self, data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                          check_overlap = FALSE, label = NULL, label_loc = NULL,
                          label_fun = NULL, label_parse = FALSE, label_style = NULL) {
        if (is.null(label_loc)) return(zeroGrob())

        units <- get_units(data)
        pres <- get_pres(data)
        ranges <- coord$limits

        # calculate spec vol range at the x and y axis range
        vlim <- with_units(units, GetMoistAirVolume(ranges$x, narrow_hum(ranges$y, units), pres))

        label <- validate_text_label(label, vlim, unique(data$step))

        if (is.null(label_fun)) {
            label_fun <- function (label) {
                label <- round(label, digits = 2)
                if (units == "SI") {
                    label[1L] <- paste0("'Specific vol'*", label[1L], "* m^3 / kg")
                    label[-1L] <- paste0(label[-1L], " * m^3 / kg")
                } else {
                    label[1L] <- paste0("'Specific vol'*", label[1L], "* ft^3 / lb")
                    label[-1L] <- paste0(label[-1L], " * ft^3 / lb")
                }
                label
            }

            label_parse <- TRUE
        }

        compute_gridtext_panel_data("SpecVol", self, data, panel_params, coord, na.rm, check_overlap,
            label, label_loc, label_fun, label_parse, label_style
        )
    },

    draw_key = draw_key_text
)
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
# GeomGridTextEnthalpy {{{
GeomGridTextEnthalpy <- ggproto("GeomGridTextEnthalpy", Geom,
    required_aes = c("step", "pres", "units"),

    default_aes = aes(
        colour = "#633F87", size = 3.88, angle = 0, hjust = 1.1,
        vjust = -0.3, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),

    draw_panel = function(self, data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                          check_overlap = FALSE, label = NULL, label_loc = NULL,
                          label_fun = NULL, label_parse = FALSE, label_style = NULL) {
        if (is.null(label_loc)) return(zeroGrob())

        units <- get_units(data)
        ranges <- coord$limits

        # calculate spec vol range at the x and y axis range
        vlim <- narrow_enth(with_units(units, GetMoistAirEnthalpy(ranges$x, narrow_hum(ranges$y, units))), units)
        vlim <- round(vlim)

        label <- validate_text_label(label, vlim, unique(data$step))

        if (is.null(label_fun)) {
            label_fun <- function (label) {
                label <- round(label, digits = 2)
                if (units == "SI") {
                    label[1L] <- paste0("Enthalpy ", label[1L], " kJ / kg")
                    label[-1L] <- paste0(label[-1L], " KJ / kg")
                } else {
                    label[1L] <- paste0("Enthalpy ", label[1L], " Btu / lb")
                    label[-1L] <- paste0(label[-1L], " Btu / lb")
                }
                label
            }

            label_parse <- FALSE
        }
        compute_gridtext_panel_data("Enthalpy", self, data, panel_params, coord, na.rm, check_overlap,
            label, label_loc, label_fun, label_parse, label_style
        )
    },

    draw_key = draw_key_text
)
# }}}

#' @importFrom checkmate assert_number
#' @importFrom grid gpar textGrob
#' @importFrom psychrolib GetTDryBulbFromMoistAirVolumeAndHumRatio GetTDryBulbFromEnthalpyAndHumRatio
# compute_gridtext_panel_data {{{
compute_gridtext_panel_data <- function (type, self, data, panel_params, coord, na.rm = FALSE, check_overlap = FALSE,
                                         label = NULL, label_loc = NULL, label_format = NULL,
                                         label_parse = FALSE, label_style = NULL) {
    assert_number(label_loc, lower = 0.0, upper = 1.0)

    units <- get_units(data)
    pres <- get_pres(data)
    ranges <- coord$limits

    # Have to update label style here
    # Cannot figure out why `$setup_params()` did not be called
    if (!is.null(label_style)) {
        if (!is.list(label_style)) {
            stop(sprintf("'geom_grid_%s': 'label_style' should be a list, not a <%s> object.",
                tolower(type), class(label_style)[1]), call. = FALSE)
        }
        aes_nm <- names(label_style)
        if (is.null(aes_nm)) {
            stop(sprintf("'geom_grid_%s': 'label_style' should be named.",
                tolower(type)), call. = FALSE)
        }

        # handle color --> colour
        aes_nm[aes_nm == "color"] <- "colour"
        names(label_style) <- aes_nm

        # get all aes
        aes_nm_all <- names(self$default_aes)

        if (length(invlid <- aes_nm[!aes_nm %in% aes_nm_all])) {
            stop(sprintf("'geom_grid_%s': invalid style specification in 'label_style' [%s].",
                tolower(type), paste0("'", invlid, "'", collapse = ",")), call = FALSE)
        }

        for (nm in aes_nm) data[[nm]] <- label_style[[nm]]
    }

    loc <- cal_text_loc(type, ranges, label, label_loc, pres, units)

    # clean data
    data$pres <- NULL
    data$units <- NULL
    data$step <- NULL
    data$n <- NULL

    # create new data
    data <- do.call(rbind, replicate(length(label), data, simplify = FALSE))
    data$x <- loc$x
    data$y <- loc$y
    data$angle <- loc$angle

    # format label
    if (!is.function(label_format)) {
        stop(sprintf("'geom_grid_%s': 'label_format' should be a function not a <%s> object.",
            tolower(type), class(label_format)[1]), call. = FALSE)
    }
    lab_fmt <- label_format(label)
    if (length(lab_fmt) != length(label)) {
        stop(sprintf("'geom_grid_%s': 'label_format' should return a character vector of the same length (%d) as 'label' but not (%d).",
            tolower(type), length(label), length(lab_fmt)), call. = FALSE)
    }
    label <- as.character(lab_fmt)

    # parse label necessary
    if (label_parse) {
        lab_par <- vector("expression", length(label))
        for (i in seq_along(label)) {
            expr <- parse(text = label[[i]])
            lab_par[[i]] <- if (length(expr) == 0L) NA else expr[[1L]]
        }
        label <- lab_par
    }

    data$label <- label

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
        data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
        data$hjust <- compute_just(data$hjust, data$x)
    }

    rot.type <- if (type == "VapPres") "rot" else "along"

    textAlongGrob(
        data$label, data$x, data$y, default.units = "native",
        hjust = data$hjust, vjust = data$vjust, rot = data$angle,
        gp = grid::gpar(
            col = alpha(data$colour, data$alpha),
            fontsize = data$size * .pt,
            fontfamily = data$family,
            fontface = data$fontface,
            lineheight = data$lineheight
            ),
        check.overlap = check_overlap, rot.type = rot.type,
        x0 = loc$x0, x1 = loc$x1, y0 = loc$y0, y1 = loc$y1
    )
}
# }}}
# cal_text_loc {{{
cal_text_loc <- function (type, ranges, label, label_loc, pres, units, offset = 0.05) {
    if (type == "RelHum") {
        label <- label / 100.0
        # calculate dry-bulb value based on label loc
        x <- ranges$x[[1L]] + diff(ranges$x) * label_loc
        x <- rep(x, length(label))

        x0 <- x * (1 - offset)
        x1 <- x * (1 + offset)

        y <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x, label, pres)), units)
        y0 <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x0, label, pres)), units)
        y1 <- amplify_hum(with_units(units, GetHumRatioFromRelHum(x1, label, pres)), units)

        angle <- line_angle(x0, y0, x1, y1)
    } else if (type == "WetBulb") {
        # start from the saturation line
        x0 <- label
        x1 <- x0 * (1 + 2 * offset)

        y0 <- amplify_hum(with_units(units, GetHumRatioFromTWetBulb(x0, label, pres)), units)
        y1 <- amplify_hum(with_units(units, GetHumRatioFromTWetBulb(x1, label, pres)), units)

        # calculate slop
        slp <- slope(x0, y0, x1, y1)
        # calculate intercept
        intercept <- y0 - slp * x0

        # check if start point y0 is out of hum-ratio axis, i.e. out of panel
        out <- y0 > ranges$y[[2L]]
        if (any(out)) {
            y0[out] <- ranges$y[[1L]]
            x0[out] <- (y0[out] - intercept[out]) / slp[out]
        }

        # get the intercept point on the dry-bulb axis
        y1 <- rep(ranges$y[[1L]], length(y1))
        x1 <- (ranges$y[[1L]] - intercept) / slp
        # if the x is outside of dry-bulb axis, set the point on the hum-ratio axis
        out <- x1 > ranges$x[[2L]]
        if (any(out)) {
            x1[out] <- ranges$x[[2L]]
            y1[out] <- x1[out] * slp[out] + intercept[out]
        }

        angle <- line_angle(x0, y0, x1, y1)

        # calculate the length
        dis <- sqrt((y1 - y0)^ 2 + (x1 - x0)^2) * label_loc

        # calculate the label location
        x <- x0 + dis * cos(angle * pi / 180)
        y <- y0 + dis * sin(angle * pi / 180)
    } else if (type == "VapPres") {
        x0 <- rep(ranges$x[[2L]], length(label))
        x1 <- x0
        y0 <- amplify_hum(with_units(units, GetHumRatioFromVapPres(label, pres)), units)
        y1 <- y0

        x <- ranges$x[[1L]] + diff(ranges$x) * label_loc
        x <- rep(x, length(label))
        y <- y0

        angle <- rep(0.0, length(label))
    } else if (type == "SpecVol") {
        y1 <- rep(ranges$y[[1L]], length(label))
        x1 <- with_units(units, GetTDryBulbFromMoistAirVolumeAndHumRatio(label, y1, pres))
        if (any(out <- x1 > ranges$x[[2L]])) {
            x1[out] <- ranges$x[[2L]]
            y1[out] <- amplify_hum(with_units(units, GetHumRatioFromAirVolume(x1[out], label[out], pres)), units)
        }

        x0 <- x1 * (1 - offset)
        y0 <- amplify_hum(with_units(units, GetHumRatioFromAirVolume(x0, label, pres)), units)

        angle <- line_angle(x0, y0, x1, y1)

        # calculate slop
        slp <- slope(x0, y0, x1, y1)
        # calculate intercept
        intercept <- y0 - slp * x0

        # # calculate the length
        # dis <- sqrt((y1 - y0)^ 2 + (x1 - x0)^2) * label_loc

        # # calculate the label location
        # x <- x0 + dis * cos(angle * pi / 180)
        # y <- y0 + dis * sin(angle * pi / 180)

        x <- x1
        y <- y1
    } else if (type == "Enthalpy") {
        y1 <- rep(ranges$y[[1L]], length(label))
        x1 <- with_units(units, GetTDryBulbFromEnthalpyAndHumRatio(amplify_enth(label, units), narrow_hum(y1, units)))
        if (any(out <- x1 > ranges$x[[2L]])) {
            x1[out] <- ranges$x[[2L]]
            y1[out] <- amplify_hum(with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(amplify_hum(label[out], units), x1[out])), units)
        }

        x0 <- x1 * (1 - offset)
        y0 <- amplify_hum(with_units(units, GetHumRatioFromEnthalpyAndTDryBulb(amplify_hum(label, units), x0)), units)

        angle <- line_angle(x0, y0, x1, y1)

        x <- x1
        y <- y1
    }

    list(x = x, y = y, x0 = x0, y0 = y0, x1 = x1, y1 = y1, angle = angle)
}
# }}}
# validate_text_label {{{
validate_text_label <- function (label, lims, step) {
    if (is.null(label)) {
        label <- seq(lims[[1L]], lims[[2L]], by = step)
        label[label > lims[[1L]] & label < lims[[2L]]]
    } else {
        assert_numeric(label, lower = lims[[1L]], upper = lims[[2L]], any.missing = FALSE)
        sort(label)
    }
}
# }}}
# compute_just {{{
# copied from tidyverse/ggplot2/R/geom-text.r
compute_just <- function(just, x) {
    inward <- just == "inward"
    just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
    outward <- just == "outward"
    just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

    unname(c(left = 0, center = 0.5, right = 1,
            bottom = 0, middle = 0.5, top = 1)[just])
}
# }}}

# adopted from thomasp85/ggraph/R/textAlong.R
#' @importFrom grid unit textGrob grob is.unit
# textAlongGrob {{{
textAlongGrob <- function(label, x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                          just = 'centre', hjust = NULL, vjust = NULL, rot = 0,
                          check.overlap = FALSE, rot.type = 'rot', x0 = 0,
                          y0 = 0, x1 = 0, y1 = 0, force.rot = TRUE, dodge = NULL,
                          push = NULL, default.units = 'npc', name = NULL, gp = gpar(),
                          vp = NULL) {
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (rot.type == 'rot') {
        textGrob(
            label = label, x = x, y = y, just = just, hjust = hjust,
            vjust = vjust, rot = rot, check.overlap = check.overlap,
            default.units = default.units, name = name, gp = gp, vp = vp
        )
    } else {
        if (!rot.type %in% c('along', 'across')) {
            stop('rot.type must be either `rot`, `along`, or `across`', call. = FALSE)
        }
        if (!is.unit(x0)) {
            x0 <- unit(x0, default.units)
        }
        if (!is.unit(y0)) {
            y0 <- unit(y0, default.units)
        }
        if (!is.unit(x1)) {
            x1 <- unit(x1, default.units)
        }
        if (!is.unit(y1)) {
            y1 <- unit(y1, default.units)
        }
        grob(
            label = label, x = x, y = y, just = just, hjust = hjust,
            vjust = vjust, rot.type = rot.type, x0 = x0, y0 = y0, x1 = x1,
            y1 = y1, force.rot = force.rot, dodge = dodge, push = push,
            check.overlap = check.overlap, name = name, gp = gp, vp = vp,
            cl = 'textalong'
        )
    }
}
# }}}

#' @importFrom grid makeContent
#' @importFrom grid convertX convertY convertHeight grob
#' @export
# makeContent.textalong {{{
makeContent.textalong <- function(x) {
    x0 <- convertX(x$x0, 'mm', TRUE)
    y0 <- convertY(x$y0, 'mm', TRUE)
    x1 <- convertX(x$x1, 'mm', TRUE)
    y1 <- convertY(x$y1, 'mm', TRUE)
    xpos <- convertX(x$x, 'mm', TRUE)
    ypos <- convertY(x$y, 'mm', TRUE)
    angle <- line_angle(x0, y0, x1, y1)
    if (x$rot.type == 'across') {
        angle <- angle - 90
    }
    if (!is.null(x$dodge)) {
        dodge <- convertHeight(x$dodge, 'mm', TRUE)
        dodge_angle <- (angle + 90) / 360 * 2 * pi
        dodge_x <- cos(dodge_angle) * dodge
        dodge_y <- sin(dodge_angle) * dodge
        xpos <- xpos + dodge_x
        ypos <- ypos + dodge_y
    }
    if (!is.null(x$push)) {
        push <- convertHeight(x$push, 'mm', TRUE)
        push_angle <- angle / 360 * 2 * pi
        push_x <- cos(push_angle) * push
        push_y <- sin(push_angle) * push
        xpos <- xpos + push_x
        ypos <- ypos + push_y
    }
    if (x$force.rot) {
        fix <- angle > 90 & angle < 270
        angle[fix] <- angle[fix] + 180
    }
    grob(
        label = x$label, x = unit(xpos, 'mm'), y = unit(ypos, 'mm'),
        just = x$just, hjust = x$hjust, vjust = x$vjust, rot = angle,
        check.overlap = x$check.overlap, name = x$name, gp = x$gp, vp = x$vp,
        cl = 'text'
    )
}
# }}}
