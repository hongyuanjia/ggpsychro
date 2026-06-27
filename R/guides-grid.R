#' Draw drybulb and hum ratio grid lines
#'
#' @param theme A ggplot [theme][ggplot2::theme]
#' @param tdb.minor,tdb.major A numeric vector of dry-bulb temperature
#'        minor/major breaks in **native** units.
#' @param hum.minor,hum.major A numeric vector of humidity ratio minor/major
#'        breaks in **native** units.
#' @param saturation,rh.minor,rh.major,twb.minor,twb.major,vappres.minor,
#'        vappres.major,specvol.minor,specvol.major,enthalpy.minor, enthalpy.major
#'        A list of 4 elements, i.e. `tdb`, `hum`, `len`, and `n`.
#' @param mollier A single logical value indicating whether a Mollier plot is
#'        desired
#' @noRd
guide_grid_psychro <- function(theme, tdb.minor, tdb.major, hum.minor, hum.major,
                               saturation,
                               rh.minor, rh.major, twb.minor, twb.major,
                               vappres.minor, vappres.major, specvol.minor, specvol.major,
                               enthalpy.minor, enthalpy.major, grid.labels,
                               mollier) {
    # create psychrometric chart panel
    if (mollier) {
        panel_x <- c(0.0, 0.0, 1.0, rev(saturation$hum), saturation$hum[1L])
        panel_y <- c(0.0, 1.0, 1.0, rev(saturation$tdb), 0.0)
        psychro_panel <- ggplot2::element_render(
            theme, "psychro.panel.background",
            x = panel_x,
            y = panel_y
        )

        psychro_mask <- ggplot2::element_render(
            theme, "psychro.panel.mask",
            x = c(saturation$hum, 1.0, saturation$hum[1L]),
            y = c(saturation$tdb, 0.0, 0.0)
        )

        nm_tdb <- "y"
        nm_hum <- "x"
        nm_x <- "hum"
        nm_y <- "tdb"
    } else {
        panel_x <- c(0.0, 0.0,                saturation$tdb, 1.0, 1.0)
        panel_y <- c(0.0, saturation$hum[1L], saturation$hum, 1.0, 0.0)
        psychro_panel <- ggplot2::element_render(
            theme, "psychro.panel.background",
            x = panel_x,
            y = panel_y
        )

        psychro_mask <- ggplot2::element_render(
            theme, "psychro.panel.mask",
            x = c(0.0, rev(saturation$tdb), 0.0),
            y = c(1.0, rev(saturation$hum), saturation$hum[1L])
        )

        nm_tdb <- "x"
        nm_hum <- "y"
        nm_x <- "tdb"
        nm_y <- "hum"
    }

    grid_elem <- function(x, type, var) {
        vx <- rep(x, each = 2L)
        vy <- rep(0:1, length(x))
        v <- if (var == "x") list(x = vx, y = vy) else list(x = vy, y = vx)

        ggplot2::element_render(
            theme, paste("panel.grid", type, var, sep = "."),
            x = v$x, y = v$y, id.lengths = rep(2, length(x))
        )
    }

    psy_grid_elem <- function(x, type, var) {
        ggplot2::element_render(
            theme, paste("psychro.panel.grid", type, var, sep = "."),
            x = x[[c("tdb", "hum")[c(!mollier, mollier)]]],
            y = x[[c("tdb", "hum")[c(mollier, !mollier)]]],
            id.lengths = rep(x$len, x$n)
        )
    }

    psy_grid_label <- function(x, var) {
        psychro_grid_label_grob(
            x, grid.labels[[var]], var, theme, mollier,
            panel_x, panel_y
        )
    }

    grill <- grid::grobTree(
        ggplot2::element_render(theme, "panel.background"),

        psychro_panel,

        if (length(hum.minor)) {
            clip_grob(psychro_panel, grid_elem(hum.minor, "minor", nm_hum))
        },

        if (length(tdb.minor)) {
            clip_grob(psychro_panel, grid_elem(tdb.minor, "minor", nm_tdb))
        },

        if (length(hum.major)) {
            clip_grob(psychro_panel, grid_elem(hum.major, "major", nm_hum))
        },

        if (length(tdb.major)) {
            clip_grob(psychro_panel, grid_elem(tdb.major, "major", nm_tdb))
        },

        if (length(rh.minor)) {
            psy_grid_elem(rh.minor, "minor", "relhum")
        },

        if (length(rh.major)) {
            psy_grid_elem(rh.major, "major", "relhum")
        },

        if (length(twb.minor)) {
            psy_grid_elem(twb.minor, "minor", "wetbulb")
        },

        if (length(twb.major)) {
            psy_grid_elem(twb.major, "major", "wetbulb")
        },

        if (length(vappres.minor)) {
            clip_grob(psychro_panel, psy_grid_elem(vappres.minor, "minor", "vappres"))
        },

        if (length(vappres.major)) {
            clip_grob(psychro_panel, psy_grid_elem(vappres.major, "major", "vappres"))
        },

        if (length(specvol.minor)) {
            clip_grob(psychro_panel, psy_grid_elem(specvol.minor, "minor", "specvol"))
        },

        if (length(specvol.major)) {
            clip_grob(psychro_panel, psy_grid_elem(specvol.major, "major", "specvol"))
        },

        if (length(enthalpy.minor)) {
            clip_grob(psychro_panel, psy_grid_elem(enthalpy.minor, "minor", "enthalpy"))
        },

        if (length(enthalpy.major)) {
            clip_grob(psychro_panel, psy_grid_elem(enthalpy.major, "major", "enthalpy"))
        },

        if (length(rh.major)) {
            psy_grid_label(rh.major, "relhum")
        },

        if (length(twb.major)) {
            psy_grid_label(twb.major, "wetbulb")
        },

        if (length(vappres.major)) {
            psy_grid_label(vappres.major, "vappres")
        },

        if (length(specvol.major)) {
            psy_grid_label(specvol.major, "specvol")
        },

        if (length(enthalpy.major)) {
            psy_grid_label(enthalpy.major, "enthalpy")
        },

        psychro_mask,

        if (length(saturation)) {
            ggplot2::element_render(
                theme, "psychro.panel.grid.saturation",
                x = saturation[[nm_x]], y = saturation[[nm_y]]
            )
        }
    )

    grill$name <- grid::grobName(grill, "grill")
    grill
}

#' @importFrom gridGeometry polyclipGrob
clip_grob <- function(panel, grob, op = "intersection") {
    gridGeometry::polyclipGrob(grob, panel, op, name = grob$name, gp = grob$gp)
}

psychro_grid_label_grob <- function(grid, label, type, theme, mollier,
                                    panel_x, panel_y) {
    if (is.null(grid) || is.null(label) || !isTRUE(label$show)) {
        return(NULL)
    }

    data <- psychro_grid_label_data(grid, label$labels, mollier, panel_x, panel_y)
    if (!nrow(data$path)) return(NULL)

    style <- psychro_grid_label_style_defaults(type, theme, label$style)
    labels <- psychro_grid_normalise_labels(data$labels, label$label_parse)
    if (!length(labels)) return(NULL)

    colour <- psychro_grid_alpha(style$colour, style$alpha)
    geomtextpath::textpathGrob(
        label = labels,
        x = data$path$x,
        y = data$path$y,
        id = data$path$id,
        hjust = rep(label$label_loc, length(labels)),
        vjust = rep(style$vjust, length(labels)),
        halign = style$halign,
        gap = style$gap,
        upright = style$upright,
        straight = style$straight,
        text_smoothing = style$text_smoothing,
        padding = style$padding,
        remove_long = style$remove_long,
        gp_text = grid::gpar(
            col = colour,
            fontsize = style$size * ggplot2::.pt,
            fontfamily = style$family,
            fontface = style$fontface,
            lineheight = style$lineheight
        ),
        gp_path = grid::gpar(col = NA, lwd = 0),
        default.units = "npc",
        name = paste0("psychro-grid-label-", type)
    )
}

psychro_grid_label_data <- function(grid, labels, mollier, panel_x, panel_y) {
    x <- grid[[c("tdb", "hum")[c(!mollier, mollier)]]]
    y <- grid[[c("tdb", "hum")[c(mollier, !mollier)]]]
    id <- grid$group

    label_missing <- psychro_grid_label_missing(labels)
    keep_group <- seq_along(labels)[!label_missing]
    inside <- psychro_inside_polygon(x, y, panel_x, panel_y) & id %in% keep_group

    pieces <- lapply(keep_group, function(group) {
        idx <- which(inside & id == group)
        if (length(idx) < 2L) return(NULL)
        new_data_frame(list(
            x = x[idx],
            y = y[idx],
            id = rep(group, length(idx))
        ))
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (!length(pieces)) {
        return(list(path = new_data_frame(list()), labels = labels[0]))
    }

    path <- do.call(rbind, pieces)
    used <- unique(path$id)
    path$id <- match(path$id, used)

    list(path = path, labels = labels[used])
}

psychro_grid_label_missing <- function(labels) {
    if (is.null(labels)) return(TRUE)
    if (is.atomic(labels)) return(is.na(labels))

    vapply(labels, function(label) {
        length(label) == 0L || anyNA(as.character(label))
    }, logical(1))
}

psychro_grid_normalise_labels <- function(labels, parse = FALSE) {
    if (is.null(labels)) return(labels)
    if (is.expression(labels)) return(labels)
    if (is.list(labels) && !is.data.frame(labels)) {
        return(as.expression(labels))
    }
    if (!parse) return(labels)

    as.expression(lapply(as.character(labels), function(label) {
        parsed <- parse(text = label)
        if (length(parsed)) parsed[[1L]] else NA
    }))
}

psychro_grid_label_style_defaults <- function(type, theme, style = list()) {
    line <- ggplot2::calc_element(
        paste("psychro.panel.grid.major", type, sep = "."),
        theme
    )
    defaults <- list(
        colour = line$colour %||% "black",
        size = 3.2,
        alpha = NA_real_,
        family = "",
        fontface = 1,
        lineheight = 1.2,
        vjust = -0.3,
        halign = "center",
        gap = NA,
        upright = TRUE,
        straight = FALSE,
        padding = grid::unit(0.05, "inch"),
        text_smoothing = 0,
        remove_long = FALSE
    )

    utils::modifyList(defaults, style)
}

psychro_grid_alpha <- function(colour, alpha) {
    if (is.null(alpha) || length(alpha) == 0L || is.na(alpha)) {
        return(colour)
    }
    grDevices::adjustcolor(colour, alpha.f = alpha)
}

psychro_inside_polygon <- function(x, y, polygon_x, polygon_y, tolerance = 1e-8) {
    n <- length(polygon_x)
    inside <- rep(FALSE, length(x))
    on_boundary <- rep(FALSE, length(x))
    j <- n

    for (i in seq_len(n)) {
        xi <- polygon_x[[i]]
        yi <- polygon_y[[i]]
        xj <- polygon_x[[j]]
        yj <- polygon_y[[j]]

        cross <- (x - xi) * (yj - yi) - (y - yi) * (xj - xi)
        within <- x >= min(xi, xj) - tolerance &
            x <= max(xi, xj) + tolerance &
            y >= min(yi, yj) - tolerance &
            y <= max(yi, yj) + tolerance
        on_boundary <- on_boundary | (abs(cross) <= tolerance & within)

        intersects <- ((yi > y) != (yj > y)) &
            (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
        inside[intersects] <- !inside[intersects]
        j <- i
    }

    inside | on_boundary
}

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}
