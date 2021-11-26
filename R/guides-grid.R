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
#' @param mollier A single logical value indicating weather a Mollier plot is
#'        desired
#' @noRd
guide_grid_psychro <- function(theme, tdb.minor, tdb.major, hum.minor, hum.major,
                               saturation,
                               rh.minor, rh.major, twb.minor, twb.major,
                               vappres.minor, vappres.major, specvol.minor, specvol.major,
                               enthalpy.minor, enthalpy.major, mollier) {
    # create psychrometric chart panel
    if (mollier) {
        psychro_panel <- ggplot2::element_render(
            theme, "psychro.panel.background",
            x = c(0.0, 0.0, 1.0, rev(saturation$hum), saturation$hum[1L]),
            y = c(0.0, 1.0, 1.0, rev(saturation$tdb), 0.0)
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
        psychro_panel <- ggplot2::element_render(
            theme, "psychro.panel.background",
            x = c(0.0, 0.0,                saturation$tdb, 1.0, 1.0),
            y = c(0.0, saturation$hum[1L], saturation$hum, 1.0, 0.0)
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

    grill <- grid::grobTree(
        ggplot2::element_render(theme, "panel.background"),

        psychro_mask, psychro_panel,

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
