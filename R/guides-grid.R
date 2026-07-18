#' Draw drybulb and hum ratio grid lines
#'
#' @param theme A ggplot [theme][ggplot2::theme]
#' @param axis A list of dry-bulb and humidity axis major/minor positions.
#' @param saturation A list of panel saturation polygon coordinates.
#' @param grid A named list of psychrometric grid major/minor line data.
#' @param mollier A single logical value indicating whether a Mollier plot is
#'        desired
#' @noRd
guide__grid_psychro <- function(
    theme,
    axis,
    saturation,
    grid,
    grid.labels,
    mollier
) {
    # create psychrometric chart panel
    panel <- panel__polygon(saturation, mollier)
    panel_x <- panel$x
    panel_y <- panel$y
    psychro_mask <- panel__mask_grob(theme, panel_x, panel_y)
    psychro_panel <- panel__background_grob(theme, panel_x, panel_y)
    psychro_panel_clip <- grid::polygonGrob(
        panel_x,
        panel_y,
        gp = grid::gpar(col = NA, fill = NA),
        name = "psychro-panel-clip"
    )

    if (mollier) {
        nm_tdb <- "y"
        nm_hum <- "x"
    } else {
        nm_tdb <- "x"
        nm_hum <- "y"
    }

    axis_grid <- function(x, type, var) {
        vx <- rep(x, each = 2L)
        vy <- rep(0:1, length(x))
        v <- if (var == "x") list(x = vx, y = vy) else list(x = vy, y = vx)

        ggplot2::element_render(
            theme,
            paste("panel.grid", type, var, sep = "."),
            x = v$x,
            y = v$y,
            id.lengths = rep(2, length(x))
        )
    }

    psychro_grid <- function(x, type, var) {
        ggplot2::element_render(
            theme,
            paste("psychro.panel.grid", type, var, sep = "."),
            x = x[[c("tdb", "hum")[c(!mollier, mollier)]]],
            y = x[[c("tdb", "hum")[c(mollier, !mollier)]]],
            id.lengths = rep(x$len, x$n)
        )
    }

    grid__label <- function(x, var) {
        guide_label__grob(
            x,
            grid.labels[[var]],
            var,
            theme,
            mollier,
            panel_x,
            panel_y
        )
    }

    axis_grobs <- guide__axis_grobs(
        axis,
        nm_tdb,
        nm_hum,
        psychro_panel_clip,
        axis_grid
    )
    grid_grobs <- guide__curved_grid_grobs(
        grid,
        psychro_panel_clip,
        psychro_grid
    )
    label_grobs <- guide__curved_label_grobs(grid, grid__label)

    grill <- do.call(
        grid::grobTree,
        c(
            list(
                ggplot2::element_render(theme, "panel.background"),
                psychro_mask,
                psychro_panel
            ),
            axis_grobs,
            grid_grobs,
            label_grobs
        )
    )

    grill$name <- grid::grobName(grill, "grill")
    grill
}

# Assemble Cartesian panel grid lines from a data table so dry-bulb and humidity
# major/minor guide order stays explicit without four duplicated branches.
guide__axis_grobs <- function(axis, nm_tdb, nm_hum, panel_clip, render_axis) {
    spec <- list(
        list(values = axis$hum$minor, type = "minor", var = nm_hum),
        list(values = axis$tdb$minor, type = "minor", var = nm_tdb),
        list(values = axis$hum$major, type = "major", var = nm_hum),
        list(values = axis$tdb$major, type = "major", var = nm_tdb)
    )

    guide__compact_grobs(lapply(spec, function(item) {
        if (!length(item$values)) {
            return(NULL)
        }
        panel__clip_grob(
            panel_clip,
            render_axis(item$values, item$type, item$var)
        )
    }))
}

# Assemble psychrometric curved grids in the same visual order as the previous
# hand-written branches: all line families first, labels later.
guide__curved_grid_grobs <- function(grid, panel_clip, render_grid) {
    pieces <- lapply(names(grid), function(var) {
        lapply(c("minor", "major"), function(type) {
            lines <- grid[[var]][[type]]
            if (!length(lines)) {
                return(NULL)
            }
            panel__clip_grob(panel_clip, render_grid(lines, type, var))
        })
    })
    guide__compact_grobs(do.call(c, pieces))
}

# Labels attach only to major psychrometric grids, matching the drawn line data
# produced by coord setup.
guide__curved_label_grobs <- function(grid, render_label) {
    guide__compact_grobs(lapply(names(grid), function(var) {
        lines <- grid[[var]]$major
        if (!length(lines)) {
            return(NULL)
        }
        render_label(lines, var)
    }))
}

# grid::grobTree() accepts grobs, not placeholder NULLs. Compacting in one
# helper keeps the data-driven guide assembly readable.
guide__compact_grobs <- function(grobs) {
    grobs[!vapply(grobs, is.null, logical(1L))]
}

# Match floating-point break values after inverse transforms and rescaling.
guide__match_break_values <- function(x, table, tolerance = 1e-8) {
    vapply(
        x,
        function(value) {
            match <- which(abs(table - value) <= tolerance)
            if (length(match)) match[[1L]] else NA_integer_
        },
        integer(1)
    )
}
