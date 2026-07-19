# Build and clip the polygonal panel pieces used by psychrometric guides.
# Compute the normalized valid psychrometric panel polygon from saturation data.
panel__polygon <- function(saturation, mollier = FALSE) {
    if (mollier) {
        return(list(
            x = c(0.0, 0.0, 1.0, rev(saturation$hum), saturation$hum[1L]),
            y = c(0.0, 1.0, 1.0, rev(saturation$tdb), 0.0)
        ))
    }

    list(
        x = c(0.0, 0.0, saturation$tdb, 1.0, 1.0),
        y = c(0.0, saturation$hum[1L], saturation$hum, 1.0, 0.0)
    )
}

# Draw the mask outside the valid psychrometric panel polygon.
panel__mask_grob <- function(theme, x, y) {
    element <- ggplot2::calc_element("psychro.panel.mask", theme)
    if (is.null(element) || inherits(element, "element_blank")) {
        return(grid::nullGrob())
    }

    # Draw the mask area as a background ring, not as a foreground cover. The
    # even-odd path fills the full panel except the valid psychrometric polygon.
    x <- c(x, x[[1L]])
    y <- c(y, y[[1L]])
    grid::pathGrob(
        x = c(0, 1, 1, 0, 0, x),
        y = c(0, 0, 1, 1, 0, y),
        id.lengths = c(5L, length(x)),
        rule = "evenodd",
        gp = grid::gpar(
            fill = element$fill,
            col = element$colour %||% element$color,
            lwd = (element$linewidth %||% element$size %||% 0) * ggplot2::.pt,
            lty = element$linetype %||% 1,
            linejoin = element$linejoin %||% "mitre"
        ),
        name = "psychro-panel-mask"
    )
}

# Draw the optional background fill inside the valid psychrometric panel.
panel__background_grob <- function(theme, x, y) {
    element <- ggplot2::calc_element("psychro.panel.background", theme)
    if (is.null(element) || inherits(element, "element_blank")) {
        return(grid::nullGrob())
    }
    if (inherits(element, "ggplot2::element_polygon")) {
        return(ggplot2::element_render(
            theme,
            "psychro.panel.background",
            x = x,
            y = y,
            name = "psychro-panel-background"
        ))
    }

    grid::polygonGrob(
        x,
        y,
        gp = grid::gpar(
            fill = element$fill,
            col = element$colour %||% element$color,
            lwd = (element$linewidth %||% element$size %||% 0) * ggplot2::.pt,
            lty = element$linetype %||% 1,
            linejoin = element$linejoin %||% "mitre"
        ),
        name = "psychro-panel-background"
    )
}

# Clip a guide grob to the psychrometric panel while preserving split styles.
#' @importFrom gridGeometry polyclipGrob
panel__clip_grob <- function(panel, grob, op = "intersection") {
    if (!identical(op, "intersection")) {
        return(gridGeometry::polyclipGrob(grob, panel, op, name = grob$name))
    }
    split <- coord_clip__split_styled_grob(grob)
    clipped <- lapply(split, coord_clip__polyclip_grob, panel = panel)
    if (length(clipped) == 1L) {
        return(clipped[[1L]])
    }
    do.call(grid::grobTree, clipped)
}
