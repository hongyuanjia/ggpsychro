element_polygon <- function(fill = NULL, colour = NULL, size = NULL, linetype = NULL,
                            color = NULL, inherit.blank = FALSE) {
    if (!is.null(color)) colour <- color

    structure(
        list(fill = fill, colour = colour, size = size,
            linetype = linetype, color = color, inherit.blank = inherit.blank
        ),
        class = c("element_polygon", "element")
    )
}

# borrowed from ggplot2
len0_null <- function(x) if (length(x) == 0L) NULL else x

#' @importFrom grid polygonGrob
#' @export
element_grob.element_polygon <- function(element, x = c(0.0, 0.5, 1.0, 0.5), y = c(0.5, 1.0, 0.5, 0.0),
                                         fill = NULL, colour = NULL, size = NULL, linetype = NULL, ...) {

    # The gp settings can override element_gp
    gp <- grid::gpar(lwd = len0_null(size * ggplot2::.pt), col = colour, fill = fill, lty = linetype)

    element_gp <- grid::gpar(
        lwd = len0_null(element$size * ggplot2::.pt),
        col = element$colour, fill = element$fill, lty = element$linetype
    )

    grid::polygonGrob(x, y, gp = utils::modifyList(element_gp, gp), ...)
}
