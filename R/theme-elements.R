#' Polygon theme element for psychrometric chart panels
#'
#' `element_polygon()` is used by ggpsychro-specific theme elements such as
#' `psychro.panel.background` and `psychro.panel.mask`.
#'
#' @param fill Fill colour.
#' @param colour,color Border colour. `color` is an alias for `colour`.
#' @param size Border width.
#' @param linetype Border linetype.
#' @param inherit.blank Whether this element inherits from blank elements.
#'
#' @return A ggplot2 theme element.
#' @export
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
#' @importFrom ggplot2 element_grob
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

register_element_polygon_merge <- function() {
    ggplot2_ns <- asNamespace("ggplot2")
    if (!exists("merge_element", envir = ggplot2_ns, inherits = FALSE)) {
        return(invisible(FALSE))
    }

    merge_element <- get("merge_element", envir = ggplot2_ns, inherits = FALSE)
    S7::method(merge_element, list(S7::new_S3_class("element_polygon"), S7::class_any)) <- function(new, old, ...) {
        if (S7::S7_inherits(old)) {
            old <- S7::props(old)
        }
        if (is.null(old) || inherits(old, "element_blank")) {
            return(new)
        }

        idx <- lengths(new) == 0L
        idx <- names(idx[idx])
        new[idx] <- old[idx]
        new
    }

    invisible(TRUE)
}
