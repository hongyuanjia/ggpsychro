#' Draw drybulb and hum ratio grid lines
#'
#' @param theme A ggplot [theme][ggplot2::theme]
#' @param x.minor,x.major A numeric vector of x axis minor/major breaks in
#'        **native** units.
#' @param y.minor,y.major A numeric vector of y axis minor/major breaks in
#'        **native** units.
#' @param x.limits,y.limits A length-2 numeric vector of x and y axis limits in
#'        the original coordinate system, i.e. not-transformed.
#' @param pressure A single number of atmospheric pressure in \[Pa\].
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#' @param mollier A single logical value indicating weather a Mollier plot is
#'        desired
#' @noRd
guide_grid_psychro <- function(theme,
                               x, y,
                               x.minor, xy.minor, x.major, xy.major,
                               y.minor, yx.minor, y.major, yx.major,
                               mollier) {
    bound_x <- c(1, 0)[c(!mollier, mollier)]
    bound_y <- c(0, 1)[c(!mollier, mollier)]

    init_vert <- function(v1, v2end, v2bound, type = "y") {
        v2 <- rep(0:1, length(v1))
        v2[v2 == v2bound] <- v2end

        out <- list(v1 = rep(v1, each = 2L), v2 = v2)

        if (type == "x") {
            names(out)[1:2] <- c("x", "y")
        } else {
            names(out)[1:2] <- c("y", "x")
        }

        out$id.lengths <- rep(2, length(v1))
        out
    }

    grob <- grid::grobTree(
        ggplot2::element_render(theme, "panel.background"),
        if (length(x) && length(y)) {
            if (mollier) {
                vx <- rev(x)
                vy <- rev(y)
                vx <- c(0.0, 0.0, 1.0, 1.0,   vx,  vx[length(vx)])
                vy <- c(0.0, 1.0, 1.0, vy[1L], vy, 0.0           )
            } else {
                vx <- c(0.0, 0.0, x, 1.0, 1.0)
                vy <- c(0.0, y[1L],   y, 1.0, 0.0)
            }

            ggplot2::element_render(
                theme, "psychro.panel.background",
                vx, vy
            )
        },
        if (length(y.minor)) {
            vert <- init_vert(y.minor, yx.minor, bound_y, "y")
            ggplot2::element_render(
                theme, "panel.grid.minor.y",
                x = vert$x, y = vert$y, id.lengths = vert$id.lengths
            )
        },
        if (length(y.major)) {
            vert <- init_vert(y.major, yx.major, bound_y, "y")
            ggplot2::element_render(
                theme, "panel.grid.major.y",
                x = vert$x, y = vert$y, id.lengths = vert$id.lengths
            )
        },
        if (length(x.minor)) {
            vert <- init_vert(x.minor, xy.minor, bound_x, "x")
            ggplot2::element_render(
                theme, "panel.grid.minor.x",
                x = vert$x, y = vert$y, id.lengths = vert$id.lengths
            )
        },
        if (length(x.major)) {
            vert <- init_vert(x.major, xy.major, bound_x, "x")
            ggplot2::element_render(
                theme, "panel.grid.major.x",
                x = vert$x, y = vert$y, id.lengths = vert$id.lengths
            )
        },
        if (length(x) && length(y)) {
            ggplot2::element_render(
                theme, "psychro.panel.grid.satuation",
                x = x, y = y
            )
        }
    )
    grob$name <- grid::grobName(grob, "grill")

    grob
}
