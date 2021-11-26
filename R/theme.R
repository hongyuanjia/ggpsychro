#' Custom theme for psychrometric chart.
#'
#' @inheritParams ggplot2::theme_grey
#' @return A ggplot2 theme.
#'
#' @author Hongyuan Jia
#' @importFrom ggplot2 "%+replace%"
#' @examples
#' theme_psychro()
#'
#' @name theme
#' @export
theme_grey_psychro <- ggplot2::theme_grey
#' @name theme
#' @export
theme_gray_psychro <- theme_grey_psychro

#' @name theme
#' @export
theme_psychro <- function(base_size = 11, base_family = "",
                           base_line_size = base_size/22,
                           base_rect_size = base_size/22) {
    ggplot2::theme_grey(
        base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size
        ) %+replace%
    ggplot2::theme(
         panel.border = ggplot2::element_blank(),
         panel.background = ggplot2::element_blank(),
         axis.line.x = ggplot2::element_line(color = grDevices::rgb(0.2, 0.2, 0.2, 1.0)),
         axis.line.y = ggplot2::element_line(color = grDevices::rgb(0.2, 0.2, 0.2, 1.0)),
         psychro.panel.background = element_polygon(fill = "white", color = NA),
         psychro.panel.mask = element_polygon(fill = "white", color = NA),
         panel.grid.major.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114, 1.0), linetype = 3, size = 0.15),
         panel.grid.minor.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114, 1.0), linetype = 3, size = 0.15),
         panel.grid.major.y = ggplot2::element_line(color = grDevices::rgb(0.0, 0.125, 0.376, 1.0), linetype = 3, size = 0.15),
         panel.grid.minor.y = ggplot2::element_line(color = grDevices::rgb(0.0, 0.125, 0.376, 1.0), linetype = 3, size = 0.15),
         psychro.panel.grid.saturation = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114), size = 1),
         psychro.panel.grid.relhum = ggplot2::element_line(color = grDevices::rgb(0.0, 0.498, 1.0, 1.0), size = 0.4, linetype = "dotdash"),
         psychro.panel.grid.wetbulb = ggplot2::element_line(color = grDevices::rgb(0.498, 0.875, 1.0, 1.0), size = 0.2),
         psychro.panel.grid.vappres = ggplot2::element_line(color = "gray60", size = 0.2),
         psychro.panel.grid.specvol = ggplot2::element_line(color = grDevices::rgb(0.0, 0.502, 0.337, 1.0), size = 0.2, linetype = "longdash"),
         psychro.panel.grid.enthalpy = ggplot2::element_line(color = grDevices::rgb(0.251, 0.0, 0.502, 1.0), size = 0.2)
    )
}
