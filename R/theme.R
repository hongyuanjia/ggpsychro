#' Custom theme for psychrometric chart.
#'
#' @param asp Aspect ratio of plot. Defaults to NULL.
#'
#' @return A ggplot2 theme.
#'
#' @keywords internal
#' @author Hongyuan Jia
#' @examples
#' theme_psychro()
#' @export
theme_psychro <- function(background = "white", base_size = 11, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22) {
    # use theme_bw as the base
    th <- theme_bw(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size, base_rect_size = base_rect_size
    )

    # flip y axis
    th <- th + theme(axis.title.y.right = element_text(angle = 90))

    # set chart background
    th <- th + theme(panel.background = element_rect(fill = background, color = background))

    # remove all grids
    th <- th + theme(panel.grid = element_blank())

    # plot margins
    th + theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
