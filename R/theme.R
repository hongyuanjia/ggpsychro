# theme_psychro {{{
#' Custom theme for psychrometric chart.
#'
#' @param asp Aspect ratio of plot. Defaults to NULL.
#'
#' @return A ggplot2 theme.
#'
#' @keywords internal
#' @author Hongyuan Jia
#' @importFrom ggplot2 element_blank element_line element_rect element_text theme theme_bw
#' @export
#' @examples
#' theme_psychro()
#'
theme_grey_psychro <- function(base_size = 11, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22) {
    theme_grey(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size, base_rect_size = base_rect_size) +
        theme(
            panel.background = element_rect(fill = NA, color = NA),
            psychro.panel.background = element_polygon(fill = "gray92", color = NA)
        )
}
theme_gray_psychro <- theme_grey_psychro
# }}}

# theme_psychro_ashrae {{{
theme_psychro_ashrae <- function (base_size = 11, base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22) {
    theme_bw() +
    ggplot2::theme(
         axis.line = element_line(color = "black", size = 0.8, linetype = 1),
         axis.text = element_text(color = "black", size = 8),
         axis.ticks = element_line(size = 0.2),
         axis.ticks.length.x = unit(0.3, "lines"),
         axis.ticks.length.y = unit(0.3, "lines"),
         axis.text.x.bottom = element_text(margin = ggplot2::margin(t = .5, unit = "lines")),
         panel.border = element_blank()
        )
}
# }}}
