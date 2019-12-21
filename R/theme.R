# theme_psychro {{{
#' Custom theme for psychrometric chart.
#'
#' @param asp Aspect ratio of plot. Defaults to NULL.
#'
#' @return A ggplot2 theme.
#'
#' @keywords internal
#' @author Hongyuan Jia
#' @importFrom ggplot2 element_line element_blank element_rect theme theme_bw
#' @examples
#' theme_psychro()
theme_psychro <- function(background = "white", base_size = 11, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22) {
    # use theme_bw as the base
    th <- theme_bw(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size, base_rect_size = base_rect_size
    )

    th <- th + theme(
        # color for grids
        panel.grid.major.x = element_line(color = "#870021", size = 0.15),
        panel.grid.minor.x = element_line(color = "#870021", size = 0.15),
        panel.grid.major.y = element_line(color = "#4E6390", size = 0.15),
        panel.grid.minor.y = element_line(color = "#4E6390", size = 0.15),

        axis.line.x = element_line(color = "#313329"),
        axis.line.y = element_line(color = "#313329"),

        axis.text.x = element_text(color = "#313329"),
        axis.text.y = element_text(color = "#313329"),

        axis.ticks.x = element_line(color = "#313329"),
        axis.ticks.y = element_line(color = "#313329"),

        # remove panel border
        panel.border = element_blank(),
        # set chart background
        panel.background = element_rect(fill = background, size = 0.8)
    )

    th
}
# }}}

# theme_psychro_ashrae {{{
theme_psychro_ashrae <- function (base_size = 11, base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22) {
    theme_bw() +
    ggplot2::theme(aspect.ratio = 9/16,
         axis.line = element_line(color = "black", size = 0.8, linetype = 1),
         axis.text = element_text(color = "black", size = 8),
         axis.ticks = element_line(size = 0.2),
         axis.ticks.length.x = unit(0.3, "lines"),
         axis.ticks.length.y = unit(0.3, "lines"),
         axis.text.x.bottom = element_text(margin = margin(t = .5, unit = "lines")),
         panel.border = element_blank()
        )
}
# }}}
