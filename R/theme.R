#' Custom theme for psychrometric chart.
#'
#' @inheritParams ggplot2::theme_grey
#' @return A ggplot2 theme.
#'
#' `theme_psychro_ashrae()` and `theme_psychro_minimal()` are inspired by
#' psychrochart's ASHRAE and minimal chart styles.
#'
#' @seealso
#' * [psychrochart ASHRAE style](https://github.com/azogue/psychrochart/blob/master/psychrochart/chart_styles/ashrae_chart_style.json)
#' * [psychrochart minimal style](https://github.com/azogue/psychrochart/blob/master/psychrochart/chart_styles/minimal_chart_style.json)
#'
#' @author Hongyuan Jia
#' @importFrom ggplot2 "%+replace%"
#' @examples
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     theme_grey_psychro()
#'
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     theme_gray_psychro()
#'
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     theme_psychro()
#'
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     theme_psychro_ashrae()
#'
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     theme_psychro_minimal()
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
         panel.background = ggplot2::element_rect(fill = "white", colour = NA),
         axis.line.x = ggplot2::element_line(color = grDevices::rgb(0.2, 0.2, 0.2, 1.0)),
         axis.line.y = ggplot2::element_line(color = grDevices::rgb(0.2, 0.2, 0.2, 1.0)),
         psychro.panel.background = element_polygon(fill = "white", color = NA),
         psychro.panel.mask = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114, 1.0), linetype = 3, linewidth = 0.15),
         panel.grid.minor.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114, 1.0), linetype = 3, linewidth = 0.15),
         panel.grid.major.y = ggplot2::element_line(color = grDevices::rgb(0.0, 0.125, 0.376, 1.0), linetype = 3, linewidth = 0.15),
         panel.grid.minor.y = ggplot2::element_line(color = grDevices::rgb(0.0, 0.125, 0.376, 1.0), linetype = 3, linewidth = 0.15),
         psychro.panel.grid.saturation = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114), linewidth = 1),
         psychro.panel.grid.relhum = ggplot2::element_line(color = grDevices::rgb(0.0, 0.498, 1.0, 1.0), linewidth = 0.4, linetype = "dotdash"),
         psychro.panel.grid.wetbulb = ggplot2::element_line(color = grDevices::rgb(0.498, 0.875, 1.0, 1.0), linewidth = 0.2),
         psychro.panel.grid.vappres = ggplot2::element_line(color = "gray60", linewidth = 0.2),
         psychro.panel.grid.specvol = ggplot2::element_line(color = grDevices::rgb(0.0, 0.502, 0.337, 1.0), linewidth = 0.2, linetype = "longdash"),
         psychro.panel.grid.enthalpy = ggplot2::element_line(color = grDevices::rgb(0.251, 0.0, 0.502, 1.0), linewidth = 0.2),
         psychro.panel.protractor = ggplot2::element_line(color = "black", linewidth = 0.3),
         psychro.panel.protractor.text = ggplot2::element_text(color = "black", size = 1.9)
    )
}

#' @name theme
#' @export
theme_psychro_ashrae <- function(base_size = 11, base_family = "",
                                 base_line_size = base_size/22,
                                 base_rect_size = base_size/22) {
    ggplot2::theme_grey(
        base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size
        ) %+replace%
    ggplot2::theme(
         panel.border = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.6),
         panel.background = ggplot2::element_rect(fill = "white", colour = NA),
         axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.6),
         axis.line.y = ggplot2::element_line(color = "black", linewidth = 0.6),
         axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.45),
         axis.text = ggplot2::element_text(color = "black"),
         axis.title = ggplot2::element_text(color = "black"),
         psychro.panel.background = element_polygon(fill = "white", color = NA),
         psychro.panel.mask = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = 1, linewidth = 0.12),
         panel.grid.minor.x = ggplot2::element_line(color = "black", linetype = 1, linewidth = 0.10),
         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = 1, linewidth = 0.12),
         panel.grid.minor.y = ggplot2::element_line(color = "black", linetype = 1, linewidth = 0.10),
         psychro.panel.grid.saturation = ggplot2::element_line(color = "black", linewidth = 0.9),
         psychro.panel.grid.relhum = ggplot2::element_line(color = "black", linewidth = 0.35, linetype = "solid"),
         psychro.panel.grid.major.relhum = ggplot2::element_line(color = "black", linewidth = 0.35, linetype = "solid"),
         psychro.panel.grid.minor.relhum = ggplot2::element_blank(),
         psychro.panel.grid.vappres = ggplot2::element_blank(),
         psychro.panel.grid.major.vappres = ggplot2::element_blank(),
         psychro.panel.grid.minor.vappres = ggplot2::element_blank(),
         psychro.panel.grid.wetbulb = ggplot2::element_line(color = "black", linewidth = 0.35, linetype = "dashed"),
         psychro.panel.grid.major.wetbulb = ggplot2::element_line(color = "black", linewidth = 0.35, linetype = "dashed"),
         psychro.panel.grid.minor.wetbulb = ggplot2::element_line(color = "black", linewidth = 0.18, linetype = "dashed"),
         psychro.panel.grid.specvol = ggplot2::element_line(color = "black", linewidth = 0.18, linetype = "solid"),
         psychro.panel.grid.major.specvol = ggplot2::element_line(color = "black", linewidth = 0.18, linetype = "solid"),
         psychro.panel.grid.minor.specvol = ggplot2::element_line(color = "black", linewidth = 0.10, linetype = "solid"),
         psychro.panel.grid.enthalpy = ggplot2::element_line(color = "black", linewidth = 0.25, linetype = "solid"),
         psychro.panel.grid.major.enthalpy = ggplot2::element_line(color = "black", linewidth = 0.25, linetype = "solid"),
         psychro.panel.grid.minor.enthalpy = ggplot2::element_line(color = "black", linewidth = 0.14, linetype = "solid"),
         psychro.panel.protractor = ggplot2::element_line(color = "black", linewidth = 0.25),
         psychro.panel.protractor.text = ggplot2::element_text(color = "black", size = 1.9)
    )
}

#' @name theme
#' @export
theme_psychro_minimal <- function(base_size = 11, base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22) {
    ggplot2::theme_grey(
        base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size
        ) %+replace%
    ggplot2::theme(
         panel.border = ggplot2::element_blank(),
         panel.background = ggplot2::element_rect(fill = "white", colour = NA),
         axis.line.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114), linewidth = 0.75),
         axis.line.y = ggplot2::element_line(color = grDevices::rgb(0, 0.125, 0.376), linewidth = 0.75),
         axis.ticks.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114), linewidth = 0.35),
         axis.ticks.y = ggplot2::element_line(color = grDevices::rgb(0, 0.125, 0.376), linewidth = 0.35),
         axis.text.x = ggplot2::element_text(color = grDevices::rgb(0.855, 0.145, 0.114)),
         axis.text.y = ggplot2::element_text(color = grDevices::rgb(0, 0.125, 0.376)),
         axis.title.x = ggplot2::element_text(color = grDevices::rgb(0.855, 0.145, 0.114)),
         axis.title.y = ggplot2::element_text(
             color = grDevices::rgb(0, 0.125, 0.376),
             angle = 90
         ),
         psychro.panel.background = element_polygon(fill = "white", color = NA),
         psychro.panel.mask = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114), linetype = "dotted", linewidth = 0.20),
         panel.grid.minor.x = ggplot2::element_blank(),
         panel.grid.major.y = ggplot2::element_blank(),
         panel.grid.minor.y = ggplot2::element_blank(),
         psychro.panel.grid.saturation = ggplot2::element_line(color = grDevices::rgb(0.855, 0.145, 0.114), linewidth = 1.4),
         psychro.panel.grid.relhum = ggplot2::element_line(color = grDevices::rgb(0, 0.498, 1), linewidth = 0.5, linetype = "solid"),
         psychro.panel.grid.major.relhum = ggplot2::element_line(color = grDevices::rgb(0, 0.498, 1), linewidth = 0.5, linetype = "solid"),
         psychro.panel.grid.minor.relhum = ggplot2::element_blank(),
         psychro.panel.grid.wetbulb = ggplot2::element_line(color = grDevices::rgb(0.498, 0.875, 1), linewidth = 0.25, linetype = "solid"),
         psychro.panel.grid.major.wetbulb = ggplot2::element_line(color = grDevices::rgb(0.498, 0.875, 1), linewidth = 0.25, linetype = "solid"),
         psychro.panel.grid.minor.wetbulb = ggplot2::element_line(color = grDevices::rgb(0.498, 0.875, 1), linewidth = 0.20, linetype = "solid"),
         psychro.panel.grid.vappres = ggplot2::element_blank(),
         psychro.panel.grid.major.vappres = ggplot2::element_blank(),
         psychro.panel.grid.minor.vappres = ggplot2::element_blank(),
         psychro.panel.grid.specvol = ggplot2::element_line(color = grDevices::rgb(0, 0.502, 0.337), linewidth = 0.5, linetype = "solid"),
         psychro.panel.grid.major.specvol = ggplot2::element_line(color = grDevices::rgb(0, 0.502, 0.337), linewidth = 0.5, linetype = "solid"),
         psychro.panel.grid.minor.specvol = ggplot2::element_blank(),
         psychro.panel.grid.enthalpy = ggplot2::element_line(color = grDevices::rgb(0.251, 0, 0.502), linewidth = 0.5, linetype = "solid"),
         psychro.panel.grid.major.enthalpy = ggplot2::element_line(color = grDevices::rgb(0.251, 0, 0.502), linewidth = 0.5, linetype = "solid"),
         psychro.panel.grid.minor.enthalpy = ggplot2::element_blank(),
         psychro.panel.protractor = ggplot2::element_line(color = "black", linewidth = 0.25),
         psychro.panel.protractor.text = ggplot2::element_text(color = "black", size = 1.9)
    )
}

#' Apply a psychrometric chart preset
#'
#' `psychro_preset()` returns a list of ggplot additions that can be added to a
#' [ggpsychro()] plot. Presets make selected reference grids explicit, configure
#' grid labels, and apply a matching psychrometric chart theme. The `"ashrae"`
#' and `"minimal"` presets are inspired by psychrochart's chart styles.
#'
#' @param name A preset name. One of `"ashrae"` or `"minimal"`.
#' @param labels A single logical value. If `FALSE`, preset grid labels are
#'   hidden while grid visibility and theme styling are kept.
#'
#' @return A list of ggplot additions.
#' @export
#'
#' @examples
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     psychro_preset("ashrae")
#'
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
#'     psychro_preset("minimal", labels = FALSE)
#'
#' @importFrom checkmate assert_flag
psychro_preset <- function(name = c("ashrae", "minimal"), labels = TRUE) {
    name <- match.arg(name)
    assert_flag(labels)

    switch(name,
        ashrae = list(
            scale_drybulb_continuous(
                breaks = psychro_regular_breaks(5),
                minor_breaks = psychro_regular_breaks(1)
            ),
            scale_humratio_continuous(
                breaks = psychro_regular_breaks(5),
                minor_breaks = psychro_regular_breaks(0.5)
            ),
            scale_relhum_continuous(
                breaks = seq(10, 90, by = 10),
                minor_breaks = NULL
            ),
            scale_wetbulb_continuous(
                breaks = seq(0, 30, by = 5),
                minor_breaks = seq(-10, 35, by = 1)
            ),
            scale_specvol_continuous(
                limits = c(0.78, 0.96),
                breaks = seq(0.80, 0.95, by = 0.05),
                minor_breaks = seq(0.78, 0.96, by = 0.01)
            ),
            scale_enthalpy_continuous(
                limits = c(10000, 130000),
                breaks = seq(50000, 100000, by = 50000),
                minor_breaks = seq(10000, 130000, by = 10000)
            ),
            geom_grid_relhum(label = labels, label.size = 3.1),
            geom_grid_wetbulb(label = labels, label_loc = 0.12, label.size = 2.8),
            geom_grid_vappres(show = FALSE),
            geom_grid_specvol(label = labels, label.size = 2.7),
            geom_grid_enthalpy(label = labels, label.size = 2.7),
            theme_psychro_ashrae()
        ),
        minimal = list(
            scale_drybulb_continuous(
                breaks = psychro_regular_breaks(5),
                minor_breaks = NULL
            ),
            scale_relhum_continuous(
                breaks = seq(20, 80, by = 20),
                minor_breaks = NULL
            ),
            scale_wetbulb_continuous(
                breaks = seq(10, 30, by = 10),
                minor_breaks = seq(-10, 35, by = 5)
            ),
            scale_specvol_continuous(
                limits = c(0.86, 0.98),
                breaks = seq(0.86, 0.98, by = 0.04),
                minor_breaks = NULL
            ),
            scale_enthalpy_continuous(
                limits = c(20000, 120000),
                breaks = seq(20000, 100000, by = 40000),
                minor_breaks = NULL
            ),
            geom_grid_relhum(label = FALSE),
            geom_grid_wetbulb(label = labels, label_loc = 0.22, label.size = 2.7),
            geom_grid_vappres(show = FALSE),
            geom_grid_specvol(label = FALSE),
            geom_grid_enthalpy(label = FALSE),
            theme_psychro_minimal()
        )
    )
}

psychro_regular_breaks <- function(by) {
    force(by)

    function(limits) {
        limits <- limits[is.finite(limits)]
        if (!length(limits)) return(numeric())
        limits <- range(limits)
        breaks <- seq(
            floor(limits[[1L]] / by) * by,
            ceiling(limits[[2L]] / by) * by,
            by = by
        )
        breaks[breaks >= limits[[1L]] & breaks <= limits[[2L]]]
    }
}
