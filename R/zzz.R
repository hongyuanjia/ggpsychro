.onLoad <- function(...) {
    ggplot2::register_theme_elements(
        psychro.panel.background = element_polygon(
            fill = "gray92", color = NA, size = 0.5, linetype = 1
        ),
        psychro.panel.grid.satuation = element_line(color = "white"),
        psychro.panel.grid.relhum = element_line(),
        psychro.panel.grid.wetbulb = element_line(),
        psychro.panel.grid.vappres = element_line(),
        psychro.panel.grid.specvol = element_line(),
        psychro.panel.grid.enthalpy = element_line(),

        element_tree = list(
            psychro.panel.background     = ggplot2::el_def("element_polygon"),
            psychro.panel.grid.satuation = ggplot2::el_def("element_line", "panel.grid"),
            psychro.panel.grid.relhum    = ggplot2::el_def("element_line", "panel.grid"),
            psychro.panel.grid.wetbulb   = ggplot2::el_def("element_line", "panel.grid"),
            psychro.panel.grid.vappres   = ggplot2::el_def("element_line", "panel.grid"),
            psychro.panel.grid.specvol   = ggplot2::el_def("element_line", "panel.grid"),
            psychro.panel.grid.enthalpy  = ggplot2::el_def("element_line", "panel.grid")
        )
    )
}

