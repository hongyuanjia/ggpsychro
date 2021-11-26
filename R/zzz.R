.onLoad <- function(...) {
    ggplot2::register_theme_elements(
        psychro.panel.mask = element_polygon(
            fill = "white", color = NA, size = 0.5, linetype = 1
        ),
        psychro.panel.background = element_polygon(
            fill = "gray92", color = NA, size = 0.5, linetype = 1
        ),
        psychro.panel.grid                = ggplot2::element_line(),
        psychro.panel.grid.saturation     = ggplot2::element_line(),
        psychro.panel.grid.relhum         = ggplot2::element_line(),
        psychro.panel.grid.major.relhum   = ggplot2::element_line(),
        psychro.panel.grid.minor.relhum   = ggplot2::element_line(),
        psychro.panel.grid.wetbulb        = ggplot2::element_line(),
        psychro.panel.grid.major.wetbulb  = ggplot2::element_line(),
        psychro.panel.grid.minor.wetbulb  = ggplot2::element_line(),
        psychro.panel.grid.vappres        = ggplot2::element_line(),
        psychro.panel.grid.major.vappres  = ggplot2::element_line(),
        psychro.panel.grid.minor.vappres  = ggplot2::element_line(),
        psychro.panel.grid.specvol        = ggplot2::element_line(),
        psychro.panel.grid.major.specvol  = ggplot2::element_line(),
        psychro.panel.grid.minor.specvol  = ggplot2::element_line(),
        psychro.panel.grid.enthalpy       = ggplot2::element_line(),
        psychro.panel.grid.major.enthalpy = ggplot2::element_line(),
        psychro.panel.grid.minor.enthalpy = ggplot2::element_line(),

        element_tree = list(
            psychro.panel.mask                = ggplot2::el_def("element_polygon"),
            psychro.panel.background          = ggplot2::el_def("element_polygon"),
            psychro.panel.grid                = ggplot2::el_def("element_line", "panel.grid"),
            psychro.panel.grid.saturation     = ggplot2::el_def("element_line", "psychro.panel.grid"),
            psychro.panel.grid.relhum         = ggplot2::el_def("element_line", "psychro.panel.grid"),
            psychro.panel.grid.major.relhum   = ggplot2::el_def("element_line", "psychro.panel.grid.relhum"),
            psychro.panel.grid.minor.relhum   = ggplot2::el_def("element_line", "psychro.panel.grid.relhum"),
            psychro.panel.grid.wetbulb        = ggplot2::el_def("element_line", "psychro.panel.grid"),
            psychro.panel.grid.major.wetbulb  = ggplot2::el_def("element_line", "psychro.panel.grid.wetbulb"),
            psychro.panel.grid.minor.wetbulb  = ggplot2::el_def("element_line", "psychro.panel.grid.wetbulb"),
            psychro.panel.grid.vappres        = ggplot2::el_def("element_line", "psychro.panel.grid"),
            psychro.panel.grid.major.vappres  = ggplot2::el_def("element_line", "psychro.panel.grid.vappres"),
            psychro.panel.grid.minor.vappres  = ggplot2::el_def("element_line", "psychro.panel.grid.vappres"),
            psychro.panel.grid.specvol        = ggplot2::el_def("element_line", "psychro.panel.grid"),
            psychro.panel.grid.major.specvol  = ggplot2::el_def("element_line", "psychro.panel.grid.specvol"),
            psychro.panel.grid.minor.specvol  = ggplot2::el_def("element_line", "psychro.panel.grid.specvol"),
            psychro.panel.grid.enthalpy       = ggplot2::el_def("element_line", "psychro.panel.grid"),
            psychro.panel.grid.major.enthalpy = ggplot2::el_def("element_line", "psychro.panel.grid.enthalpy"),
            psychro.panel.grid.minor.enthalpy = ggplot2::el_def("element_line", "psychro.panel.grid.enthalpy")
        )
    )
}
