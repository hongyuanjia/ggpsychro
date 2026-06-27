#' Add psychrometric grid lines
#'
#' These helpers provide ggplot-style controls for psychrometric reference
#' grids. They do not add data layers; they mark a grid as visible and let
#' [coord_psychro()] render it in the panel background.
#'
#' @param ... Line style settings passed to [ggplot2::element_line()], such as
#'   `colour`, `color`, `linewidth`, `size`, and `linetype`. Unused label
#'   arguments are accepted for compatibility and ignored.
#' @param show A single logical value. If `FALSE`, hide the corresponding grid.
#'
#' @rdname geom_grid
#' @export
geom_grid_relhum <- function(..., show = TRUE) {
    psychro_grid_layer("relhum", ..., show = show)
}

#' @rdname geom_grid
#' @export
geom_grid_wetbulb <- function(..., show = TRUE) {
    psychro_grid_layer("wetbulb", ..., show = show)
}

#' @rdname geom_grid
#' @export
geom_grid_vappres <- function(..., show = TRUE) {
    psychro_grid_layer("vappres", ..., show = show)
}

#' @rdname geom_grid
#' @export
geom_grid_specvol <- function(..., show = TRUE) {
    psychro_grid_layer("specvol", ..., show = show)
}

#' @rdname geom_grid
#' @export
geom_grid_enthalpy <- function(..., show = TRUE) {
    psychro_grid_layer("enthalpy", ..., show = show)
}

psychro_grid_layer <- function(type, ..., show = TRUE) {
    assert_choice(type, names(default_psychro_grids()))
    assert_flag(show)

    structure(
        list(type = type, show = show, style = psychro_grid_style(...)),
        class = "PsyGrid"
    )
}

default_psychro_grids <- function() {
    list(
        relhum = TRUE,
        wetbulb = TRUE,
        vappres = TRUE,
        specvol = TRUE,
        enthalpy = TRUE
    )
}

merge_psychro_grids <- function(grids) {
    if (is.null(grids)) {
        grids <- list()
    }
    utils::modifyList(default_psychro_grids(), grids)
}

psychro_grid_enabled <- function(grids, type) {
    isTRUE(merge_psychro_grids(grids)[[type]])
}

psychro_grid_style <- function(...) {
    params <- list(...)
    if (!length(params)) return(list())

    if (!is.null(params$color) && is.null(params$colour)) {
        params$colour <- params$color
    }
    params$color <- NULL

    if (!is.null(params$size) && is.null(params$linewidth)) {
        params$linewidth <- params$size
    }
    params$size <- NULL

    alpha <- params$alpha
    params$alpha <- NULL
    if (!is.null(alpha) && !is.null(params$colour)) {
        params$colour <- grDevices::adjustcolor(params$colour, alpha.f = alpha)
    }

    keep <- c(
        "colour", "linewidth", "linetype", "lineend", "linejoin",
        "arrow", "arrow.fill", "inherit.blank"
    )
    params[names(params) %in% keep]
}

psychro_grid_theme <- function(type, style) {
    if (!length(style)) return(NULL)

    element <- do.call(ggplot2::element_line, style)
    stats::setNames(list(element), paste0("psychro.panel.grid.", type))
}
