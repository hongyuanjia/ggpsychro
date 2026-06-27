#' Add psychrometric grid lines
#'
#' These helpers provide ggplot-style controls for psychrometric reference
#' grids. They do not add data layers; they mark a grid as visible and let
#' [coord_psychro()] render it in the panel background.
#'
#' @param ... Line style settings passed to [ggplot2::element_line()], such as
#'   `colour`, `color`, `linewidth`, `size`, and `linetype`. Label style
#'   settings can be supplied with `label.*` names to keep them separate from
#'   line style settings, such as
#'   `label.colour`, `label.color`, `label.size`, `label.alpha`,
#'   `label.family`, `label.fontface`, `label.lineheight`, and `label.vjust`.
#' @param show A single logical value. If `FALSE`, hide the corresponding grid.
#' @param label A single logical value. If `FALSE`, hide labels for the
#'   corresponding major grid lines.
#' @param label_loc A single number in range `[0, 1]` indicating the label
#'   position along each grid line. `NA` hides labels.
#' @param label_parse If `TRUE`, labels are parsed as plotmath expressions.
#'
#' @rdname geom_grid
#' @export
geom_grid_relhum <- function(..., show = TRUE, label = TRUE, label_loc = 0.95,
                             label_parse = FALSE) {
    psychro_grid_layer("relhum", ..., show = show, label = label,
        label_loc = label_loc, label_parse = label_parse)
}

#' @rdname geom_grid
#' @export
geom_grid_wetbulb <- function(..., show = TRUE, label = TRUE, label_loc = 0.10,
                              label_parse = TRUE) {
    psychro_grid_layer("wetbulb", ..., show = show, label = label,
        label_loc = label_loc, label_parse = label_parse)
}

#' @rdname geom_grid
#' @export
geom_grid_vappres <- function(..., show = TRUE, label = TRUE, label_loc = 0.50,
                              label_parse = FALSE) {
    psychro_grid_layer("vappres", ..., show = show, label = label,
        label_loc = label_loc, label_parse = label_parse)
}

#' @rdname geom_grid
#' @export
geom_grid_specvol <- function(..., show = TRUE, label = TRUE, label_loc = 0.95,
                              label_parse = TRUE) {
    psychro_grid_layer("specvol", ..., show = show, label = label,
        label_loc = label_loc, label_parse = label_parse)
}

#' @rdname geom_grid
#' @export
geom_grid_enthalpy <- function(..., show = TRUE, label = TRUE, label_loc = 0.95,
                               label_parse = TRUE) {
    psychro_grid_layer("enthalpy", ..., show = show, label = label,
        label_loc = label_loc, label_parse = label_parse)
}

psychro_grid_layer <- function(type, ..., show = TRUE, label = TRUE,
                               label_loc = NULL, label_parse = FALSE) {
    assert_choice(type, names(default_psychro_grids()))
    assert_flag(show)
    assert_flag(label)
    if (!is.null(label_loc)) {
        assert_number(label_loc, lower = 0.0, upper = 1.0, na.ok = TRUE)
    }
    assert_flag(label_parse)

    structure(
        list(
            type = type, show = show, style = psychro_grid_style(...),
            label = psychro_grid_label(label, label_loc, label_parse, ...)
        ),
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

psychro_grid_label <- function(label, label_loc, label_parse, ...) {
    style <- psychro_grid_label_style(...)
    show <- isTRUE(label) && !is.null(label_loc) && !is.na(label_loc)

    list(
        show = show,
        label = label,
        label_loc = label_loc,
        label_parse = label_parse,
        style = style
    )
}

psychro_grid_label_enabled <- function(labels, type) {
    label <- labels[[type]]
    is.list(label) && isTRUE(label$show)
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

psychro_grid_label_style <- function(...) {
    params <- list(...)
    if (!length(params)) return(list())

    if (!is.null(params$label.color) && is.null(params$label.colour)) {
        params$label.colour <- params$label.color
    }

    # Convert user-facing label.* arguments to the style names used by the
    # textpath rendering helper.
    label_style_names <- c(
        label.colour = "colour",
        label.size = "size",
        label.alpha = "alpha",
        label.family = "family",
        label.fontface = "fontface",
        label.lineheight = "lineheight",
        label.vjust = "vjust",
        label.halign = "halign",
        label.gap = "gap",
        label.upright = "upright",
        label.straight = "straight",
        label.padding = "padding",
        label.text_smoothing = "text_smoothing",
        label.remove_long = "remove_long"
    )

    keep <- intersect(names(label_style_names), names(params))
    stats::setNames(params[keep], unname(label_style_names[keep]))
}

psychro_grid_theme <- function(type, style) {
    if (!length(style)) return(NULL)

    element <- do.call(ggplot2::element_line, style)
    stats::setNames(list(element), paste0("psychro.panel.grid.", type))
}
