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
#' @return A ggplot addition that controls rendering of a psychrometric grid.
#'
#' @rdname geom_grid
#' @examples
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_grid_relhum() +
#'     geom_grid_wetbulb() +
#'     geom_grid_enthalpy()
#'
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_grid_relhum(color = "black", linewidth = 0.6, label.size = 4) +
#'     scale_relhum_continuous(
#'         breaks = seq(25, 75, by = 25),
#'         minor_breaks = NULL
#'     ) +
#'     geom_grid_wetbulb(color = "black", label = FALSE) +
#'     scale_wetbulb_continuous(
#'         breaks = seq(10, 30, by = 10),
#'         minor_breaks = NULL
#'     ) +
#'     geom_grid_vappres(show = FALSE) +
#'     geom_grid_specvol(label_loc = 0.90) +
#'     geom_grid_enthalpy(label.size = 4)
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

#' Add a psychrometric protractor
#'
#' `geom_psychro_protractor()` adds a heat-moisture ratio and sensible heat
#' ratio reference protractor to the mask area of a [ggpsychro()] plot. The
#' orientation is determined by the parent chart: regular psychrometric charts
#' place the protractor in the upper-left mask area, while Mollier charts rotate
#' the same protractor geometry into the lower-right mask area.
#' `guide_psychro_protractor()` configures the protractor ticks and labels.
#'
#' @param ... Line style settings passed to [ggplot2::element_line()], such as
#'   `colour`, `color`, `linewidth`, `size`, and `linetype`. Label style
#'   settings can be supplied with `label.*` names, such as `label.colour`,
#'   `label.color`, `label.size`, `label.alpha`, `label.family`, and
#'   `label.fontface`.
#' @param show A single logical value. If `FALSE`, hide the protractor.
#' @param label A single logical value. If `FALSE`, hide protractor labels.
#' @param annotation A single logical value, character vector, or expression
#'   vector. If `FALSE`, hide the helper formula text. Character and expression
#'   vectors must have length 2 and are used as the helper formula labels.
#' @param scale A single positive number for overall protractor scaling. It
#'   multiplies `radius`, protractor line width, and label text size; `margin`
#'   is not scaled.
#' @param radius A single number in panel coordinates controlling the
#'   protractor radius.
#' @param margin A single number or length-2 numeric vector in panel
#'   coordinates controlling the distance from the mask-area edge. When length
#'   2, the values are horizontal and vertical margins, respectively.
#' @param guide A protractor guide created by [guide_psychro_protractor()].
#' @param shr_breaks,shr_minor_breaks Numeric vectors controlling the labelled
#'   and unlabelled ticks for the sensible heat ratio axis. Use `NULL` to hide
#'   that tick tier.
#' @param ratio_breaks,ratio_minor_breaks Numeric vectors controlling the
#'   labelled and unlabelled ticks for the heat-moisture-ratio axis
#'   (`dh/dW`). Use `waiver()` for the ASHRAE-style defaults, or `NULL` to hide
#'   that tick tier.
#' @param shr_labels,ratio_labels A character vector, expression vector,
#'   function, `NULL`, or `waiver()` controlling labels for the corresponding
#'   major breaks. `waiver()` uses the default numeric labels, and `NULL` hides
#'   the labels.
#' @param check_overlap A single logical value. If `TRUE`, overlapping
#'   protractor labels are dropped by the grid text grob.
#'
#' @return A ggplot addition.
#' @export
#'
#' @examples
#' # Draw the default protractor on a psychrometric chart.
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_psychro_protractor()
#'
#' # Draw the protractor in Mollier orientation.
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30), mollier = TRUE) +
#'     geom_psychro_protractor()
#'
#' # Customize major tick breaks with a guide.
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_psychro_protractor(
#'         guide = guide_psychro_protractor(
#'             shr_breaks = seq(0, 1, by = 0.25),
#'             ratio_breaks = c(0, 2000, 4000)
#'         )
#'     )
#'
#' # Hide the helper formula annotations.
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_psychro_protractor(annotation = FALSE)
#'
#' # Scale and move the protractor inside the mask area.
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_psychro_protractor(scale = 1.2, margin = c(0.03, 0.10))
#'
#' # Supply custom labels for selected sensible heat ratio ticks.
#' ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
#'     geom_psychro_protractor(
#'         guide = guide_psychro_protractor(
#'             shr_breaks = c(0, 0.5, 1),
#'             shr_labels = c("0", "half", "1")
#'         )
#'     )
geom_psychro_protractor <- function(..., show = TRUE, label = TRUE, annotation = TRUE,
                                    scale = 1, radius = 0.24, margin = 0.08,
                                    guide = guide_psychro_protractor()) {
    assert_flag(show)
    assert_flag(label)
    validate_psychro_protractor_annotation(annotation)
    assert_number(scale, lower = 0, .var.name = "scale")
    if (scale <= 0) {
        stop("`scale` must be a positive number.", call. = FALSE)
    }
    assert_number(radius, lower = 0.02, upper = 0.5)
    assert_numeric(
        margin, lower = 0, upper = 0.5, any.missing = FALSE,
        min.len = 1L, max.len = 2L, .var.name = "margin"
    )
    validate_psychro_protractor_guide(guide)

    structure(
        list(
            show = show,
            label = label,
            annotation = annotation,
            scale = scale,
            radius = radius,
            margin = margin,
            guide = guide,
            style = psychro_grid_style(...),
            label_style = psychro_grid_label_style(...)
        ),
        class = "PsyProtractor"
    )
}

#' @rdname geom_psychro_protractor
#' @export
guide_psychro_protractor <- function(shr_breaks = waiver(),
                                     shr_minor_breaks = waiver(),
                                     shr_labels = waiver(),
                                     ratio_breaks = waiver(),
                                     ratio_minor_breaks = waiver(),
                                     ratio_labels = waiver(),
                                     check_overlap = TRUE) {
    validate_psychro_protractor_breaks(shr_breaks, "shr_breaks")
    validate_psychro_protractor_breaks(shr_minor_breaks, "shr_minor_breaks")
    validate_psychro_protractor_breaks(ratio_breaks, "ratio_breaks")
    validate_psychro_protractor_breaks(ratio_minor_breaks, "ratio_minor_breaks")
    validate_psychro_protractor_labels(shr_labels, "shr_labels")
    validate_psychro_protractor_labels(ratio_labels, "ratio_labels")
    validate_psychro_protractor_break_labels(shr_breaks, shr_labels, "shr_breaks", "shr_labels")
    validate_psychro_protractor_break_labels(ratio_breaks, ratio_labels, "ratio_breaks", "ratio_labels")
    assert_flag(check_overlap)

    structure(
        list(
            shr_breaks = shr_breaks,
            shr_minor_breaks = shr_minor_breaks,
            shr_labels = shr_labels,
            ratio_breaks = ratio_breaks,
            ratio_minor_breaks = ratio_minor_breaks,
            ratio_labels = ratio_labels,
            check_overlap = check_overlap
        ),
        class = "PsyProtractorGuide"
    )
}

validate_psychro_protractor_guide <- function(guide) {
    if (inherits(guide, "PsyProtractorGuide")) {
        return(invisible(guide))
    }
    stop("`guide` must be created by `guide_psychro_protractor()`.", call. = FALSE)
}

validate_psychro_protractor_annotation <- function(annotation) {
    if (is.logical(annotation) && length(annotation) == 1L && !is.na(annotation)) {
        return(invisible(annotation))
    }
    if (is.character(annotation) && length(annotation) == 2L && !anyNA(annotation)) {
        return(invisible(annotation))
    }
    if (is.expression(annotation) && length(annotation) == 2L) {
        return(invisible(annotation))
    }
    stop(
        "`annotation` must be either a single `TRUE`/`FALSE` value, a character vector of length 2, or an expression vector of length 2.",
        call. = FALSE
    )
}

validate_psychro_protractor_breaks <- function(breaks, arg) {
    if (is.waive(breaks) || is.null(breaks)) {
        return(invisible(breaks))
    }
    assert_numeric(breaks, any.missing = FALSE, .var.name = arg)
}

validate_psychro_protractor_labels <- function(labels, arg) {
    if (is.waive(labels) || is.null(labels) || is.function(labels)) {
        return(invisible(labels))
    }
    if (is.character(labels) && !anyNA(labels)) {
        return(invisible(labels))
    }
    if (is.expression(labels)) {
        return(invisible(labels))
    }
    stop(
        sprintf("`%s` must be a character vector, expression vector, function, NULL, or waiver().", arg),
        call. = FALSE
    )
}

validate_psychro_protractor_break_labels <- function(breaks, labels, breaks_arg, labels_arg) {
    if (is.waive(labels) || is.null(labels) || is.function(labels)) {
        return(invisible(NULL))
    }
    if (is.waive(breaks)) {
        stop(
            sprintf("`%s` requires explicit `%s` unless it is a function, NULL, or waiver().", labels_arg, breaks_arg),
            call. = FALSE
        )
    }
    if (length(breaks) != length(labels)) {
        stop(
            sprintf("`%s` must have the same length as `%s`.", labels_arg, breaks_arg),
            call. = FALSE
        )
    }
    invisible(NULL)
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
