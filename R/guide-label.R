# Render curved psychrometric grid labels along clipped guide paths.
# Build a textpath grob for one psychrometric grid label family.
guide_label__grob <- function(
    grid,
    label,
    type,
    theme,
    mollier,
    panel_x,
    panel_y
) {
    if (is.null(grid) || is.null(label) || !isTRUE(label$show)) {
        return(NULL)
    }

    data <- guide_label__data(
        grid,
        label$labels,
        mollier,
        panel_x,
        panel_y
    )
    if (!nrow(data$path)) {
        return(NULL)
    }

    style <- guide_label__style_defaults(type, theme, label$style)
    labels <- guide_label__normalise(data$labels, label$label_parse)
    if (!length(labels)) {
        return(NULL)
    }

    colour <- util__apply_alpha(style$colour, style$alpha)
    gp_text <- grid::gpar(
        col = colour,
        fontsize = style$size * ggplot2::.pt,
        fontfamily = style$family,
        fontface = style$fontface,
        lineheight = style$lineheight
    )

    # Grid labels use the internal textpath renderer so CRAN examples avoid the
    # old external renderer's cold glyph-index cost.
    textpath__grob(
        label = labels,
        x = data$path$x,
        y = data$path$y,
        id = data$path$id,
        hjust = rep(label$label_loc, length(labels)),
        vjust = rep(style$vjust, length(labels)),
        upright = style$upright,
        straight = style$straight,
        remove_long = style$remove_long,
        gp_text = gp_text,
        default.units = "npc",
        name = paste0("psychro-grid-label-", type)
    )
}

# Keep only label paths that have enough visible points inside the panel.
guide_label__data <- function(grid, labels, mollier, panel_x, panel_y) {
    x <- grid[[c("tdb", "hum")[c(!mollier, mollier)]]]
    y <- grid[[c("tdb", "hum")[c(mollier, !mollier)]]]
    id <- grid$group

    label_missing <- guide_label__missing(labels)
    keep_group <- seq_along(labels)[!label_missing]
    inside <- util__inside_polygon(x, y, panel_x, panel_y) &
        id %in% keep_group

    pieces <- lapply(keep_group, function(group) {
        idx <- which(inside & id == group)
        if (length(idx) < 2L) {
            return(NULL)
        }
        util__new_data_frame(list(
            x = x[idx],
            y = y[idx],
            id = rep(group, length(idx))
        ))
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (!length(pieces)) {
        return(list(path = util__new_data_frame(list()), labels = labels[0]))
    }

    path <- do.call(rbind, pieces)
    used <- unique(path$id)
    path$id <- match(path$id, used)

    list(path = path, labels = labels[used])
}

# Identify labels that should not produce a rendered textpath.
guide_label__missing <- function(labels) {
    if (is.null(labels)) {
        return(TRUE)
    }
    if (is.atomic(labels)) {
        return(is.na(labels))
    }

    vapply(
        labels,
        function(label) {
            length(label) == 0L || anyNA(as.character(label))
        },
        logical(1)
    )
}

# Normalize character, expression, and list labels into a textpath-ready form.
guide_label__normalise <- function(labels, parse = FALSE) {
    if (is.null(labels)) {
        return(labels)
    }
    if (is.expression(labels)) {
        return(labels)
    }
    if (is.list(labels) && !is.data.frame(labels)) {
        return(as.expression(labels))
    }
    if (!parse) {
        return(labels)
    }

    as.expression(lapply(as.character(labels), function(label) {
        parsed <- parse(text = label)
        if (length(parsed)) parsed[[1L]] else NA
    }))
}

# Resolve grid-label style defaults from the matching major grid theme element.
guide_label__style_defaults <- function(type, theme, style = list()) {
    line <- ggplot2::calc_element(
        paste("psychro.panel.grid.major", type, sep = "."),
        theme
    )
    defaults <- list(
        colour = line$colour %||% "black",
        size = 3.2,
        alpha = NA_real_,
        family = "",
        fontface = 1,
        lineheight = 1.2,
        vjust = -0.3,
        gap = NA,
        upright = TRUE,
        straight = FALSE,
        padding = grid::unit(0.05, "inch"),
        remove_long = FALSE
    )

    utils::modifyList(defaults, style)
}
