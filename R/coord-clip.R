#' @include coord-psychro.R
NULL

# Clip ordinary layer grobs to the saturated psychrometric panel.
coord_clip__grob_to_panel <- function(grob, coord, panel_params) {
    panel <- coord_psy__panel_grob(coord, panel_params)
    if (is.null(panel) || inherits(grob, c("nullGrob", "zeroGrob"))) {
        return(grob)
    }
    split <- coord_clip__split_styled_grob(grob)
    if (length(split) > 1L) {
        clipped <- lapply(split, function(child) {
            coord_clip__polyclip_grob(child, panel)
        })
        return(do.call(grid::grobTree, clipped))
    }
    coord_clip__polyclip_grob(grob, panel)
}

# Defer textpath clipping until makeContent() has generated stroke children.
coord_clip__textpath_to_panel <- function(grob, coord, panel_params) {
    panel <- coord_psy__panel_grob(coord, panel_params)
    if (is.null(panel) || inherits(grob, c("nullGrob", "zeroGrob"))) {
        return(grob)
    }
    grob$psychro_panel <- panel
    class(grob) <- c("psychro_textpath_clip", class(grob))
    grob
}

#' @method makeContent psychro_textpath_clip
#' @importFrom grid makeContent
#' @export
makeContent.psychro_textpath_clip <- function(x) {
    panel <- x$psychro_panel
    x$psychro_panel <- NULL
    class(x) <- setdiff(class(x), "psychro_textpath_clip")

    # Path labels must lay out in their normal grid drawing context. Clipping
    # the input data, or forcing the grob from a wrapper, can flip contour
    # labels; therefore we expand first and only clip stroke children.
    x <- grid::makeContent(x)
    class(x) <- setdiff(class(x), "textpath")
    if (is.null(panel)) {
        return(x)
    }
    coord_clip__textpath_lines(x, panel, open_lines = TRUE)
}

# Walk expanded textpath grobs and clip only their line children.
coord_clip__textpath_lines <- function(grob, panel, open_lines = FALSE) {
    if (inherits(grob, c("textpath", "psychro_textpath"))) {
        # The wrapper is expanded before its children, so force internal
        # textpath grobs here; otherwise their stroke child is created only
        # after the clipping pass has already returned.
        grob <- grid::makeContent(grob)
        class(grob) <- setdiff(class(grob), c("textpath", "psychro_textpath"))
    }
    if (coord_clip__is_line_grob(grob)) {
        split <- coord_clip__split_styled_grob(grob)
        clipped <- lapply(
            split, coord_clip__polyclip_grob, panel = panel,
            open_lines = open_lines
        )
        if (length(clipped) == 1L) {
            return(clipped[[1L]])
        }
        return(do.call(grid::grobTree, clipped))
    }
    if (!is.null(grob$children)) {
        children <- as.list(grob$children)
        children <- lapply(
            children, coord_clip__textpath_lines, panel = panel,
            open_lines = open_lines
        )
        grob$children <- do.call(grid::gList, children)
        grob$childrenOrder <- names(children)
    }
    if (!is.null(grob$grobs)) {
        grob$grobs <- lapply(
            grob$grobs, coord_clip__textpath_lines, panel = panel,
            open_lines = open_lines
        )
    }
    grob
}

# Dispatch gridGeometry clipping while preserving open-line textpath strokes.
coord_clip__polyclip_grob <- function(grob, panel, open_lines = FALSE) {
    if (coord_clip__is_line_grob(grob)) {
        if (isTRUE(open_lines)) {
            clipped <- coord_clip__open_line(grob, panel)
            if (!is.null(clipped)) {
                return(clipped)
            }
        }
        return(gridGeometry::polyclipGrob(
            grob, panel, "intersection",
            closedFn = coord_clip__xy_list_to_null,
            name = grob$name,
            gp = grob$gp %||% grid::gpar()
        ))
    }
    gridGeometry::polyclipGrob(grob, panel, "intersection", name = grob$name)
}

# Clip open line paths numerically so contour textpath strokes stop at the
# psychrometric panel boundary without polyclip adding closed boundary edges.
coord_clip__open_line <- function(grob, panel) {
    if (!inherits(grob, c("polyline", "lines"))) {
        return(NULL)
    }

    id <- coord_clip__grob_ids(grob)
    x <- grid::convertX(grob$x, "in", valueOnly = TRUE)
    y <- grid::convertY(grob$y, "in", valueOnly = TRUE)
    if (is.null(id)) {
        id <- rep.int(1L, length(x))
    }
    panel_path <- coord_clip__panel_path_inches(panel, grob)

    pieces <- split(seq_along(x), id)
    segments <- vector("list", length(pieces))
    out_group <- 0L
    for (piece in pieces) {
        line <- list(list(x = x[piece], y = y[piece]))
        if (length(line[[1L]]$x) < 2L) {
            next
        }
        # closed = FALSE is the important part: these are stroked contour
        # segments, not filled polygons, so boundary connector edges are invalid.
        clipped <- polyclip::polyclip(
            line, panel_path, op = "intersection", closed = FALSE
        )
        for (segment in clipped) {
            if (length(segment$x) < 2L || length(segment$y) < 2L) {
                next
            }
            out_group <- out_group + 1L
            # Store fragments and flatten once after clipping. Repeated c()
            # growth is avoidable here and can dominate dense contour output.
            segments[[out_group]] <- segment
        }
    }
    if (!out_group) {
        return(NULL)
    }
    segments <- segments[seq_len(out_group)]
    segment_lengths <- vapply(
        segments, function(segment) length(segment$x), integer(1L)
    )

    grid::polylineGrob(
        x = grid::unit(
            unlist(lapply(segments, `[[`, "x"), use.names = FALSE), "in"
        ),
        y = grid::unit(
            unlist(lapply(segments, `[[`, "y"), use.names = FALSE), "in"
        ),
        id = rep.int(seq_along(segments), segment_lengths),
        arrow = grob$arrow,
        name = grob$name,
        gp = grob$gp %||% grid::gpar()
    )
}

# Map the stored npc panel polygon into the panel-local inch coordinates that
# the textpath renderer used for its stroke child.
coord_clip__panel_path_inches <- function(panel, grob) {
    width <- attr(grob, "psychro_panel_width_in", exact = TRUE)
    height <- attr(grob, "psychro_panel_height_in", exact = TRUE)
    if (length(width) && length(height) &&
            is.finite(width) && is.finite(height) &&
            width > 0 && height > 0) {
        return(list(list(
            x = grid::convertX(panel$x, "npc", valueOnly = TRUE) * width,
            y = grid::convertY(panel$y, "npc", valueOnly = TRUE) * height
        )))
    }

    list(list(
        x = grid::convertX(panel$x, "in", valueOnly = TRUE),
        y = grid::convertY(panel$y, "in", valueOnly = TRUE)
    ))
}

# Identify stroked grobs that can be line-clipped against the panel polygon.
coord_clip__is_line_grob <- function(grob) {
    inherits(grob, c("polyline", "segments", "lines"))
}

# Drop closed polygon output when line clipping should keep only stroke pieces.
coord_clip__xy_list_to_null <- function(...) {
    grid::nullGrob()
}

# Split multi-style grobs so polyclip can preserve per-piece graphical params.
coord_clip__split_styled_grob <- function(grob) {
    if (inherits(grob, "pathgrob")) {
        return(coord_clip__split_path_grob(grob))
    }
    if (inherits(grob, "polyline")) {
        return(coord_clip__split_polyline_grob(grob))
    }
    if (inherits(grob, "polygon")) {
        return(coord_clip__split_polygon_grob(grob))
    }
    list(grob)
}

# Split compound path grobs by path id while keeping matching style slices.
coord_clip__split_path_grob <- function(grob) {
    path_id <- grob$pathId %||% grob$id
    if (is.null(path_id)) {
        return(list(grob))
    }
    path_ids <- unique(path_id)
    n <- length(path_ids)
    if (n <= 1L) {
        return(list(grob))
    }

    lapply(seq_along(path_ids), function(i) {
        keep <- path_id == path_ids[[i]]
        id <- grob$id[keep]
        id <- match(id, unique(id))
        grid::pathGrob(
            grob$x[keep], grob$y[keep],
            id = id,
            pathId = rep(1L, sum(keep)),
            rule = grob$rule %||% "winding",
            name = paste0(grob$name %||% "path", "-", i),
            gp = coord_clip__gpar_slice(grob$gp, i, n)
        )
    })
}

# Split multi-id polylines before clipping so each line keeps its own style.
coord_clip__split_polyline_grob <- function(grob) {
    id <- coord_clip__grob_ids(grob)
    if (is.null(id)) {
        return(list(grob))
    }
    ids <- unique(id)
    n <- length(ids)
    if (n <= 1L) {
        return(list(grob))
    }

    lapply(seq_along(ids), function(i) {
        keep <- id == ids[[i]]
        child <- grid::polylineGrob(
            grob$x[keep], grob$y[keep],
            id = rep(1L, sum(keep)),
            arrow = grob$arrow,
            name = paste0(grob$name %||% "polyline", "-", i),
            gp = coord_clip__gpar_slice(grob$gp, i, n)
        )
        coord_clip__copy_panel_size(child, grob)
    })
}

# Preserve textpath panel dimensions across styled polyline splitting so later
# open-line clipping still uses the correct panel-local coordinate frame.
coord_clip__copy_panel_size <- function(child, parent) {
    for (name in c("psychro_panel_width_in", "psychro_panel_height_in")) {
        value <- attr(parent, name, exact = TRUE)
        if (!is.null(value)) {
            attr(child, name) <- value
        }
    }
    child
}

# Split multi-id polygons before clipping so fills and outlines stay aligned.
coord_clip__split_polygon_grob <- function(grob) {
    id <- coord_clip__grob_ids(grob)
    if (is.null(id)) {
        return(list(grob))
    }
    ids <- unique(id)
    n <- length(ids)
    if (n <= 1L) {
        return(list(grob))
    }

    lapply(seq_along(ids), function(i) {
        keep <- id == ids[[i]]
        grid::polygonGrob(
            grob$x[keep], grob$y[keep],
            id = rep(1L, sum(keep)),
            name = paste0(grob$name %||% "polygon", "-", i),
            gp = coord_clip__gpar_slice(grob$gp, i, n)
        )
    })
}

# Recover per-vertex ids from grid grobs using either id or id.lengths.
coord_clip__grob_ids <- function(grob) {
    if (!is.null(grob$id)) {
        return(grob$id)
    }
    if (!is.null(grob$id.lengths)) {
        return(rep(seq_along(grob$id.lengths), grob$id.lengths))
    }
    NULL
}

# Slice vectorized graphical parameters for the i-th split child grob.
coord_clip__gpar_slice <- function(gp, i, n) {
    if (is.null(gp)) {
        return(grid::gpar())
    }
    args <- lapply(as.list(gp), function(value) {
        if (length(value) == n) value[[i]] else value
    })
    do.call(grid::gpar, args)
}

# Filter point-like data using the normalized panel polygon after coord transform.
coord_clip__filter_data_to_panel <- function(data, panel_params, coord) {
    if (!nrow(data) || !all(c("x", "y") %in% names(data))) {
        return(data)
    }
    panel <- coord_psy__panel_polygon_npc(coord, panel_params)
    if (is.null(panel)) {
        return(data)
    }
    transformed <- coord$transform(data, panel_params)
    keep <- psychro_inside_polygon(transformed$x, transformed$y, panel$x, panel$y)
    data[keep, , drop = FALSE]
}

# Clip polygon data in scaled coordinate space before ggplot builds the grob.
coord_clip__polygon_data_to_panel <- function(data, panel_params, coord) {
    if (!nrow(data) || !all(c("x", "y", "group") %in% names(data))) {
        return(data)
    }
    panel <- coord_psy__panel_polygon_scaled(coord, panel_params)
    if (is.null(panel)) {
        return(data)
    }

    group <- data$group
    if ("subgroup" %in% names(data)) {
        group <- interaction(group, data$subgroup, drop = TRUE, lex.order = TRUE)
    }
    pieces <- split(data, group)

    out <- list()
    group_id <- 0L
    for (piece in pieces) {
        if (nrow(piece) < 3L) {
            next
        }
        clipped <- polyclip::polyclip(
            list(list(x = piece$x, y = piece$y)),
            list(panel),
            op = "intersection",
            fillA = "evenodd",
            fillB = "nonzero",
            closed = TRUE
        )
        if (!length(clipped)) {
            next
        }
        for (poly in clipped) {
            if (length(poly$x) < 3L || length(poly$y) < 3L) {
                next
            }
            group_id <- group_id + 1L
            clipped_piece <- piece[rep(1L, length(poly$x)), , drop = FALSE]
            clipped_piece$x <- poly$x
            clipped_piece$y <- poly$y
            clipped_piece$group <- group_id
            if ("subgroup" %in% names(clipped_piece)) {
                clipped_piece$subgroup <- 1L
            }
            out[[length(out) + 1L]] <- clipped_piece
        }
    }

    if (!length(out)) {
        return(data[0L, , drop = FALSE])
    }
    do.call(rbind, out)
}
