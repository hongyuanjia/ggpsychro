# Internal textpath grobs defer layout until grid knows the final panel viewport,
# so label angles are computed in device inches rather than raw npc coordinates.
psychro_textpath_grob <- function(
    label,
    x,
    y,
    id,
    hjust = 0.5,
    vjust = 0.5,
    upright = TRUE,
    straight = FALSE,
    remove_long = FALSE,
    gp_text,
    gp_path = NULL,
    text_only = TRUE,
    gap = FALSE,
    keep_path_side = FALSE,
    padding = grid::unit(1, "pt"),
    default.units = "npc",
    name = NULL
) {
    if (!psychro_textpath_supported(label, vjust)) {
        return(NULL)
    }

    grid::gTree(
        psychro_textpath = list(
            label = label,
            x = grid::unit(x, default.units),
            y = grid::unit(y, default.units),
            id = as.integer(id),
            hjust = hjust,
            vjust = vjust,
            upright = isTRUE(upright),
            straight = isTRUE(straight),
            remove_long = isTRUE(remove_long),
            gp_text = gp_text,
            gp_path = gp_path,
            text_only = isTRUE(text_only),
            gap = isTRUE(gap),
            keep_path_side = isTRUE(keep_path_side),
            padding = padding
        ),
        name = name,
        cl = "psychro_textpath"
    )
}

# The internal renderer deliberately accepts only labels that grid can measure
# directly, because ggpsychro now owns all textpath drawing.
psychro_textpath_supported <- function(label, vjust) {
    if (!(is.character(label) || is.expression(label))) {
        return(FALSE)
    }
    if (!length(label) || anyNA(as.character(label))) {
        return(FALSE)
    }
    if (grid::is.unit(vjust)) {
        return(length(vjust) > 0L)
    }
    !anyNA(vjust)
}

#' @method makeContent psychro_textpath
#' @importFrom grid makeContent
#' @export
makeContent.psychro_textpath <- function(x) {
    spec <- x$psychro_textpath
    x$psychro_textpath <- NULL
    class(x) <- setdiff(class(x), "psychro_textpath")

    measured <- textpath_measure(
        spec$label,
        spec$gp_text,
        spec$vjust,
        spec$straight
    )
    if (is.null(measured) || !length(measured$piece_label)) {
        return(grid::nullGrob())
    }

    # Unit conversion must happen inside makeContent(), where grid has already
    # pushed the panel viewport used by the psychrometric guide or layer grob.
    path <- new_data_frame(list(
        x = grid::convertX(spec$x, "in", valueOnly = TRUE),
        y = grid::convertY(spec$y, "in", valueOnly = TRUE),
        id = spec$id
    ))

    placed <- textpath_place(
        path,
        measured,
        spec$hjust,
        spec$upright,
        spec$remove_long,
        spec$keep_path_side
    )
    if (!nrow(placed)) {
        return(grid::nullGrob())
    }

    children <- list()
    if (!is.null(spec$gp_path) && !isTRUE(spec$text_only)) {
        # The path child is built from the same placement result so the visual
        # gap, when requested, matches the measured label span.
        path_grob <- textpath_path_grob(
            path,
            placed,
            spec$gp_path,
            spec$gap,
            spec$padding,
            x$name
        )
        if (!is.null(path_grob)) {
            children[[length(children) + 1L]] <- path_grob
        }
    }

    labels <- measured$piece_label[placed$piece]
    text <- grid::textGrob(
        label = labels,
        x = grid::unit(placed$x, "in"),
        y = grid::unit(placed$y, "in"),
        rot = placed$angle,
        hjust = 0.5,
        vjust = 0.5,
        gp = spec$gp_text,
        name = paste0(x$name %||% "psychro-textpath", "-text")
    )
    children[[length(children) + 1L]] <- text
    grid::setChildren(x, do.call(grid::gList, children))
}

# Internal ggplot2 geom used by comfort layers that previously relied on
# the previous external path-label geom. It only implements the feature subset
# produced by ggpsychro's own stats and layer wrappers.
GeomPsychroTextpath <- ggplot2::ggproto(
    "GeomPsychroTextpath",
    ggplot2::Geom,
    required_aes = c("x", "y", "label"),
    default_aes = ggplot2::aes(
        colour = "black",
        alpha = NA,
        linewidth = 0.5,
        linetype = 1,
        size = 3.88,
        family = "",
        fontface = 1,
        lineheight = 1.2,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0
    ),
    draw_key = ggplot2::draw_key_path,

    draw_panel = function(
        data,
        panel_params,
        coord,
        na.rm = FALSE,
        text_only = FALSE,
        upright = TRUE,
        straight = FALSE,
        remove_long = FALSE,
        gap = FALSE,
        padding = grid::unit(1, "pt"),
        keep_path_side = FALSE,
        label_path = TRUE
    ) {
        if (!nrow(data)) {
            return(grid::nullGrob())
        }

        keep <- is.finite(data$x) & is.finite(data$y) & !is.na(data$label)
        if (!all(keep)) {
            if (!isTRUE(na.rm)) {
                warning(
                    "Removed missing values from psychro textpath layer.",
                    call. = FALSE
                )
            }
            data <- data[keep, , drop = FALSE]
        }
        if (!nrow(data)) {
            return(grid::nullGrob())
        }

        data <- coord$transform(data, panel_params)
        groups <- split(data, data$group, drop = TRUE)
        grobs <- lapply(
            groups,
            textpath_group_grob,
            text_only = text_only,
            upright = upright,
            straight = straight,
            remove_long = remove_long,
            gap = gap,
            padding = padding,
            keep_path_side = keep_path_side
        )
        grobs <- grobs[!vapply(grobs, is.null, logical(1L))]
        if (!length(grobs)) {
            return(grid::nullGrob())
        }
        grid::gTree(children = do.call(grid::gList, grobs))
    }
)

# Build one textpath grob per data group so per-contour colours, line
# widths, and labels remain independent without vectorising grid parameters.
textpath_group_grob <- function(
    data,
    text_only,
    upright,
    straight,
    remove_long,
    gap,
    padding,
    keep_path_side
) {
    label <- first_value(data$label, NA_character_)
    if (is.na(label)) {
        return(NULL)
    }

    alpha <- first_value(data$alpha, NA_real_)
    colour <- apply_alpha(first_value(data$colour, "black"), alpha)
    linewidth <- first_value(data$linewidth, 0.5)
    gp_text <- grid::gpar(
        col = colour,
        fontsize = first_value(data$size, 3.88) * ggplot2::.pt,
        fontfamily = first_value(data$family, ""),
        fontface = first_value(data$fontface, 1),
        lineheight = first_value(data$lineheight, 1.2)
    )
    gp_path <- if (isTRUE(text_only)) {
        NULL
    } else {
        grid::gpar(
            col = colour,
            lwd = linewidth * ggplot2::.pt,
            lty = first_value(data$linetype, 1),
            lineend = "butt"
        )
    }

    psychro_textpath_grob(
        label = as.character(label),
        x = data$x,
        y = data$y,
        id = rep.int(1L, nrow(data)),
        hjust = first_value(data$hjust, 0.5),
        vjust = first_value(data$vjust, 0.5),
        upright = upright,
        straight = straight,
        remove_long = remove_long,
        gp_text = gp_text,
        gp_path = gp_path,
        text_only = text_only,
        gap = gap,
        keep_path_side = keep_path_side,
        padding = padding
    )
}

# Measure labels using grid's normal text metrics. Character labels are split
# into drawable pieces, while plotmath labels stay as whole expressions.
textpath_measure <- function(label, gp, vjust, straight = FALSE) {
    if (is.expression(label)) {
        return(textpath_measure_expr(label, gp, vjust))
    }
    textpath_measure_chr(label, gp, vjust, straight)
}

# Plotmath expressions cannot be split into glyphs without reimplementing R's
# expression renderer, so they are laid out as one rotated label per path.
textpath_measure_expr <- function(label, gp, vjust) {
    width <- text_width_in(label, gp)
    height <- text_lineheight_in(label, gp)
    n <- length(label)

    list(
        piece_label = label,
        piece_id = seq_len(n),
        piece_mid = width / 2,
        piece_width = width,
        label_width = width,
        label_offset = textpath_offset(vjust, height, n),
        n_label = n
    )
}

# Character labels use prefix widths rather than isolated glyph widths. This
# keeps device kerning in the advances while avoiding a full Unicode glyph-index
# cache.
textpath_measure_chr <- function(label, gp, vjust, straight = FALSE) {
    label_height <- text_lineheight_in(label, gp)
    label_offset <- textpath_offset(
        vjust,
        label_height,
        length(label)
    )

    pieces <- lapply(seq_along(label), function(i) {
        text <- label[[i]]
        # Multiline labels must stay as one grid text grob so embedded line
        # breaks remain visible after rotation.
        if (isTRUE(straight) || grepl("\n", text, fixed = TRUE)) {
            width <- text_width_in(text, gp)
            return(list(
                label = text,
                id = i,
                mid = width / 2,
                width = width,
                total = width,
                offset = label_offset[[i]]
            ))
        }

        chars <- strsplit(text, "", fixed = FALSE, useBytes = FALSE)[[1L]]
        if (!length(chars)) {
            return(NULL)
        }

        prefixes <- vapply(
            seq_along(chars),
            function(j) {
                paste0(chars[seq_len(j)], collapse = "")
            },
            character(1L)
        )
        prefix_width <- text_width_in(prefixes, gp)
        char_width <- pmax(diff(c(0, prefix_width)), 0)

        list(
            label = chars,
            id = rep.int(i, length(chars)),
            mid = cumsum(char_width) - char_width / 2,
            width = char_width,
            total = prefix_width[[length(prefix_width)]],
            offset = label_offset[[i]]
        )
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1L))]
    if (!length(pieces)) {
        return(NULL)
    }

    label_width <- rep(NA_real_, length(label))
    piece_offset <- rep(NA_real_, length(label))
    for (piece in pieces) {
        # Each piece record belongs to one original label. Cache those
        # per-label metrics once instead of scanning all pieces repeatedly.
        i <- piece$id[[1L]]
        label_width[[i]] <- piece$total
        piece_offset[[i]] <- piece$offset
    }

    list(
        piece_label = unlist(lapply(pieces, `[[`, "label"), use.names = FALSE),
        piece_id = unlist(lapply(pieces, `[[`, "id"), use.names = FALSE),
        piece_mid = unlist(lapply(pieces, `[[`, "mid"), use.names = FALSE),
        piece_width = unlist(lapply(pieces, `[[`, "width"), use.names = FALSE),
        label_width = label_width,
        label_offset = piece_offset,
        n_label = length(label)
    )
}

# Numeric vjust follows text hjust/vjust semantics; unit vjust is treated as an
# absolute normal offset, matching the way ggpsychro uses point offsets for PMV
# axis labels.
textpath_offset <- function(vjust, height, n) {
    if (grid::is.unit(vjust)) {
        idx <- rep(seq_along(vjust), length.out = n)
        unit_offset <- grid::convertHeight(vjust[idx], "in", valueOnly = TRUE)
        return(unit_offset + 0.5 * height)
    }
    vjust <- rep_len(as.numeric(vjust), n)
    (0.5 - vjust) * height
}

# Widths are measured through grid so the fast renderer follows the active
# graphics device and font family without asking systemfonts for all glyphs.
text_width_in <- function(label, gp) {
    vapply(
        seq_along(label),
        function(i) {
            grid::convertWidth(
                grid::grobWidth(grid::textGrob(label = label[i], gp = gp)),
                "in",
                valueOnly = TRUE
            )
        },
        numeric(1L)
    )
}

# Heights determine the normal offset produced by numeric label.vjust.
text_height_in <- function(label, gp) {
    vapply(
        seq_along(label),
        function(i) {
            grid::convertHeight(
                grid::grobHeight(grid::textGrob(label = label[i], gp = gp)),
                "in",
                valueOnly = TRUE
            )
        },
        numeric(1L)
    )
}

# Offset height uses the nominal font line height rather than only the ink
# bounding box. That matches path-label spacing expectations for large numeric
# vjust values used by comfort standards and Givoni labels.
text_lineheight_in <- function(label, gp) {
    actual <- text_height_in(label, gp)
    fontsize <- rep_len(gp$fontsize %||% 12, length(label))
    lineheight <- rep_len(gp$lineheight %||% 1.2, length(label))
    n_lines <- if (is.character(label)) {
        lengths(regmatches(label, gregexpr("\n", label, fixed = TRUE))) + 1L
    } else {
        rep.int(1L, length(label))
    }
    nominal <- grid::convertHeight(
        grid::unit(fontsize * lineheight * n_lines, "pt"),
        "in",
        valueOnly = TRUE
    )
    pmax(actual, nominal)
}

# C handles the repeated arclength interpolation and tangent-angle calculation
# after R has converted paths and text metrics to inches.
textpath_place <- function(
    path,
    measured,
    hjust,
    upright,
    remove_long,
    keep_path_side = FALSE
) {
    if (!nrow(path) || !length(measured$piece_id)) {
        return(new_data_frame(list(
            x = numeric(),
            y = numeric(),
            angle = numeric(),
            piece = integer(),
            label = integer(),
            left = numeric(),
            right = numeric()
        )))
    }

    placed <- .Call(
        C_psychro_textpath_place,
        as.numeric(path$x),
        as.numeric(path$y),
        as.integer(path$id),
        as.integer(measured$piece_id),
        as.numeric(measured$piece_mid),
        as.numeric(measured$piece_width),
        as.numeric(measured$label_width),
        as.numeric(measured$label_offset),
        as.numeric(rep_len(hjust, measured$n_label)),
        isTRUE(upright),
        isTRUE(remove_long),
        isTRUE(keep_path_side)
    )
    new_data_frame(placed)
}

# Draw the underlying path, optionally deleting segments under label spans when
# a comfort contour requests text gap behaviour.
textpath_path_grob <- function(
    path,
    placed,
    gp_path,
    gap,
    padding,
    name = NULL
) {
    if (isTRUE(gap) && nrow(placed)) {
        path <- textpath_gap_path(path, placed, padding)
    }
    if (!nrow(path)) {
        return(NULL)
    }

    path_grob <- grid::polylineGrob(
        x = grid::unit(path$x, "in"),
        y = grid::unit(path$y, "in"),
        id = path$id,
        gp = gp_path,
        name = paste0(name %||% "psychro-textpath", "-path")
    )
    # The clipping wrapper runs after this grob is expanded. Keep the panel
    # dimensions that were active during placement so its npc polygon can be
    # mapped back into the same panel-local inch coordinate system.
    attr(path_grob, "psychro_panel_width_in") <- grid::convertWidth(
        grid::unit(1, "npc"),
        "in",
        valueOnly = TRUE
    )
    attr(path_grob, "psychro_panel_height_in") <- grid::convertHeight(
        grid::unit(1, "npc"),
        "in",
        valueOnly = TRUE
    )
    path_grob
}

# Gap clipping cuts paths at measured label-span boundaries. Keeping each
# remaining continuous run under one id preserves dashed line patterns and
# avoids text-over-line collisions on sparse contour paths.
textpath_gap_path <- function(path, placed, padding) {
    pad <- if (grid::is.unit(padding)) {
        grid::convertWidth(padding, "in", valueOnly = TRUE)
    } else {
        as.numeric(padding)
    }
    pad <- rep_len(pad %||% 0, 1L)

    bounds <- unique(placed[c("label", "left", "right")])
    bounds_by_label <- split(bounds, bounds$label, drop = TRUE)
    runs <- list()

    path_groups <- split(seq_len(nrow(path)), path$id, drop = TRUE)
    for (path_id in names(path_groups)) {
        idx <- path_groups[[path_id]]
        if (length(idx) < 2L) {
            next
        }

        x <- path$x[idx]
        y <- path$y[idx]
        finite <- is.finite(x) & is.finite(y)
        x <- x[finite]
        y <- y[finite]
        if (length(x) < 2L) {
            next
        }

        arc <- c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
        label_bounds <- bounds_by_label[[path_id]]
        if (is.null(label_bounds)) {
            label_bounds <- bounds[0, , drop = FALSE]
        }
        keep <- textpath_keep(arc, label_bounds, pad)
        for (i in seq_len(nrow(keep))) {
            if (keep$right[[i]] - keep$left[[i]] <= 1e-6) {
                next
            }

            run <- path_slice(x, y, arc, keep$left[[i]], keep$right[[i]])
            if (nrow(run) < 2L) {
                next
            }

            # Store path fragments and flatten once after all gaps are removed;
            # repeated vector growth is avoidable on dense contour paths.
            runs[[length(runs) + 1L]] <- run
        }
    }

    if (!length(runs)) {
        return(new_data_frame(list(
            x = numeric(),
            y = numeric(),
            id = integer()
        )))
    }
    run_lengths <- vapply(runs, nrow, integer(1L))
    new_data_frame(list(
        x = unlist(lapply(runs, `[[`, "x"), use.names = FALSE),
        y = unlist(lapply(runs, `[[`, "y"), use.names = FALSE),
        id = rep.int(seq_along(runs), run_lengths)
    ))
}

# Convert label spans into the complementary path intervals that should remain
# visible after gap removal.
textpath_keep <- function(arc, label_bounds, pad) {
    path_length <- max(arc)
    if (!nrow(label_bounds)) {
        return(new_data_frame(list(left = 0, right = path_length)))
    }

    gaps <- new_data_frame(list(
        left = pmax(0, label_bounds$left - pad),
        right = pmin(path_length, label_bounds$right + pad)
    ))
    gaps <- gaps[gaps$right > gaps$left, , drop = FALSE]
    if (!nrow(gaps)) {
        return(new_data_frame(list(left = 0, right = path_length)))
    }

    gaps <- gaps[order(gaps$left, gaps$right), , drop = FALSE]
    merged <- list()
    for (i in seq_len(nrow(gaps))) {
        if (
            !length(merged) || gaps$left[[i]] > merged[[length(merged)]]$right
        ) {
            merged[[length(merged) + 1L]] <- gaps[i, , drop = FALSE]
        } else {
            merged[[length(merged)]]$right <- max(
                merged[[length(merged)]]$right,
                gaps$right[[i]]
            )
        }
    }
    gaps <- do.call(rbind, merged)

    starts <- c(0, gaps$right)
    ends <- c(gaps$left, path_length)
    keep <- new_data_frame(list(left = starts, right = ends))
    keep[keep$right > keep$left, , drop = FALSE]
}

# Slice a polyline by arclength, interpolating boundary points when the label
# gap cuts through the middle of an original segment.
path_slice <- function(x, y, arc, left, right) {
    inside <- arc > left & arc < right
    xx <- c(
        path_at(x, arc, left),
        x[inside],
        path_at(x, arc, right)
    )
    yy <- c(
        path_at(y, arc, left),
        y[inside],
        path_at(y, arc, right)
    )
    keep <- c(TRUE, xx[-1L] != xx[-length(xx)] | yy[-1L] != yy[-length(yy)])
    new_data_frame(list(x = xx[keep], y = yy[keep]))
}

# Interpolate one coordinate vector on an arclength-parametrised polyline.
path_at <- function(value, arc, target) {
    stats::approx(arc, value, xout = target, rule = 2, ties = "ordered")$y
}

# Extract one group-level aesthetic value while preserving grid unit objects,
# which behave differently from ordinary data-frame vectors.
first_value <- function(x, default = NULL) {
    if (is.null(x) || !length(x)) {
        return(default)
    }
    if (grid::is.unit(x)) {
        return(x[1L])
    }
    x[[1L]]
}

# Combine colour and alpha in the same late stage used for constructing grid
# parameters, so the fast geom works for mapped alpha values.
apply_alpha <- function(colour, alpha) {
    if (length(alpha) && !is.na(alpha)) {
        return(grDevices::adjustcolor(colour, alpha.f = alpha))
    }
    colour
}
