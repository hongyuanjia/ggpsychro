# Collect every grob below a plot/table node so tests can inspect nested grid
# output without depending on a specific gtable depth.
collect_grobs <- function(grob) {
    children <- c(
        if (!is.null(grob$grobs)) grob$grobs else list(),
        if (!is.null(grob$children)) as.list(grob$children) else list()
    )

    c(list(grob), unlist(lapply(children, collect_grobs), recursive = FALSE))
}

# Count visible line-like grid primitives after ggplot2 has built the grob tree.
count_line_shapes <- function(plot) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    sum(vapply(
        grobs,
        function(grob) {
            inherits(grob, "polyline") ||
                inherits(grob, "polygon") ||
                inherits(grob, "polyclipgrob")
        },
        logical(1)
    ))
}

# Count psychrometric textpath grobs to distinguish native labels from ordinary
# grid text labels.
count_textpath_shapes <- function(plot) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    sum(vapply(
        grobs,
        function(grob) {
            inherits(grob, "psychro_textpath")
        },
        logical(1)
    ))
}

# Count named grobs with a regex so tests can assert guide and clipping layout
# without locking to a full gtable snapshot.
count_named_grobs <- function(plot, pattern) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    sum(vapply(
        grobs,
        function(grob) {
            name <- grob$name
            !is.null(name) && grepl(pattern, name)
        },
        logical(1)
    ))
}

# Return named grobs with a regex when tests need to inspect their coordinates.
find_named_grobs <- function(plot, pattern) {
    grobs <- collect_grobs(ggplot2::ggplotGrob(plot))
    grobs[vapply(
        grobs,
        function(grob) {
            name <- grob$name
            !is.null(name) && grepl(pattern, name)
        },
        logical(1)
    )]
}

# Search within an already extracted grob subtree without rebuilding the plot.
find_named_grobs_in <- function(grob, pattern) {
    grobs <- collect_grobs(grob)
    grobs[vapply(
        grobs,
        function(grob) {
            name <- grob$name
            !is.null(name) && grepl(pattern, name)
        },
        logical(1)
    )]
}

# Locate panel children by name after ggplot has composed the panel grob.
panel_child_index <- function(plot, pattern) {
    panel <- panel_grob(plot)
    which(grepl(pattern, names(panel$children)))
}

# Extract the single-panel grob used by most ggpsychro rendering tests.
panel_grob <- function(plot) {
    gtable <- ggplot2::ggplotGrob(plot)
    gtable$grobs[[which(gtable$layout$name == "panel")]]
}

# Convert units in a fixed rectangular viewport to make guide geometry tests
# independent from the ambient graphics device.
convert_units_in_rectangular_viewport <- function(x, y) {
    path <- tempfile(fileext = ".pdf")
    grDevices::pdf(path, width = 8, height = 4)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(
        width = grid::unit(8, "cm"),
        height = grid::unit(4, "cm")
    ))
    out <- list(
        x = grid::convertX(x, "mm", valueOnly = TRUE),
        y = grid::convertY(y, "mm", valueOnly = TRUE)
    )
    grid::popViewport()
    grDevices::dev.off()
    unlink(path)
    out
}

# Measure the rendered protractor arc radius for regression-style guide tests.
protractor_arc_radius_mm <- function(plot) {
    arc <- find_named_grobs(plot, "psychro-protractor-arc")[[1L]]
    arc_mm <- convert_units_in_rectangular_viewport(arc$x, arc$y)
    diff(range(arc_mm$x)) / 2
}

# Assert that empty charts were trained with real psychrometric limits rather
# than ggplot2's fallback [0, 1] panel range.
expect_trained_panel_ranges <- function(plot) {
    built <- ggplot2::ggplot_build(plot)
    panel <- built$layout$panel_params[[1L]]

    expect_length(panel$x$scale$range$range, 2L)
    expect_length(panel$y$scale$range$range, 2L)
    expect_gt(diff(panel$x$continuous_range), 0)
    expect_gt(diff(panel$y$continuous_range), 0)
    expect_false(
        isTRUE(all.equal(panel$x$continuous_range, c(0, 1))) &&
            isTRUE(all.equal(panel$y$continuous_range, c(0, 1)))
    )

    invisible(built)
}

# Read one typed pythermalcomfort oracle vector for a model/case metric.
comfort_oracle <- function(model, case, metric) {
    path <- testthat::test_path("fixtures", "comfort-oracle.csv")
    oracle <- utils::read.csv(path, stringsAsFactors = FALSE)
    rows <- oracle[
        oracle$model == model & oracle$case == case & oracle$metric == metric,
        ,
        drop = FALSE
    ]
    expect_gt(nrow(rows), 0L)
    rows <- rows[order(rows$index), , drop = FALSE]
    expect_equal(rows$index, seq_len(nrow(rows)))

    values <- Map(comfort_oracle_value, rows$type, rows$value)
    values <- unlist(values, use.names = FALSE)

    if (all(rows$type == "numeric")) {
        return(as.numeric(values))
    }
    if (all(rows$type == "logical")) {
        return(as.logical(values))
    }
    if (all(rows$type == "character")) {
        return(as.character(values))
    }

    stop("Mixed oracle value types are not supported.", call. = FALSE)
}

# Convert one serialized oracle cell back to the closest R scalar type.
comfort_oracle_value <- function(type, value) {
    switch(
        type,
        numeric = as.numeric(value),
        logical = as.logical(value),
        character = as.character(value),
        stop("Unknown oracle value type: ", type, call. = FALSE)
    )
}

# Compare a ggpsychro output column with the stored pythermalcomfort vector.
expect_comfort_oracle <- function(
    actual,
    model,
    case,
    metric,
    tolerance = 1e-8
) {
    expected <- comfort_oracle(model, case, metric)
    actual <- actual[[metric]]
    expect_equal(length(actual), length(expected))
    # pythermalcomfort stores unavailable string fields as NaN; ggpsychro uses
    # the natural R missing value for the public column type.
    expect_equal(is.na(actual), is.na(expected))
    keep <- !is.na(actual) & !is.na(expected)
    if (!any(keep)) {
        return(invisible(NULL))
    }
    if (is.numeric(expected)) {
        expect_equal(actual[keep], expected[keep], tolerance = tolerance)
    } else {
        expect_equal(actual[keep], expected[keep])
    }
}

# Drop the automatically added saturation layer when tests only need user data.
built_data_layers <- function(built) {
    keep <- !vapply(
        built$plot$layers,
        function(layer) {
            inherits(layer$geom, "GeomPsychroSaturation")
        },
        logical(1L)
    )
    built$data[keep]
}

# Return the first non-saturation built layer for compact test expectations.
first_built_data <- function(built) {
    built_data_layers(built)[[1L]]
}
