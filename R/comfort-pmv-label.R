#' @include comfort-core.R
NULL

# PMV label helpers isolate text, justification, and level formatting rules.

# Convert PMV values into thermal sensation vote labels.
pmv__thermal_sensation <- function(pmv) {
    labels <- c(
        "Cold",
        "Cool",
        "Slightly Cool",
        "Neutral",
        "Slightly Warm",
        "Warm",
        "Hot"
    )
    out <- labels[findInterval(pmv, c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)) + 1L]
    out[is.na(pmv)] <- NA_character_
    out
}

# Keep only integer PMV levels with named sensation labels.
pmv__sensation_levels <- function(levels) {
    levels[!is.na(vapply(levels, pmv__sensation_label, character(1L)))]
}

# Resolve horizontal justification for PMV axis-side labels.
pmv__axis_label_text_hjust <- function(axis_label_hjust) {
    if (util__is_waive(axis_label_hjust)) {
        return(0.95)
    }
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(1 - max(0, min(0.2, axis_label_hjust[[1L]])))
    }
    0.95
}

# Resolve vertical justification for PMV axis-side labels.
pmv__axis_label_text_vjust <- function(axis_label_vjust, size = NULL) {
    if (util__is_waive(axis_label_vjust)) {
        size <- if (is.null(size)) 2.8 else as.numeric(size)[[1L]]
        offset <- max(3.5, size * ggplot2::.pt * 0.42)
        return(grid::unit(offset, "pt"))
    }
    if (grid::is.unit(axis_label_vjust)) {
        return(axis_label_vjust)
    }
    if (is.numeric(axis_label_vjust) && length(axis_label_vjust)) {
        return(axis_label_vjust[[1L]])
    }
    0.5
}

# Convert axis label hjust into the start offset along humidity ratio.
pmv__axis_label_offset <- function(axis_label_hjust) {
    if (util__is_waive(axis_label_hjust)) {
        return(0.025)
    }
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(max(0, min(0.08, axis_label_hjust[[1L]])))
    }
    0.025
}

# Convert axis label hjust into the end offset along humidity ratio.
pmv__axis_label_end <- function(axis_label_hjust) {
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(min(0.16, max(0, axis_label_hjust[[1L]]) + 0.055))
    }
    0.07
}

# Return the PMV contour linetype for a level.
pmv__linetype <- function(level) {
    if (abs(level) < 1e-8) "dashed" else "solid"
}

# Build the displayed PMV curve label for a level and label mode.
pmv__curve_label <- function(level, label) {
    switch(
        label,
        none = NA_character_,
        sensation = pmv__sensation_label(level),
        boundary = paste("PMV", pmv__format_level(level)),
        comfort = "COMFORT"
    )
}

# Resolve horizontal justification for PMV curve labels.
pmv__curve_hjust <- function(label, override = NULL) {
    if (!is.null(override)) {
        return(override)
    }
    switch(
        label,
        none = 0.5,
        sensation = 0.52,
        boundary = 0.045,
        comfort = 0.52
    )
}

# Resolve vertical justification for PMV curve labels.
pmv__curve_vjust <- function(
    level,
    label,
    override = NULL,
    mollier = FALSE
) {
    if (!is.null(override)) {
        return(override)
    }
    if (label == "boundary") {
        if (isTRUE(mollier)) {
            return(if (level <= 0) 1.25 else -0.25)
        }
        return(if (level <= 0) -0.25 else 1.25)
    }
    if (label == "comfort") {
        return(0.5)
    }
    if (abs(level) < 1e-8) {
        return(0.5)
    }
    0.5
}

# Return the named thermal sensation label for an integer PMV level.
pmv__sensation_label <- function(level) {
    if (abs(level - round(level)) > 1e-8) {
        return(NA_character_)
    }
    labels <- c(
        "-3" = "COLD",
        "-2" = "COOL",
        "-1" = "SLIGHTLY COOL",
        "0" = "NEUTRAL",
        "1" = "SLIGHTLY WARM",
        "2" = "WARM",
        "3" = "HOT"
    )
    labels[[as.character(as.integer(round(level)))]] %||% NA_character_
}

# Format a PMV level with an explicit sign for positive values.
pmv__format_level <- function(level) {
    ifelse(level > 0, sprintf("+%.1f", level), sprintf("%.1f", level))
}
