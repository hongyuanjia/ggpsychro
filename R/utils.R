# Return a fallback only when the primary value is NULL.
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

# Test whether a value is ggplot2's waiver sentinel without importing the helper.
util__is_waive <- function(x) inherits(x, "waiver")

# copied from ggplot2/R/performance.R
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
util__new_data_frame <- function(x = list(), n = NULL) {
    if (length(x) != 0 && is.null(names(x))) {
        stop("Elements must be named")
    }
    lengths <- vapply(x, length, integer(1))
    if (is.null(n)) {
        n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
    }
    for (i in seq_along(x)) {
        if (lengths[i] == n) {
            next
        }
        if (lengths[i] != 1) {
            stop("Elements must equal the number of rows or 1")
        }
        x[[i]] <- rep(x[[i]], n)
    }

    class(x) <- "data.frame"

    attr(x, "row.names") <- .set_row_names(n)
    x
}

# Return psychrolib's mutable option store for temporary unit and tolerance edits.
psychrolib__options <- function() {
    get("PSYCHRO_OPT", envir = asNamespace("psychrolib"), inherits = FALSE)
}

# Evaluate an expression under a temporary psychrolib unit system and restore it.
psychrolib__with_units <- function(units, expr) {
    psy_op <- psychrolib__options()
    old_units <- psy_op$UNITS
    old_tolerance <- psy_op$TOLERANCE
    # SetUnitSystem() rejects NA, so restore psychrolib's internal options
    # directly and avoid leaking ggpsychro's temporary unit context.
    on.exit(
        {
            psy_op$UNITS <- old_units
            psy_op$TOLERANCE <- old_tolerance
        },
        add = TRUE
    )
    psychrolib::SetUnitSystem(units)
    force(expr)
}

# Evaluate an expression with psychrolib's lower humidity-ratio clamp disabled.
psychrolib__with_no_hum_limit <- function(expr) {
    psy_op <- psychrolib__options()
    old <- psy_op$MIN_HUM_RATIO
    psy_op$MIN_HUM_RATIO <- -Inf
    on.exit(psy_op$MIN_HUM_RATIO <- old, add = TRUE)
    force(expr)
}

# Encode public unit names into compact integer codes used by native routines.
unit__encode <- function(units) {
    switch(
        units,
        "SI" = 1L,
        "IP" = 2L,
        stop("'units' can only be either 'SI' or 'IP'.")
    )
}

# Decode native integer unit codes back to public unit names.
unit__decode <- function(code) {
    c("SI", "IP")[code]
}

# Resolve encoded, factor, or character unit columns to a single unit system.
unit__from_data <- function(data) {
    units <- unique(data$units)
    if (length(units) != 1L) {
        stop("`units` must resolve to a single unit system.", call. = FALSE)
    }

    if (is.factor(units)) {
        units <- as.character(units)
    }
    if (is.character(units)) {
        return(match.arg(units, c("SI", "IP")))
    }

    unit__decode(as.integer(units))
}

# Convert between the SI and IP display units used by chart limits.
unit__convert_display <- function(x, to) {
    switch(
        to,
        "F" = unit__f_from_c(x),
        "C" = unit__c_from_f(x),
        "Gr" = unit__gr_from_g(x),
        "G" = unit__g_from_gr(x)
    )
}

# Convert Celsius dry-bulb temperatures to Fahrenheit.
unit__f_from_c <- function(x) x * 9. / 5. + 32.

# Convert Fahrenheit dry-bulb temperatures to Celsius.
unit__c_from_f <- function(x) (x - 32) * 5. / 9.

# Convert IP humidity grains per lb_dry_air to SI g per kg_dry_air.
unit__g_from_gr <- function(x) x / 7.

# Convert SI humidity g per kg_dry_air to IP grains per lb_dry_air.
unit__gr_from_g <- function(x) x * 7.

# Return dry-bulb domain limits in the requested public unit system.
psychro__tdb_limits <- function(units) {
    if (units == "SI") {
        c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max)
    } else if (units == "IP") {
        unit__convert_display(c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max), "F")
    }
}

# Return humidity-ratio display limits in the requested public unit system.
psychro__hum_limits <- function(units) {
    if (units == "SI") {
        c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max)
    } else if (units == "IP") {
        unit__convert_display(c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max), "Gr")
    }
}

# Return default chart limits for empty psychrometric panels.
psychro__default_limits <- function(units) {
    list(
        tdb = if (units == "SI") {
            c(GGPSY_OPT$tdb_default_min, GGPSY_OPT$tdb_default_max)
        } else {
            unit__convert_display(
                c(GGPSY_OPT$tdb_default_min, GGPSY_OPT$tdb_default_max),
                "F"
            )
        },
        hum = if (units == "SI") {
            c(GGPSY_OPT$hum_default_min, GGPSY_OPT$hum_default_max)
        } else {
            unit__convert_display(
                c(GGPSY_OPT$hum_default_min, GGPSY_OPT$hum_default_max),
                "Gr"
            )
        }
    )
}

# Return the default protractor configuration used by ggpsychro() metadata.
psychro__default_protractor <- function() {
    list(
        show = FALSE,
        label = TRUE,
        annotation = TRUE,
        scale = 1,
        radius = 0.24,
        margin = 0.08,
        guide = guide_psychro_protractor(),
        style = list(),
        label_style = list()
    )
}

# Convert native psychrolib humidity ratios to chart display units.
unit__hum_to_chart <- function(hum, units) {
    if (units == "SI") {
        hum * 1000.0
    } else {
        hum * 7000.0
    }
}

# Convert chart display humidity ratios back to native psychrolib ratios.
unit__hum_from_chart <- function(hum, units) {
    if (units == "SI") {
        hum / 1000.0
    } else {
        hum / 7000.0
    }
}

# Drop missing values and return NULL for zero-length vectors.
util__remove_na <- function(x) if (!length(x)) NULL else x[!is.na(x)]

# Clamp values to a closed numeric interval.
util__cut_oob <- function(x, limits) {
    x[x < limits[1L]] <- limits[1L]
    x[x > limits[2L]] <- limits[2L]
    x
}

# Rescale values linearly into [0, 1] using the supplied interval.
util__rescale01 <- function(x, limits) {
    (x - limits[1L]) / (limits[2L] - limits[1L])
}

# Apply an optional alpha channel to grid and coord-owned foreground colours.
util__apply_alpha <- function(colour, alpha) {
    if (is.null(alpha) || length(alpha) == 0L || is.na(alpha)) {
        return(colour)
    }
    grDevices::adjustcolor(colour, alpha.f = alpha)
}

# Test whether points are inside or on the boundary of a polygon.
util__inside_polygon <- function(
    x,
    y,
    polygon_x,
    polygon_y,
    tolerance = 1e-8
) {
    n <- length(polygon_x)
    inside <- rep(FALSE, length(x))
    on_boundary <- rep(FALSE, length(x))
    j <- n

    for (i in seq_len(n)) {
        xi <- polygon_x[[i]]
        yi <- polygon_y[[i]]
        xj <- polygon_x[[j]]
        yj <- polygon_y[[j]]

        # Boundary points should survive clipping and label filtering, so they
        # are tracked separately from the ray-casting inside flag.
        cross <- (x - xi) * (yj - yi) - (y - yi) * (xj - xi)
        within <- x >= min(xi, xj) - tolerance &
            x <= max(xi, xj) + tolerance &
            y >= min(yi, yj) - tolerance &
            y <= max(yi, yj) + tolerance
        on_boundary <- on_boundary | (abs(cross) <= tolerance & within)

        intersects <- ((yi > y) != (yj > y)) &
            (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
        inside[intersects] <- !inside[intersects]
        j <- i
    }

    inside | on_boundary
}
