# Test whether a value is ggplot2's waiver sentinel without importing the helper.
is.waive <- function(x) inherits(x, "waiver")

# copied from ggplot2/R/performance.R
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
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
psychrolib_options <- function() {
    get("PSYCHRO_OPT", envir = asNamespace("psychrolib"), inherits = FALSE)
}

# Evaluate an expression under a temporary psychrolib unit system and restore it.
with_units <- function(units, expr) {
    psy_op <- psychrolib_options()
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
with_no_hum_limit <- function(expr) {
    psy_op <- psychrolib_options()
    old <- psy_op$MIN_HUM_RATIO
    psy_op$MIN_HUM_RATIO <- -Inf
    on.exit(psy_op$MIN_HUM_RATIO <- old, add = TRUE)
    force(expr)
}

# Encode public unit names into compact integer codes used by native routines.
encode_units <- function(units) {
    switch(
        units,
        "SI" = 1L,
        "IP" = 2L,
        stop("'units' can only be either 'SI' or 'IP'.")
    )
}

# Decode native integer unit codes back to public unit names.
decode_units <- function(code) {
    c("SI", "IP")[code]
}

# Resolve encoded, factor, or character unit columns to a single unit system.
get_units <- function(data) {
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

    decode_units(as.integer(units))
}

# Convert between the SI and IP display units used by chart limits.
bid_conv <- function(x, to) {
    switch(
        to,
        "F" = get_f_from_c(x),
        "C" = get_c_from_f(x),
        "Gr" = get_gr_from_g(x),
        "G" = get_g_from_gr(x)
    )
}

# Convert Celsius dry-bulb temperatures to Fahrenheit.
get_f_from_c <- function(x) x * 9. / 5. + 32.

# Convert Fahrenheit dry-bulb temperatures to Celsius.
get_c_from_f <- function(x) (x - 32) * 5. / 9.

# Convert IP humidity grains per lb_dry_air to SI g per kg_dry_air.
get_g_from_gr <- function(x) x / 7.

# Convert SI humidity g per kg_dry_air to IP grains per lb_dry_air.
get_gr_from_g <- function(x) x * 7.

# Return dry-bulb domain limits in the requested public unit system.
get_tdb_limits <- function(units) {
    if (units == "SI") {
        c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max), "F")
    }
}

# Return humidity-ratio display limits in the requested public unit system.
get_hum_limits <- function(units) {
    if (units == "SI") {
        c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max), "Gr")
    }
}

# Return default chart limits for empty psychrometric panels.
default_psychro_limits <- function(units) {
    list(
        tdb = if (units == "SI") c(0, 50) else bid_conv(c(0, 50), "F"),
        hum = if (units == "SI") c(0, 50) else bid_conv(c(0, 50), "Gr")
    )
}

# Return the default protractor configuration used by ggpsychro() metadata.
default_psychro_protractor <- function() {
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

# Compute Euclidean distance between paired start and end coordinates.
dist_euclid <- function(x, y, xend, yend) {
    sqrt((xend - x)^2 + (yend - y)^2)
}

# Repeat a data frame n times and row-bind the repeated copies.
rep_dataframe <- function(df, n) {
    do.call(rbind, replicate(n, df, simplify = FALSE))
}

# The units of humidity ratio is lb_H2O lb_Air-1 [IP] or kg_H2O kg_Air-1 [SI],
# but for Psychrometric Chart, we use gr_H2O lb_Air-1 [IP] or g_H2O kg_Air-1
# [SI]. Should amplify before plotting or do reversely during calculation
amplify_hum <- function(hum, units) {
    if (units == "SI") {
        hum * 1000.0
    } else {
        hum * 7000.0
    }
}

# Convert chart display humidity ratios back to native psychrolib ratios.
narrow_hum <- function(hum, units) {
    if (units == "SI") {
        hum / 1000.0
    } else {
        hum / 7000.0
    }
}

# The units of enthalpy is J kg-1 [SI], but for Psychrometric Chart, we use kJ
# kg-1 [SI]. Should amplify before plotting or do reversely during calculation
amplify_enth <- function(enth, units) {
    if (units == "SI") {
        enth * 1000.0
    } else {
        enth
    }
}

# Convert chart display enthalpy back to native psychrolib enthalpy.
narrow_enth <- function(enth, units) {
    if (units == "SI") {
        enth / 1000.0
    } else {
        enth
    }
}

# Compute the slope of a segment from start and end coordinates.
slope <- function(x, y, xend, yend) {
    (yend - y) / (xend - x)
}

# adopted from thomasp85/ggraph/R/utils.R
# Convert segment slopes to angles for path text and guide placement.
line_angle <- function(x, y, xend, yend, degrees = TRUE) {
    angles <- atan(slope(x, y, xend, yend))
    angles[is.nan(angles)] <- 2 * pi
    angles[angles < 0] <- angles[angles < 0] + 2 * pi
    if (degrees) {
        angles * 360 / (2 * pi)
    } else {
        angles
    }
}

# Drop missing values and return NULL for zero-length vectors.
remove_na <- function(x) if (!length(x)) NULL else x[!is.na(x)]

# Clamp values to a closed numeric interval.
cut_oob <- function(x, limits) {
    x[x < limits[1L]] <- limits[1L]
    x[x > limits[2L]] <- limits[2L]
    x
}

# Test whether values fall outside a closed numeric interval.
is_oob <- function(x, limits) {
    x < limits[1L] | x > limits[2L]
}

# Rescale values linearly into [0, 1] using the supplied interval.
rescale01 <- function(x, limits) {
    (x - limits[1L]) / (limits[2L] - limits[1L])
}
