is.waive <- function(x) inherits(x, "waiver")

# new_data_frame {{{
# copied from ggplot2/R/performance.R
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
    if (length(x) != 0 && is.null(names(x))) {
        abort("Elements must be named")
    }
    lengths <- vapply(x, length, integer(1))
    if (is.null(n)) {
        n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
    }
    for (i in seq_along(x)) {
        if (lengths[i] == n) next
        if (lengths[i] != 1) {
            abort("Elements must equal the number of rows or 1")
        }
        x[[i]] <- rep(x[[i]], n)
    }

    class(x) <- "data.frame"

    attr(x, "row.names") <- .set_row_names(n)
    x
}
# }}}

# with_units {{{
#' @importFrom psychrolib SetUnitSystem
with_units <- function (units, expr) {
    psychrolib::SetUnitSystem(units)
    psy_op <- psychrolib:::PSYCHRO_OPT
    on.exit(psy_op$UNITS <- NA_character_, add = TRUE)

    force(expr)
}
# }}}

# The units of humidity ratio is lb_H2O lb_Air-1 [IP] or kg_H2O kg_Air-1 [SI],
# but for Psychrometric Chart, we use gr_H2O lb_Air-1 [IP] or g_H2O kg_Air-1
# [SI]. Should amplify before plotting or do reversely during calculation
# amplify_hum {{{
amplify_hum <- function (hum, units) {
    if (units == "SI") {
        hum * 1000.0
    } else {
        hum * 7000.0
    }
}
# }}}
# narrow_hum {{{
narrow_hum <- function (hum, units) {
    if (units == "SI") {
        hum / 1000.0
    } else {
        hum / 7000.0
    }
}
# }}}
# bid_conv {{{
# bidirectional conversion
bid_conv <- function (x, to) {
    switch(to,
        "F" = get_f_from_c(x),
        "C" = get_c_from_f(x),
        "Gr" = get_gr_from_g(x),
        "G" = get_g_from_gr(x)
    )
}
get_f_from_c <- function (x) x * 9. / 5. + 32.
get_c_from_f <- function (x) (x - 32) * 5. / 9.
get_g_from_gr <- function (x) x / 7.
get_gr_from_g <- function (x) x * 7.
# }}}
