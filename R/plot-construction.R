#' @export
ggplot_add.CoordPsychro <- function(object, plot, object_name) {
    if (!is.ggpsychro(plot)) return(NextMethod())

    # update plot meta data if necessary
    if (is.null(object$mollier)) {
        object$mollier <- plot$psychro$mollier
    } else if (object$mollier != plot$psychro$mollier) {
        if (!plot$psychro$mollier) {
            message("Chart type has been reset to Mollier Chart.")
        } else {
            message("Chart type has been reset to Psychrometric Chart.")
        }
        plot$psychro$mollier <- object$mollier
    }

    if (is.null(object$units)) {
        object$units <- plot$psychro$units
    } else if (object$units != plot$psychro$units) {
        message(sprintf("Unit system has been reset from '%s' to '%s'.", plot$psychro$units, object$units))
        plot$psychro$units <- object$units
    }

    if (is.null(object$altitude)) {
        object$altitude <- plot$psychro$altitude
    } else if (object$altitude != plot$psychro$altitude) {
        u <- if (object$units == "SI") "m" else "ft"
        message(sprintf(
            "Altitude has been reset from '%s %s' to '%s %s'.",
            plot$psychro$altitude, u, object$altitude, u
        ))
        plot$psychro$altitude <- object$altitude
    }

    if (is.null(object$limits$tdb)) {
        object$limits$tdb <- plot$psychro$tdb_lim
    } else if (!identical(object$limits$tdb, plot$psychro$tdb_lim)) {
        if (!is.null(plot$psychro$tdb_lim)) {
            u <- if (object$units == "SI") "\u00B0C" else "\u00B0F"
            message(sprintf(
                "Dry-bulb temperature limits has been reset from '[%s] %s' to '[%s] %s'.",
                paste(plot$psychro$tdb_lim, collapse = ", "), u,
                paste(object$limits$tdb, collapse = ", "), u
            ))
        }
        plot$psychro$tdb_lim <- object$limits$tdb
    }

    if (is.null(object$limits$hum)) {
        object$limits$hum <- plot$psychro$hum_lim
    } else if (!identical(object$limits$hum, plot$psychro$hum_lim)) {
        if (!is.null(plot$psychro$hum_lim)) {
            u <- if (object$units == "SI") "g/kg" else "gr/lb"
            message(sprintf(
                "Humidity ratio limits has been reset from '[%s] %s' to '[%s] %s'.",
                paste(plot$psychro$hum_lim, collapse = ", "), u,
                paste(object$limits$hum, collapse = ", "), u
            ))
        }
        plot$psychro$hum_lim <- object$limits$hum
    }

    plot$coordinates <- object
    plot
}

#' @importFrom ggplot2 ggplot_add
#' @export
# ggplot_add.PsyLayer{{{
ggplot_add.PsyLayer <- function (object, plot, object_name) {
    if (!is.ggpsychro(plot)) {
        names(object$mapping)
        if ( is.waive(object$aes_params$units) ||
            (is.null(object$aes_params$units) && !"units" %in% names(object$mapping))) {
            abort_unit_waiver(object_name)
        }
        return(NextMethod())
    }

    meta <- plot$psychro

    if (is.ggpsychro(plot)) {
        object <- add_default_meta(object, meta$units, meta$pressure)
    }
    NextMethod()
}
# }}}

#' @importFrom ggplot2 ggplot_add
#' @export
# ggplot_add.PsyScale {{{
ggplot_add.PsyScale <- function (object, plot, object_name) {
    # check if the trans is set with units being waiver()
    if (is.empty_trans(object$trans)) {
        if (!is.ggpsychro(plot)) abort_unit_waiver(object_name)
        # assign new trans
        object$trans <- get_trans_by_aes(object$scale_name)(units = plot$psychro$units)
    }

    NextMethod()
}
# }}}

# add_default_meta {{{
add_default_meta <- function (object, units, pressure) {
    types <- paste(c("geom", "stat", "aes"), "params", sep = "_")

    for (type in types) {
        # add if not exists
        if (is.null(object[[type]])) {
            object[[type]] <- list(units = units, pres = pressure)
        } else {
            if (is.waive(object[[type]]$units) || is.null(object[[type]]$units)) {
                object[[type]]$units <- units
            }

            if (is.waive(object[[type]]$pres) || is.null(object[[type]]$pres)) {
                object[[type]]$pres <- pressure
            }
        }
    }

    object
}
# }}}
