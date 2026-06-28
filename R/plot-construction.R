#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.CoordPsychro <- function(object, plot, object_name, ...) {
    if (!is.ggpsychro(plot)) {
        return(NextMethod())
    }

    plot$psychro$grids <- merge_psychro_grids(plot$psychro$grids)
    if (is.null(object$grids)) {
        object$grids <- plot$psychro$grids
    } else {
        object$grids <- merge_psychro_grids(object$grids)
        plot$psychro$grids <- object$grids
    }
    if (is.null(object$grid_labels)) {
        object$grid_labels <- plot$psychro$grid_labels
    } else {
        plot$psychro$grid_labels <- object$grid_labels
    }
    if (is.null(object$protractor)) {
        object$protractor <- plot$psychro$protractor
    } else {
        plot$psychro$protractor <- object$protractor
    }

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

#' @export
ggplot_add.PsyGrid <- function(object, plot, object_name, ...) {
    add_psychro_grid(object, plot)
}

#' @export
ggplot_add.PsyComfortForeground <- function(object, plot, object_name, ...) {
    add_psychro_comfort_foreground(object, plot)
}

add_psychro_comfort_foreground <- function(object, plot) {
    if (!is.ggpsychro(plot)) {
        stop(
            "`geom_comfort_givoni()` foreground markers can only be added ",
            "to a ggpsychro plot.",
            call. = FALSE
        )
    }
    plot$coordinates$comfort_foreground <- c(
        plot$coordinates$comfort_foreground, list(object)
    )
    plot
}

add_psychro_grid <- function(object, plot) {
    if (!is.ggpsychro(plot)) {
        stop("`geom_grid_*()` helpers can only be added to a ggpsychro plot.", call. = FALSE)
    }

    plot$psychro$grids <- merge_psychro_grids(plot$psychro$grids)
    plot$psychro$grids[[object$type]] <- object$show
    if (is.null(plot$psychro$grid_labels)) {
        plot$psychro$grid_labels <- list()
    }
    plot$psychro$grid_labels[[object$type]] <- object$label
    if (!object$show) {
        plot$psychro$grid_labels[[object$type]]$show <- FALSE
    }

    if (inherits(plot$coordinates, "CoordPsychro")) {
        plot$coordinates$grids <- plot$psychro$grids
        plot$coordinates$grid_labels <- plot$psychro$grid_labels
    }

    theme_args <- psychro_grid_theme(object$type, object$style)
    if (length(theme_args)) {
        plot <- plot + do.call(ggplot2::theme, theme_args)
    }

    plot
}

#' @export
ggplot_add.PsyProtractor <- function(object, plot, object_name, ...) {
    add_psychro_protractor(object, plot)
}

add_psychro_protractor <- function(object, plot) {
    if (!is.ggpsychro(plot)) {
        stop("`geom_psychro_protractor()` can only be added to a ggpsychro plot.", call. = FALSE)
    }

    plot$psychro$protractor <- object

    if (inherits(plot$coordinates, "CoordPsychro")) {
        plot$coordinates$protractor <- object
    }

    plot
}

local({
    ggplot2_ns <- asNamespace("ggplot2")
    if (exists("update_ggplot", envir = ggplot2_ns, inherits = FALSE) &&
            exists("class_ggplot", envir = ggplot2_ns, inherits = FALSE)) {
        update_ggplot <- get("update_ggplot", envir = ggplot2_ns)
        class_ggplot <- get("class_ggplot", envir = ggplot2_ns)
        S7::method(update_ggplot, list(S7::new_S3_class("PsyGrid"), class_ggplot)) <- function(object, plot, ...) {
            add_psychro_grid(object, plot)
        }
        S7::method(update_ggplot, list(S7::new_S3_class("PsyProtractor"), class_ggplot)) <- function(object, plot, ...) {
            add_psychro_protractor(object, plot)
        }
        S7::method(update_ggplot, list(S7::new_S3_class("PsyComfortForeground"), class_ggplot)) <- function(object, plot, ...) {
            add_psychro_comfort_foreground(object, plot)
        }
    }
})

#' @importFrom S7 method new_S3_class
NULL

#' @export
ggplot_add.PsyScale <- function(object, plot, object_name, ...) {
    # check if the trans is set as waiver()
    if (is.empty_trans(object$trans)) {
        # assign new trans
        trans <- get(paste0(object$scale_name, "_trans"), envir = asNamespace("ggpsychro"))
        object$trans <- trans(units = plot$psychro$units)
        if (is.numeric(object$limits)) {
            object$limits <- object$trans$transform(object$limits)
        }
    }

    if (identical(object$scale_name, "drybulb")) {
        object$aesthetics <- if (plot$psychro$mollier) GGPSY_OPT$y_aes else GGPSY_OPT$x_aes
    }

    if (identical(object$scale_name, "humratio")) {
        object$aesthetics <- if (plot$psychro$mollier) GGPSY_OPT$x_aes else GGPSY_OPT$y_aes
    }

    NextMethod()
}
