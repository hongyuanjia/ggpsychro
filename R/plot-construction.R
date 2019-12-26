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
