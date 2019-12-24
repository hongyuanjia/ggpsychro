#' @export
# is.ggpsychro {{{
is.ggpsychro <- function (x) {
    inherits(x, "ggpsychro")
}
# }}}

#' @importFrom ggplot2 ggplot_add
#' @export
# ggplot_add.PsyLayer{{{
ggplot_add.PsyLayer <- function (object, plot, object_name) {
    # only manually add data for grid layer
    type <- get_grid_type(object)

    if (is.na(type)) return(NextMethod())

    waive_units <- is.waive(object$aes_params$units)

    if (!is.ggpsychro(plot)) {
        if (waive_units) abort_unit_waiver(object_name)
        return(NextMethod())
    }

    # update units if waived
    if (waive_units) object$aes_params$units <- plot$psychro$units

    # init grid value to 0 to bypass aesthetics checking
    val <- list(grid_value = 0)
    names(val) <- type

    # create init data for this layer
    if (is.waive(object$data)) {
        object$data <- new_data_frame(c(val, list(
            pres = plot$psychro$pressure
        )))

        # add mapping
        if (is.null(object$mapping)) {
            # try to avoid use quasiquotation
            object$mapping <- switch(type,
                grid_relhum = aes(grid_relhum = grid_relhum, pres = pres),
                grid_wetbulb = aes(grid_wetbulb = grid_wetbulb, pres = pres),
                grid_vappres = aes(grid_vappres = grid_vappres, pres = pres),
                grid_specvol = aes(grid_specvol = grid_specvol, pres = pres),
                grid_enthalpy = aes(grid_enthalpy = grid_enthalpy, pres = pres),
            )
        }
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
        object$trans <- get_trans_by_aes(object$aesthetics)(plot$psychro$units)
    }

    NextMethod()
}
# }}}

#' @importFrom ggplot2 ggplot_build
#' @export
# ggplot_build.ggpsychro {{{
ggplot_build.ggpsychro <- function (plot) {
    # move mastarea and sat line to the last
    plot <- cover_mask(plot)

    # get all layers
    layers <- plot$layers

    # get all layer grid types
    type <- lapply(layers, get_grid_type)

    # do nothing if there is no grid layer
    if (all(is.na(type))) return(NextMethod())

    # get current scale list
    scales <- plot$scales

    for (i in seq_along(type)) {
        aes <- type[[i]]

        # skip if not a grid layer
        if (is.na(aes)) next

        # skip if there is already scale added for it
        if (scales$has_scale(aes)) next

        scales$add(get_grid_scale_by_aes(aes)(plot$psychro$units))
    }

    plot$scales <- scales

    NextMethod()
}
# }}}

# layer_names {{{
# get all layer names
layer_names <- function (gg) {
    vapply(gg$layers, function (l) class(l$geom)[[1L]], character(1))
}
# }}}

# cover_mask {{{
# Put the mask layer to the end of all layers
cover_mask <- function (gg) {
    # get all layer name
    nm <- layer_names(gg)

    # get the last mask layer
    pos_m <- which(nm == "GeomMaskArea")
    # get the last sat layer
    pos_s <- which(nm == "GeomLineSat")

    if (!length(pos_m) && !length(pos_s)) return(gg)

    # remove the last mask layer to the end
    if (length(pos_m) > 1L) pos_m <- max(pos_m)
    if (length(pos_s) > 1L) pos_s <- max(pos_s)
    gg$layers <- c(gg$layers[-c(pos_m, pos_s)], gg$layers[c(pos_m, pos_s)])

    gg
}
# }}}

# get_trans_by_aes {{{
get_trans_by_aes <- function (aes) {
    get(paste0(substring(aes, 6L), "_trans"), asNamespace("ggpsychro"))
}
# }}}

# get_grid_scale_by_aes {{{
get_grid_scale_by_aes <- function (aes) {
    get(paste0("scale_", aes), asNamespace("ggpsychro"))
}
# }}}

# abort_unit_waiver {{{
abort_unit_waiver <- function (object_name) {
    stop(sprintf("%s: 'units' cannot be 'waiver()' when adding to a non-ggpsychro plot.",
            gsub("()", "", object_name, fixed = TRUE)),
        call. = FALSE
    )
}
# }}}
