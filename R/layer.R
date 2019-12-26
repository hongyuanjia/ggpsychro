#' @importFrom ggplot2 layer
# psychro_layer {{{
psychro_layer <- function (`_class` = NULL, ...) {
    l <- layer(...)
    class(l) <- c(`_class`, "PsyLayer", class(l))
    l
}
# }}}

# get_layer_types {{{
get_layer_types <- function (layers) {
    vapply(layers, function (l) class(l)[[1L]], character(1))
}
# }}}

# get_geom_types {{{
get_geom_types <- function (layers) {
    vapply(layers, function (l) {
        nm <- names(.geom_list)[.geom_list %in% class(l$geom)]
        if (!length(nm)) return(NA_character_)
        nm[[1L]]
    }, character(1))
}
# }}}

# cover_mask {{{
# Put the mask layer to the end of all layers
cover_mask <- function (layers) {
    # get all layer name
    nm <- get_layer_types(layers)

    # get the last mask layer
    pos_m <- which(nm == "PsyLayerMaskArea")
    # get the last sat layer
    pos_s <- which(nm == "PsyLayerLineSat")

    if (!length(pos_m) && !length(pos_s)) return(layers)

    # remove the last mask layer to the end
    if (length(pos_m) > 1L) pos_m <- max(pos_m)
    if (length(pos_s) > 1L) pos_s <- max(pos_s)

    append(layers[-c(pos_m, pos_s)], layers[c(pos_m, pos_s)])
}
# }}}
