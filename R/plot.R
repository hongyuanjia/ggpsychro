# print.ggpsychro {{{
#' @export
print.ggpsychro <- function (x, ...) {
    x <- cover_mask(x)
    NextMethod()
}

#' @export
plot.ggpsychro <- print.ggpsychro
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
    pos <- which(nm == "GeomMaskArea")

    if (!length(pos)) return(gg)

    # remove the last mask layer to the end
    if (length(pos) > 1L) pos <- max(pos)
    gg$layers <- c(gg$layers[-pos], gg$layers[pos])

    gg
}
# }}}
