# is.ggpsychro {{{
is.ggpsychro <- function (x) {
    inherits(x, "ggpsychro")
}
# }}}

#' @importFrom ggplot2 ggplot_build
#' @export
# ggplot_build.ggpsychro {{{
ggplot_build.ggpsychro <- function (plot) {
    plot <- cover_mask(plot)
    NextMethod()
}
# }}}

## print.ggpsychro {{{
##' @export
#print.ggpsychro <- function (x, ...) {
#    x <- cover_mask(x)
#    NextMethod()
#}

##' @export
#plot.ggpsychro <- print.ggpsychro
## }}}

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
