#' @importFrom ggplot2 layer
psychro_layer <- function (`_class` = NULL, ...) {
    l <- layer(...)
    class(l) <- c(`_class`, "PsyLayer", class(l))
    l
}
