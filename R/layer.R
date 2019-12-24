#' @importFrom ggplot2 layer
psychro_layer <- function (...) {
    l <- layer(...)
    class(l) <- c("PsyLayer", class(l))
    l
}
