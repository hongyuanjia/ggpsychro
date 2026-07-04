#' Create a ggpsychro-aware layer
#'
#' Internal wrapper around [ggplot2::layer()] that tags generated layers with the
#' `PsyLayer` class and an optional private class for ggpsychro build hooks.
#'
#' @param `_class` Optional class name prepended to the generated layer.
#' @param ... Arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer object with ggpsychro-specific classes.
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 layer
psychro_layer <- function (`_class` = NULL, ...) {
    l <- layer(...)
    class(l) <- c(`_class`, "PsyLayer", class(l))
    l
}
