# add_relhum_line {{{
add_relhum_line <- function (min = 0.0, max = 1.0, step = 0.1, n = 101, style = waiver(), ...) {
    rh <- seq(min, max, step)
    # exclude the saturation line
    rh <- rh[rh != 1.0]
    lapply(rh, function (x) geom_relhum(value = x, n = n, ...))
}
# }}}

# geom_relhum_line {{{
#' Plot relative line
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#'
#' @section Aesthetics:
#' `geom_relhum_line` understands the following aesthetics (required aesthetics
#' are in bold).
#'
#' - `**xmin`**
#' - `**xmax`**
#' - `**relhum`**
#' - `**n`**
#' - `color`
#' - `size`
#' - `linetype`
#' - `alpha`
#'
#' `geom_relhum_grid` understands the following aesthetics (required aesthetics
#' are in bold).
#'
#' - **`step`**
#' - **`n`**
#' - `color`
#' - `color.sat`
#' - `size`
#' - `size.sat`
#' - `linetype`
#' - `linetype.sat`
#' - `alpha`
#' - `alpha.sat`
#'
#' Draw relative humidity lines
#'
#' @name geom_relhum
#' @export
geom_relhum_line <- function (mapping = NULL, data = NULL, n = 201,
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {

    layer(
        data = data,
        mapping = mapping,
        stat = StatRelHum,
        geom = GeomLine,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            n = n,
            na.rm = na.rm,
            ...
        )
    )
}
# }}}

# geom_relhum_grid {{{
#' @name geom_relhum
#' @export
geom_relhum_grid <- function (mapping = NULL, data = NULL, n = 201, ..., na.rm = FALSE, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = "identity", geom = GeomRelHumGrid,
          position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...)
    )
}
# }}}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @importFrom checkmate assert_count
#' @export
# GeomRelHumGrid {{{
GeomRelHumGrid <- ggproto("GeomRelHumGrid", GeomLine,
    setup_data = function (data, params) {
        # reset group
        data$group <- -1L

        # only keep unique
        data <- unique(data[c("PANEL", "group", "pres", "units")])

        # temporarily set x and y
        data$x <- 0
        data$y <- 0

        GeomLine$setup_data(data, params)
    },

    draw_panel = function(self, data, panel_params, coord) {
        # get units
        units <- get_units(data)

        # check pressure
        pres <- get_pres(data)

        # check n
        n <- unique(data$n)
        assert_count(n, positive = TRUE)

        # get relative humidity
        rh <- seq(0.0, 1.0, data$step)

        # create new data
        d <- do.call(rbind, replicate(length(rh), data, simplify = FALSE))

        d$rh <- rh

        # each relative humidity is a single group
        d$group <- seq_along(rh)

        # update panel params for saturation line
        d$colour[length(rh)] <- d$colour.sat[length(rh)]
        d$size[length(rh)] <- d$size.sat[length(rh)]
        d$linetype[length(rh)] <- d$linetype.sat[length(rh)]
        d$alpha[length(rh)] <- d$alpha.sat[length(rh)]
        d$colour.sat <- NULL
        d$size.sat <- NULL
        d$linetype.sat <- NULL
        d$alpha.sat <- NULL

        # get coord ranges
        ranges <- coord$backtransform_range(panel_params)

        # get x based on axis x limits
        x <- seq(ranges$x[[1L]], ranges$x[[2L]], length.out = n)

        # create new data
        data <- do.call(rbind, replicate(length(x), d, simplify = FALSE))
        data$x <- rep(x, each = length(rh))

        # calculate hum ratio at each relative humidity
        data$y <- amplify_hum(with_units(units, GetHumRatioFromRelHum(data$x, data$rh, pres)), units)

        # now can safely remove helpers
        data$rh <- NULL
        data$pres <- NULL
        data$units <- NULL
        data$step <- NULL
        data$n <- NULL

        GeomLine$draw_panel(data, panel_params, coord)
    },

    required_aes = c("step", "n", "pres", "units"),

    default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,
        colour.sat = "black", size.sat = 1.5, linetype.sat = 1, alpha.sat = NA
    )
)
# }}}
