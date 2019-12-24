#' Label wet-bulb temperature
#'
#' Format numbers as main variables on the psychrometric chart.
#'
#' @param x A numeric vector
#' @param A single string indcating the unit system to use. Should be either
#'        `"SI"` or `"IP"`
#' @inherit scales::number_format params return
#'
#' @examples
#' # for labelling dry-bulb temperature
#' demo_scale(10:50, labels = label_drybulb(units = "SI"))
#' demo_scale(10:50, labels = label_drybulb(units = "IP"))
#'
#' # for labelling humidity ratio
#' demo_scale(10:20, labels = label_humratio(scale = 0.001, units = "SI"))
#' demo_scale(10:20, labels = label_humratio(scale = 0.007, units = "IP"))
#'
#' demo_scale(10:50, labels = label_relhum(units = "SI"))
#' demo_scale(10:50, labels = label_relhum(units = "IP"))
#'
#' demo_scale(10:50, labels = label_wetbulb(units = "SI"))
#' demo_scale(10:50, labels = label_wetbulb(units = "IP"))
#'
#' demo_scale(10:50, labels = label_specvol(units = "SI"))
#' demo_scale(10:50, labels = label_specvol(units = "IP"))
#'
#' demo_scale(10:50, labels = label_vappres(units = "SI"))
#' demo_scale(10:50, labels = label_vappres(units = "IP"))
#'
#' demo_scale(seq(1000, 2000), labels = label_enthalpy(units = "SI"))
#' demo_scale(seq(1000, 2000), labels = label_enthalpy(units = "IP"))
#'
#' @rdname label
#' @export
# label_drybulb {{{
label_drybulb <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "dry_bulb", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
# label_humratio {{{
label_humratio <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "hum_ratio", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
# label_relhum {{{
label_relhum <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "rel_hum", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
# label_wetbulb {{{
label_wetbulb <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "wet_bulb", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
# label_vappres {{{
label_vappres <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "vap_pres", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
# label_specvol {{{
label_specvol <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "spec_vol", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
# label_enthalpy {{{
label_enthalpy <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "enthalpy", units = units,
        big.mark, decimal.mark, trim, ...)
}
# }}}

#' @rdname label
#' @export
drybulb_format <- label_drybulb

#' @rdname label
#' @export
humratio_format <- label_humratio

#' @rdname label
#' @export
relhum_format <- label_relhum

#' @rdname label
#' @export
wetbulb_format <- label_wetbulb

#' @rdname label
#' @export
vappres_format <- label_vappres

#' @rdname label
#' @export
specvol_format <- label_specvol

#' @rdname label
#' @export
enthalpy_format <- label_enthalpy

# label_unit {{{
#' @importFrom scales label_parse number
label_unit <- function (x, accuracy = NULL, scale = 1, type, units,
                        big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
    force_all(
        accuracy,
        scale,
        units,
        big.mark,
        decimal.mark,
        trim,
        ...
    )

    units <- match.arg(units, c("SI", "IP"))

    prefix <- get_prefix(type)
    suffix <- paste0(" ", get_unit(units, type))

    if (need_parse(type)) {
        prefix <- paste0("'", prefix, " '*")
        suffix <- paste("*~", suffix)
        big.mark <- paste0("*'", big.mark, "'*")
        decimal.mark <- paste0("*'", decimal.mark, "'*")
    }

    function (x) {
        num <- number(x, accuracy = accuracy, scale = scale,
            prefix = "", suffix = suffix,
            big.mark = big.mark, decimal.mark = decimal.mark,
            trim = trim, ...)

        if (need_parse(type)) {
            num <- gsub("(\\d+)", "'\\1'", num, perl = TRUE)
        }

        num[[1L]] <- paste(prefix, num[[1L]])

        if (need_parse(type)) num <- label_parse()(num)

        num
    }
}
# }}}

# get_unit {{{
get_unit <- function (unit, type) {
    .unit_list[[unit]][[type]]
}
# }}}

# get_prefix {{{
get_prefix <- function (type) {
    gsub("_", " ", gsub("^(\\w)", "\\U\\1", type, perl = TRUE))
}
# }}}

# need_parse {{{
need_parse <- function (type) {
    .unit_list$parse[[type]]
}
# }}}

# .unit_list {{{
.unit_list <- list(
    SI = list(
        dry_bulb = "degree * C",
        hum_ratio = "g[m] * ' / ' * kg[da]",
        rel_hum = "%",
        wet_bulb = "degree * C",
        vap_pres = "kPa",
        spec_vol = "m^3 * ' / ' * kg",
        enthalpy = "kJ * ' / ' * kg"
    ),
    IP = list(
        dry_bulb = "degree * C",
        hum_ratio = "gr[m] * ' / ' * lb[da]",
        rel_hum = "%",
        wet_bulb = "degree * F",
        vap_pres = "kPsi",
        spec_vol = "ft^3 * ' / ' * lb",
        enthalpy = "Btu * ' / ' * lb"
    ),
    parse = list(
        dry_bulb = TRUE,
        hum_ratio = TRUE,
        rel_hum = FALSE,
        wet_bulb = TRUE,
        vap_pres = FALSE,
        spec_vol = TRUE,
        enthalpy = TRUE
    )
)
# }}}

# force_all {{{
# reference r-lib/scales/R/utils.r
force_all <- function (...) list(...)
# }}}

# demo_scale {{{
# adopted from scales::demo_continuous
#' Demonstrate scales functions with ggplot2 code
#'
#' This function generates ggplot2 code needed to use scales functions for real
#' code.
#'
#' @param x A vector of data
#' @param ... Other arguments pass to scale functions
#'
#' @export
demo_scale <- function (x, ...) {
    df <- data.frame(x = x, stringsAsFactors = FALSE)
    ggplot2::ggplot(df, ggplot2::aes(x, 1)) +
        ggplot2::geom_blank() +
        ggplot2::scale_x_continuous(NULL, ...) +
        ggplot2::scale_y_continuous(NULL, breaks = NULL) +
        ggplot2::theme(aspect.ratio = 1 / 5)
}
# }}}
