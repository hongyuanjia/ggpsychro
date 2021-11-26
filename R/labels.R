default_labs <- function(units = "SI", mollier = FALSE) {
    if (units == "SI") {
        lab_x <- expression("Dry-bulb temperature ("*degree*C*")")
        lab_y <- expression("Humidity ratio ("*g[m]*"/"*kg[da]*")")
    } else if (units == "IP") {
        lab_x <- expression("Dry-bulb temperature ("*degree*F*")")
        lab_y <- expression("Humidity ratio ("*gr[m]*"/"*lb[da]*")")
    }

    if (mollier) {
        list(x = lab_y, y = lab_x)
    } else {
        list(x = lab_x, y = lab_y)
    }
}

#' Label wet-bulb temperature
#'
#' Format numbers as main variables on the psychrometric chart.
#'
#' @param x A numeric vector
#' @param units A single string indcating the unit system to use. Should be either
#'        `"SI"` or `"IP"`
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'        displayed as described in `?plotmath`. Default: `FALSE`.
#' @inherit scales::number_format params return
#'
#' @examples
#' demo_scale(10:50, labels = label_drybulb(units = "SI", parse = TRUE))
#' demo_scale(10:50, labels = label_drybulb(units = "IP", parse = TRUE))
#'
#' demo_scale(10:20, labels = label_humratio(scale = 0.001, units = "SI", parse = TRUE))
#' demo_scale(10:20, labels = label_humratio(scale = 0.007, units = "IP", parse = TRUE))
#'
#' demo_scale(10:50, labels = label_relhum(units = "SI"))
#' demo_scale(10:50, labels = label_relhum(units = "IP"))
#'
#' demo_scale(10:50, labels = label_wetbulb(units = "SI", parse = TRUE))
#' demo_scale(10:50, labels = label_wetbulb(units = "IP", parse = TRUE))
#'
#' demo_scale(10:50, labels = label_specvol(units = "SI", parse = TRUE))
#' demo_scale(10:50, labels = label_specvol(units = "IP", parse = TRUE))
#'
#' demo_scale(10:50, labels = label_vappres(units = "SI"))
#' demo_scale(10:50, labels = label_vappres(units = "IP"))
#'
#' demo_scale(seq(1000, 2000), labels = label_enthalpy(units = "SI", parse = TRUE))
#' demo_scale(seq(1000, 2000), labels = label_enthalpy(units = "IP", parse = TRUE))
#'
#' @rdname label
#' @export
label_drybulb <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "drybulb", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

#' @rdname label
#' @export
label_humratio <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "humratio", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

#' @rdname label
#' @export
label_relhum <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "relhum", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

#' @rdname label
#' @export
label_wetbulb <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "wetbulb", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

#' @rdname label
#' @export
label_vappres <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "vappres", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

#' @rdname label
#' @export
label_specvol <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "specvol", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

#' @rdname label
#' @export
label_enthalpy <- function(x, accuracy = NULL, scale = 1, units,
                         big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE, ...) {
    label_unit(x, accuracy = accuracy, scale = scale, type = "enthalpy", units = units,
        big.mark, decimal.mark, trim, parse, ...)
}

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

#' @importFrom scales number
label_unit <- function (x, accuracy = NULL, scale = 1, type, units,
                        big.mark = ",", decimal.mark = ".", trim = TRUE, parse = FALSE,
                        ...) {
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
    if (parse && suffix == " %") suffix <- paste0("'", suffix, "'")

    if (parse || need_parse(type)) {
        prefix <- paste0("'", prefix, " '*")
        suffix <- paste("*~", suffix)
        big.mark <- paste0("*'", big.mark, "'*")
        decimal.mark <- paste0("*'", decimal.mark, "'*")
    }

    function (x) {
        if (type == "relhum") x <- x * 100.0
        if (type == "vappres") x <- x / 1000.0
        if (type == "enthalpy") x <- x / 1000.0
        num <- number(x, accuracy = accuracy, scale = scale,
            prefix = "", suffix = suffix,
            big.mark = big.mark, decimal.mark = decimal.mark,
            trim = trim, ...)

        if (need_parse(type)) {
            num <- gsub("(\\d+)", "'\\1'", num, perl = TRUE)
        }

        num[1] <- paste(prefix, num[1])

        if (parse) {
            parse(text = num)
        } else {
            num
        }
    }
}

get_unit <- function (unit, type) {
    .unit_list[[unit]][[type]]
}

get_prefix <- function (type) {
    if (type == "relhum") {
        "RH"
    } else {
        pre <- gsub("_", " ", gsub("_(\\w)", "\\U\\1", type, perl = TRUE))
        gsub("^(\\w)", "\\U\\1", pre, perl = TRUE)
    }
}

need_parse <- function (type) {
    .unit_list$parse[[type]]
}

# reference r-lib/scales/R/utils.r
force_all <- function (...) list(...)

#' Demonstrate scales functions with ggplot2 code
#'
#' This function generates ggplot2 code needed to use scales functions for real
#' code.
#'
#' @param x A vector of data
#' @param ... Other arguments pass to scale functions
#'
#' @keywords internal
#' @export
# adopted from scales::demo_continuous
demo_scale <- function (x, ...) {
    df <- data.frame(x = x, stringsAsFactors = FALSE)
    ggplot2::ggplot(df, ggplot2::aes(x, 1)) +
        ggplot2::geom_blank() +
        ggplot2::scale_x_continuous(NULL, ...) +
        ggplot2::scale_y_continuous(NULL, breaks = NULL) +
        ggplot2::theme(aspect.ratio = 1 / 5)
}
