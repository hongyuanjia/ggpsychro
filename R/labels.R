default_labs <- function(units = "SI", mollier = FALSE) {
    if (units == "SI") {
        lab_x <- expression("Dry-bulb temperature (" * degree * C * ")")
        lab_y <- expression("Humidity ratio (" * g[m] * "/" * kg[da] * ")")
    } else if (units == "IP") {
        lab_x <- expression("Dry-bulb temperature (" * degree * F * ")")
        lab_y <- expression("Humidity ratio (" * gr[m] * "/" * lb[da] * ")")
    }

    if (mollier) {
        list(x = lab_y, y = lab_x)
    } else {
        list(x = lab_x, y = lab_y)
    }
}

#' Label psychrometric scale breaks
#'
#' Format numbers as main variables on the psychrometric chart.
#'
#' @param units A single string indicating the unit system to use. Should be either
#'        `"SI"` or `"IP"`
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'        displayed as described in `?plotmath`. Default: `FALSE`.
#' @inherit scales::number_format params return
#' @return A labelling function that formats numeric breaks.
#'
#' @examples
#' demo_scale(10:50, labels = label_drybulb(units = "SI", parse = TRUE))
#' demo_scale(10:50, labels = label_drybulb(units = "IP", parse = TRUE))
#'
#' demo_scale(10:20, labels = label_humratio(units = "SI", parse = TRUE))
#' demo_scale(70:140, labels = label_humratio(units = "IP", parse = TRUE))
#'
#' demo_scale(seq(0.1, 0.5, by = 0.1), labels = label_relhum(units = "SI"))
#' demo_scale(seq(0.1, 0.5, by = 0.1), labels = label_relhum(units = "IP"))
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
label_drybulb <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "drybulb",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

#' @rdname label
#' @export
label_humratio <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "humratio",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

#' @rdname label
#' @export
label_relhum <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "relhum",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

#' @rdname label
#' @export
label_wetbulb <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "wetbulb",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

#' @rdname label
#' @export
label_vappres <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "vappres",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

#' @rdname label
#' @export
label_specvol <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "specvol",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

#' @rdname label
#' @export
label_enthalpy <- function(
    accuracy = NULL,
    scale = 1,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__unit(
        accuracy = accuracy,
        scale = scale,
        type = "enthalpy",
        units = units,
        big.mark,
        decimal.mark,
        trim,
        parse,
        ...
    )
}

# Format psychrometric scale breaks with property prefixes and unit suffixes.
#' @importFrom scales number
label__unit <- function(
    accuracy = NULL,
    scale = 1,
    type,
    units,
    big.mark = ",",
    decimal.mark = ".",
    trim = TRUE,
    parse = FALSE,
    ...
) {
    label__force_all(
        accuracy,
        scale,
        units,
        big.mark,
        decimal.mark,
        trim,
        ...
    )

    units <- match.arg(units, c("SI", "IP"))

    prefix <- label__prefix(type)
    suffix <- paste0(" ", label__unit_suffix(units, type))
    if (parse && suffix == " %") {
        suffix <- paste0("'", suffix, "'")
    }

    fmt_big_mark <- big.mark
    fmt_decimal_mark <- decimal.mark
    if (parse || label__needs_parse(type)) {
        prefix <- paste0("'", prefix, " '*")
        suffix <- paste("*~", suffix)
    }

    function(x) {
        if (type == "relhum") {
            x <- x * 100.0
        }
        if (type == "vappres" && units == "SI") {
            x <- x / 1000.0
        }
        if (type == "enthalpy") {
            x <- x / 1000.0
        }
        num <- number(
            x,
            accuracy = accuracy,
            scale = scale,
            prefix = "",
            suffix = suffix,
            big.mark = fmt_big_mark,
            decimal.mark = fmt_decimal_mark,
            trim = trim,
            ...
        )

        if (parse || label__needs_parse(type)) {
            if (nzchar(fmt_big_mark)) {
                num <- gsub(
                    fmt_big_mark,
                    paste0("*'", fmt_big_mark, "'*"),
                    num,
                    fixed = TRUE
                )
            }
            if (nzchar(fmt_decimal_mark)) {
                num <- gsub(
                    fmt_decimal_mark,
                    paste0("*'", fmt_decimal_mark, "'*"),
                    num,
                    fixed = TRUE
                )
            }
        }

        if (label__needs_parse(type)) {
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

# Look up the unit suffix for a psychrometric property and unit system.
label__unit_suffix <- function(unit, type) {
    GGPSY_UNIT_SPECS[[unit]][[type]]
}

# Build the text prefix used by guide and axis labels.
label__prefix <- function(type) {
    if (type == "relhum") {
        "RH"
    } else {
        pre <- gsub("_", " ", gsub("_(\\w)", "\\U\\1", type, perl = TRUE))
        gsub("^(\\w)", "\\U\\1", pre, perl = TRUE)
    }
}

# Test whether a property label needs plotmath parsing by default.
label__needs_parse <- function(type) {
    GGPSY_UNIT_SPECS$parse[[type]]
}

# Force lazy formatter arguments when creating the returned labelling function.
label__force_all <- function(...) list(...)

#' Demonstrate psychrometric label and scale functions
#'
#' This helper builds a compact ggplot2 scale preview for label and scale
#' functions.
#'
#' @param x A vector of data
#' @param ... Other arguments pass to scale functions
#' @return A ggplot object demonstrating the supplied scale settings.
#'
#' @examples
#' demo_scale(0:10, labels = scales::label_number())
#'
#' @export
# adopted from scales::demo_continuous
demo_scale <- function(x, ...) {
    df <- data.frame(x = x, stringsAsFactors = FALSE)
    ggplot2::ggplot(df, ggplot2::aes(x, 1)) +
        ggplot2::geom_blank() +
        ggplot2::scale_x_continuous(NULL, ...) +
        ggplot2::scale_y_continuous(NULL, breaks = NULL) +
        ggplot2::theme(aspect.ratio = 1 / 5)
}
