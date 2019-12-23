#' @include utils.R
#' @include ggpsychro.R
NULL

#' Create transformation objects for psychrometric chart
#'
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#'
#' @rdname trans
#' @importFrom scales trans_new
#' @export
# drybulb_trans {{{
drybulb_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("drybulb", "force", "force", domain = get_tdb_limits(units))
}
# }}}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
# humratio_trans {{{
humratio_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("humratio",
        transform = function(hum) narrow_hum(hum, units),
        inverse = function (hum) amplify_hum(hum, units),
        domain = get_hum_limits(units),
    )
}
# }}}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
# relhum_trans {{{
relhum_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("relhum",
        transform = function (x) x * 100.0,
        inverse = function (x) x / 100.0,
        domain = c(-100.0, 100.0),
        format = function (x) {
            x[1L] <- paste("RH", x[1L], "%")
            x[-1L] <- paste(x[-1L], "%")
            x
        }
    )
}
# }}}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
# wetbulb_trans {{{
wetbulb_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("wetbulb", "force", "force",
        domain = get_tdb_limits(units),
        format = function (x) {
            symbol <- switch(units, SI = expression(degree*C), IP = expression(degree*F))
            x[1L] <- paste0("Wet-bulb ~", x[1L], "*", symbol)
            x[-1L] <- paste0(x[-1L], "*", symbol)
            x
        }
    )
}
# }}}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
# vappres_trans {{{
vappres_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("vappres", "force", "force",
        format = function (x) {
            x <- sprintf("%.2f", x / 1000.0)
            if (units == "SI") {
                x[1L] <- paste("Vapor Pressure", x[1L], "kPa")
                x[-1L] <- paste(x[-1L], "kPa")
            } else {
                x[1L] <- paste(x[1L], "kPsi")
                x[-1L] <- paste(x[-1L], "kPsi")
            }
            x
        }
    )
}
# }}}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
# specvol_trans {{{
specvol_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("specvol", "force", "force",
        format = function (x) {
            x <- sprintf("%.2f", x)
            if (units == "SI") {
                x[1L] <- paste0("'Specific vol'*", x[1L], "* m^3 / kg")
                x[-1L] <- paste0(x[-1L], " * m^3 / kg")
            } else {
                x[1L] <- paste0("'Specific vol'*", x[1L], "* ft^3 / lb")
                x[-1L] <- paste0(x[-1L], " * ft^3 / lb")
            }
        }
    )
}
# }}}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
# enthalpy_trans {{{
enthalpy_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("enthalpy",
        transform = function (x) narrow_enth(x, units),
        inverse = function (x) amplify_hum(x, units),
        format = function (x) {
            x <- round(x, digits = 2)
            if (units == "SI") {
                x[1L] <- paste0("Enthalpy ", x[1L], " kJ / kg")
                x[-1L] <- paste0(x[-1L], " KJ / kg")
            } else {
                x[1L] <- paste0("Enthalpy ", x[1L], " Btu / lb")
                x[-1L] <- paste0(x[-1L], " Btu / lb")
            }
            x
        }
    )
}
# }}}
