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
        format = label_relhum(units = units)
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
        format = label_wetbulb(units = units)
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
        format = label_vappres(units = units)
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
        format = label_specvol(units = units)
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
        format = label_enthalpy(units = units)
    )
}
# }}}
