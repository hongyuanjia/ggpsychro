#' @include utils.R
#' @include ggpsychro.R
NULL

# empty_trans {{{
empty_trans <- function () {
    trans_new("empty", "force", "force", breaks = identity, minor_breaks = identity)
}
# }}}
# is.empty_trans {{{
is.empty_trans <- function (trans) {
    trans$name == "empty"
}
# }}}

#' Create transformation objects for psychrometric chart
#'
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#'
#' @rdname trans
#' @importFrom scales trans_new extended_breaks regular_minor_breaks
#' @export
# drybulb_trans {{{
drybulb_trans <- function (units) {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("drybulb", "force", "force",
        breaks = extended_breaks(10),
        domain = get_tdb_limits(units)
    )
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
        breaks = extended_breaks(10),
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
    trans_new("relhum", "force", "force",
        domain = c(0.0, 1.0),
        breaks = extended_breaks(8),
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
        breaks = extended_breaks(8),
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
        breaks = extended_breaks(8),
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
        breaks = extended_breaks(8),
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
    trans_new("enthalpy", "force", "force",
        breaks = extended_breaks(8),
        format = label_enthalpy(units = units)
    )
}
# }}}
