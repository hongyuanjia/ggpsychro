#' @include utils.R
#' @include ggpsychro.R
NULL

empty_trans <- function() {
    trans_new("empty", "force", "force", breaks = identity, minor_breaks = identity)
}

is.empty_trans <- function(trans) {
    trans$name == "empty"
}

#' Create transformation objects for psychrometric chart
#'
#' @param units A string indicating the system of units chosen. Should be either
#'        `"SI"` or `"IP"`.
#'
#' @rdname trans
#' @importFrom scales trans_new extended_breaks regular_minor_breaks
#' @export
#' @examples
#' plot(drybulb_trans("SI"), xlim = c(0, 5))
#' plot(humratio_trans("SI"), xlim = c(0, 1000))
#' plot(relhum_trans("SI"), xlim = c(0, 1))
#' plot(wetbulb_trans("SI"), xlim = c(-50, 40))
#' plot(vappres_trans("SI"), xlim = c(1000, 4000))
#' plot(specvol_trans("SI"), xlim = c(0.8, 1))
drybulb_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("drybulb", "force", "force",
        domain = get_tdb_limits(units)
    )
}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
humratio_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("humratio",
        transform = function(hum) narrow_hum(hum, units),
        inverse = function(hum) amplify_hum(hum, units),
        domain = get_hum_limits(units),
    )
}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
relhum_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("relhum",
        function(x) x / 100,
        function(x) x * 100,
        domain = c(0.0, 1.0),
        format = label_relhum(units = units)
    )
}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
wetbulb_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("wetbulb", "force", "force",
        domain = get_tdb_limits(units),
        format = label_wetbulb(units = units)
    )
}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
vappres_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("vappres", "force", "force",
        format = label_vappres(units = units)
    )
}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
specvol_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("specvol", "force", "force",
        format = label_specvol(units = units)
    )
}

#' @rdname trans
#' @importFrom scales trans_new
#' @export
enthalpy_trans <- function(units = "SI") {
    units <- match.arg(units, c("SI", "IP"))
    trans_new("enthalpy", "force", "force",
        format = label_enthalpy(units = units)
    )
}
