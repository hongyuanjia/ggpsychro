#' @include stat.R psychro-extra.R utils.R
NULL

#' Draw psychrometric state points or process lines
#'
#' `stat_psychro_state()` converts a dry-bulb temperature plus exactly one
#' psychrometric state property to chart coordinates. `geom_psychro_process()`
#' uses the same conversion and draws the states as a path.
#'
#' @details
#' The `tdb` aesthetic is required. Exactly one of `humratio`, `relhum`,
#' `wetbulb`, `vappres`, `specvol`, or `enthalpy` must also be mapped.
#'
#' `relhum` is supplied in percent. `humratio` is supplied in chart display
#' units: g/kg in SI and gr/lb in IP.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @rdname psychro_state
#' @export
geom_psychro_process <- function(mapping = NULL, data = NULL, stat = "psychro_state",
                                 position = "identity", ..., na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = stat, data = data, mapping = mapping, geom = "path",
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @rdname psychro_state
#' @export
stat_psychro_state <- function(mapping = NULL, data = NULL, geom = "point",
                               position = "identity", ..., na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatPsychroState, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

psychro_state_properties <- function() {
    c("humratio", "relhum", "wetbulb", "vappres", "specvol", "enthalpy")
}

psychro_state_property <- function(data) {
    props <- psychro_state_properties()
    present <- props[props %in% names(data)]

    if (length(present) == 0L) {
        stop(
            "One psychrometric state aesthetic must be supplied: ",
            paste(props, collapse = ", "),
            call. = FALSE
        )
    }
    if (length(present) > 1L) {
        stop(
            "Only one psychrometric state aesthetic can be supplied. Found: ",
            paste(present, collapse = ", "),
            call. = FALSE
        )
    }

    present
}

psychro_check_finite <- function(data, vars, na.rm = FALSE) {
    keep <- stats::complete.cases(data[vars])
    keep <- keep & Reduce(`&`, lapply(data[vars], is.finite))

    if (all(keep)) {
        return(data)
    }
    if (isTRUE(na.rm)) {
        return(data[keep, , drop = FALSE])
    }

    bad <- vars[vapply(data[vars], function(x) any(!is.finite(x) | is.na(x)), logical(1L))]
    stop("Missing or non-finite psychrometric values found in: ",
        paste(bad, collapse = ", "), call. = FALSE)
}

psychro_check_relhum_fraction <- function(relhum) {
    if (any(relhum < 0 | relhum > 1, na.rm = TRUE)) {
        stop("`relhum` must be in the range [0, 100].", call. = FALSE)
    }
}

psychro_check_relhum_percent <- function(relhum) {
    if (any(relhum < 0 | relhum > 100, na.rm = TRUE)) {
        stop("`relhum` limits must be in the range [0, 100].", call. = FALSE)
    }
}

psychro_humratio_from_property <- function(tdb, value, property, units, pres) {
    with_units(units, switch(property,
        humratio = narrow_hum(value, units),
        relhum = {
            psychro_check_relhum_fraction(value)
            psychrolib::GetHumRatioFromRelHum(tdb, value, pres)
        },
        wetbulb = psychrolib::GetHumRatioFromTWetBulb(tdb, value, pres),
        vappres = psychrolib::GetHumRatioFromVapPres(value, pres),
        specvol = GetHumRatioFromAirVolume(tdb, value, pres),
        enthalpy = GetHumRatioFromEnthalpyAndTDryBulb(value, tdb),
        stop("Invalid psychrometric state property.", call. = FALSE)
    ))
}

psychro_output_xy <- function(data, tdb, humratio, mollier = FALSE) {
    if (isTRUE(mollier)) {
        data$x <- humratio
        data$y <- tdb
    } else {
        data$x <- tdb
        data$y <- humratio
    }

    data
}

psychro_compute_state <- function(data, units, pres, mollier, na.rm = FALSE) {
    if (!"tdb" %in% names(data)) {
        stop("`tdb` must be supplied.", call. = FALSE)
    }

    property <- psychro_state_property(data)
    data <- psychro_check_finite(data, c("tdb", property), na.rm = na.rm)
    if (!nrow(data)) {
        return(data)
    }

    humratio <- psychro_humratio_from_property(
        data$tdb, data[[property]], property, units, pres
    )
    data$humratio <- humratio
    data <- psychro_check_finite(data, "humratio", na.rm = na.rm)

    if (!nrow(data)) {
        return(data)
    }

    psychro_output_xy(data, data$tdb, data$humratio, mollier)
}

#' @rdname ggpsychro-extensions
#' @format NULL
#' @usage NULL
#' @export
StatPsychroState <- ggplot2::ggproto(
    "StatPsychroState", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    required_aes = c("tdb"),

    optional_aes = psychro_state_properties(),

    extra_params = c("na.rm", "units", "pres", "mollier"),

    compute_group = function(self, data, scales, units, pres, mollier = FALSE,
                             na.rm = FALSE) {
        psychro_compute_state(data, units, pres, mollier, na.rm)
    }
)
