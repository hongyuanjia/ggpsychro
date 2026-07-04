#' Calculate psychrometric properties of moist air
#'
#' @details
#'
#' * `stat_relhum` requires an extra `relhum` aesthetics for relative humidity
#'   in range \[0, 100\] in %
#' * `stat_wetbulb` requires an extra `wetbulb` aesthetics for wet-bulb
#'   temperature in degree_F \[IP\] or degree_C \[SI\]
#' * `stat_vappres` requires an extra `vappres` aesthetics for partial pressure
#'   of water vapor in moist air in Psi \[IP\] or Pa \[SI\]
#' * `stat_specvol` requires an extra `specvol` aesthetics for specific volume
#'   of moist air in ft3 lb-1 of dry air \[IP\] or in m3 kg-1 of dry air \[SI\]
#' * `stat_enthalpy` requires an extra `enthalpy` aesthetics for moist air
#'   enthalpy in Btu lb-1 \[IP\] or J kg-1
#'
#' What these [ggplot2::ggproto()] objects do are to take input values,
#' calculate the corresponding humidity ratio and replace the `y` aesthetic
#' values in each group.
#'
#' All of stats above requires two additional aesthetics:
#'
#' * `units`: A single string indicating the unit system to use. Should be
#'   either `"SI"` or `"IP"`, or `waiver()` which uses the value from the
#'   parent plot. Default: `waiver()`
#'
#' * `pres`: A single number indicating the atmosphere pressure in Pa \[SI\] or
#'   Psi \[IP\]. If `waiver()`, the pressure calculated from the parent plot's
#'   altitude value will be used. Default: `waiver()`
#'
#' However, when these stats are used inside a ggplot `geom_*` as the `stat`
#' argument, both `units` and `pres` have to be specified.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @return A ggplot layer.
#' @importFrom ggplot2 ggproto Stat Geom
#' @importFrom psychrolib GetHumRatioFromRelHum
#' @importFrom psychrolib GetHumRatioFromTWetBulb
#' @importFrom psychrolib GetHumRatioFromVapPres
#' @importFrom psychrolib GetHumRatioFromEnthalpyAndTDryBulb
#' @rdname stat
#' @examples
#' states <- data.frame(
#'     tdb = c(18, 22, 26, 30),
#'     relhum = c(70, 55, 45, 35)
#' )
#'
#' ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
#'     stat_relhum(aes(x = tdb, relhum = relhum), data = states)
#'
#' wetbulb_line <- data.frame(tdb = 18:30, wetbulb = 16)
#' ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
#'     geom_grid_wetbulb() +
#'     stat_wetbulb(aes(x = tdb, wetbulb = wetbulb),
#'         data = wetbulb_line, geom = "line")
#'
#' # The stats can also be used from ordinary ggplot2 geoms.
#' ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
#'     geom_grid_wetbulb() +
#'     geom_line(aes(x = tdb, wetbulb = wetbulb),
#'         data = wetbulb_line, stat = "wetbulb")
#'
#' vapour_pressure <- data.frame(
#'     tdb = c(12, 18, 24, 30),
#'     vappres = c(900, 1200, 1800, 2400)
#' )
#' ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
#'     stat_vappres(aes(x = tdb, vappres = vappres),
#'         data = vapour_pressure)
#'
#' specific_volume <- data.frame(
#'     tdb = c(20, 25, 30),
#'     specvol = c(0.84, 0.86, 0.88)
#' )
#' ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
#'     stat_specvol(aes(x = tdb, specvol = specvol),
#'         data = specific_volume)
#'
#' enthalpy <- data.frame(
#'     tdb = c(18, 24, 30),
#'     enthalpy = c(35000, 50000, 65000)
#' )
#' ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25)) +
#'     stat_enthalpy(aes(x = tdb, enthalpy = enthalpy), data = enthalpy)
#'
#' @export
stat_relhum <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    psychro_layer(
        stat = StatRelhum,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @export
#' @rdname stat
stat_wetbulb <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    psychro_layer(
        stat = StatWetbulb,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @export
#' @rdname stat
stat_vappres <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    psychro_layer(
        stat = StatVappres,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @export
#' @rdname stat
stat_specvol <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    psychro_layer(
        stat = StatSpecvol,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @export
#' @rdname stat
stat_enthalpy <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    psychro_layer(
        stat = StatEnthalpy,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

init_stat_data <- function(data, params) {
    if (!"units" %in% names(data)) {
        data$units <- encode_units(params$units)
    } else {
        data$units <- encode_units(unique(data$units))
    }

    if (!"pres" %in% names(data)) {
        data$pres <- params$pres
    }

    data
}

# Resolve either a raw ggplot2 scale or a panel view scale to the underlying
# scale object so stat and coord code can share the same transform helpers.
psychro_scale_object <- function(scale) {
    if (is.null(scale)) {
        return(NULL)
    }
    scale$scale %||% scale
}

# Invert values that ggplot2 has already moved into scale space before stat
# computation; psychrolib expects physical/public units instead.
psychro_scale_inverse <- function(scale, x) {
    scale <- psychro_scale_object(scale)
    if (is.null(scale) || is.null(x)) {
        return(x)
    }
    scale$trans$inverse(x)
}

# Apply a ggplot2 scale transform while quietly dropping values that the
# transform itself cannot represent, such as negative humidity on a log scale.
psychro_scale_transform <- function(scale, x) {
    scale <- psychro_scale_object(scale)
    if (is.null(scale) || is.null(x)) {
        return(x)
    }
    out <- suppressWarnings(scale$transform(x))
    out[!is.finite(out)] <- NA_real_
    out
}

# Only columns actually owned by a scale have been transformed by ggplot2. Zone
# bounds such as `relhum_min` remain public values unless a scale lists them.
psychro_scale_has_aesthetic <- function(scale, aesthetic) {
    scale <- psychro_scale_object(scale)
    !is.null(scale) && aesthetic %in% scale$aesthetics
}

# Summarize a scale for cache keys without retaining the ggproto object or its
# enclosing environments.
psychro_scale_cache_key <- function(scale) {
    scale <- psychro_scale_object(scale)
    if (is.null(scale)) {
        return(NULL)
    }

    trans <- scale$trans
    list(
        aesthetics = scale$aesthetics,
        scale_name = scale$scale_name %||% NULL,
        trans_name = trans$name %||% class(trans)[[1L]],
        trans_domain = trans$domain
    )
}

# Keep all psychrometric scales that may transform data before a stat runs; the
# x/y entries are the actual chart position scales used for final coordinates.
psychro_stat_scale_context <- function(scales, psychro) {
    if (is.null(scales)) {
        return(NULL)
    }
    pos_tdb <- if (isTRUE(psychro$mollier)) "y" else "x"
    pos_hum <- if (isTRUE(psychro$mollier)) "x" else "y"

    list(
        x = scales$get_scales("x"),
        y = scales$get_scales("y"),
        pos_tdb = scales$get_scales(pos_tdb),
        pos_hum = scales$get_scales(pos_hum),
        tdb = scales$get_scales("tdb") %||% scales$get_scales(pos_tdb),
        humratio = scales$get_scales("humratio") %||%
            scales$get_scales(pos_hum),
        relhum = scales$get_scales("relhum"),
        wetbulb = scales$get_scales("wetbulb"),
        vappres = scales$get_scales("vappres"),
        specvol = scales$get_scales("specvol"),
        enthalpy = scales$get_scales("enthalpy")
    )
}

# Public relative-humidity inputs are percentages; after inverse-transforming a
# scale value, convert once to the 0-1 fraction required by psychrolib.
psychro_stat_relhum_fraction <- function(
    relhum,
    psychro_scales = NULL,
    aesthetic = "relhum"
) {
    if (psychro_scale_has_aesthetic(psychro_scales$relhum, aesthetic)) {
        relhum <- psychro_scale_inverse(psychro_scales$relhum, relhum)
    }
    psychro_check_relhum_percent(relhum)
    relhum / 100
}

# Inverse-transform one psychrometric property column from scale space into the
# public unit documented for that aesthetic.
psychro_stat_inverse_property <- function(
    x,
    property,
    psychro_scales = NULL,
    aesthetic = property
) {
    scale <- psychro_scales[[property]]
    if (psychro_scale_has_aesthetic(scale, aesthetic)) {
        return(psychro_scale_inverse(scale, x))
    }
    x
}

# Inverse-transform all known psychrometric columns that are present in a data
# frame before downstream helpers do psychrolib math.
psychro_stat_inverse_columns <- function(data, psychro_scales = NULL) {
    scale_for <- c(
        tdb = "tdb",
        tdb_min = "tdb",
        tdb_max = "tdb",
        humratio = "humratio",
        humratio_min = "humratio",
        humratio_max = "humratio",
        relhum = "relhum",
        relhum_min = "relhum",
        relhum_max = "relhum",
        wetbulb = "wetbulb",
        vappres = "vappres",
        specvol = "specvol",
        specvol_min = "specvol",
        specvol_max = "specvol",
        enthalpy = "enthalpy",
        enthalpy_min = "enthalpy",
        enthalpy_max = "enthalpy"
    )
    for (var in intersect(names(scale_for), names(data))) {
        data[[var]] <- psychro_stat_inverse_property(
            data[[var]],
            scale_for[[var]],
            psychro_scales,
            aesthetic = var
        )
    }
    data
}

# Humidity ratios leave psychrolib in native kg/kg or lb/lb units; convert back
# to the active chart scale before ggplot2 trains and maps the y position.
psychro_stat_scale_humratio <- function(humratio, units, scale) {
    psychro_scale_transform(scale, amplify_hum(humratio, units))
}

finish_stat_humratio <- function(data, humratio, units, scales) {
    ys <- names(data)[names(data) %in% GGPSY_OPT$y_aes]

    if (!length(ys)) {
        ys <- "y"
    }

    humratio <- psychro_stat_scale_humratio(humratio, units, scales$y)
    for (var in ys) {
        data[[var]] <- humratio
    }

    data
}

# Internal ggproto backing stat_relhum(); the user-facing API is stat_relhum().
StatRelhum <- ggproto(
    "StatRelhum",
    Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label", "psychro_scales"),

    required_aes = c("x", "relhum", "pres", "units"),

    compute_group = function(self, data, scales, psychro_scales = NULL) {
        units <- get_units(data)
        tdb <- psychro_scale_inverse(scales$x, data$x)
        relhum <- psychro_stat_relhum_fraction(data$relhum, psychro_scales)
        humratio <- with_units(
            units,
            GetHumRatioFromRelHum(tdb, relhum, data$pres)
        )
        finish_stat_humratio(data, humratio, units, scales)
    }
)

# Internal ggproto backing stat_wetbulb(); the user-facing API is stat_wetbulb().
StatWetbulb <- ggproto(
    "StatWetbulb",
    Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label", "psychro_scales"),

    required_aes = c("x", "wetbulb", "pres", "units"),

    compute_group = function(self, data, scales, psychro_scales = NULL) {
        units <- get_units(data)
        tdb <- psychro_scale_inverse(scales$x, data$x)
        wetbulb <- psychro_stat_inverse_property(
            data$wetbulb,
            "wetbulb",
            psychro_scales
        )
        humratio <- with_units(
            units,
            GetHumRatioFromTWetBulb(tdb, wetbulb, data$pres)
        )
        finish_stat_humratio(data, humratio, units, scales)
    }
)

# Internal ggproto backing stat_vappres(); the user-facing API is stat_vappres().
StatVappres <- ggproto(
    "StatVappres",
    Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label", "psychro_scales"),

    required_aes = c("x", "vappres", "pres", "units"),

    compute_group = function(self, data, scales, psychro_scales = NULL) {
        units <- get_units(data)
        vappres <- psychro_stat_inverse_property(
            data$vappres,
            "vappres",
            psychro_scales
        )
        humratio <- with_units(
            units,
            GetHumRatioFromVapPres(vappres, data$pres)
        )
        finish_stat_humratio(data, humratio, units, scales)
    }
)

# Internal ggproto backing stat_specvol(); the user-facing API is stat_specvol().
StatSpecvol <- ggproto(
    "StatSpecvol",
    Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label", "psychro_scales"),

    required_aes = c("x", "specvol", "pres", "units"),

    compute_group = function(self, data, scales, psychro_scales = NULL) {
        units <- get_units(data)
        tdb <- psychro_scale_inverse(scales$x, data$x)
        specvol <- psychro_stat_inverse_property(
            data$specvol,
            "specvol",
            psychro_scales
        )
        humratio <- with_units(
            units,
            GetHumRatioFromAirVolume(tdb, specvol, data$pres)
        )
        finish_stat_humratio(data, humratio, units, scales)
    }
)

# Internal ggproto backing stat_enthalpy(); the user-facing API is stat_enthalpy().
StatEnthalpy <- ggproto(
    "StatEnthalpy",
    Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    extra_params = c("na.rm", "label", "psychro_scales"),

    required_aes = c("x", "enthalpy", "pres", "units"),

    compute_group = function(self, data, scales, psychro_scales = NULL) {
        units <- get_units(data)
        tdb <- psychro_scale_inverse(scales$x, data$x)
        enthalpy <- psychro_stat_inverse_property(
            data$enthalpy,
            "enthalpy",
            psychro_scales
        )
        humratio <- with_units(
            units,
            GetHumRatioFromEnthalpyAndTDryBulb(enthalpy, tdb)
        )
        finish_stat_humratio(data, humratio, units, scales)
    }
)
