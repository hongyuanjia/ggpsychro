#' @include psychro-state.R stat-psychro-bin.R
NULL

# Shared data-shaping and validation helpers for comfort layers. Model-specific
# equations live in the PMV, adaptive, heat-index, and Givoni modules.
comfort__model <- function(type, params) {
    structure(
        list(type = type, params = params),
        class = c("PsyComfortModel", "list")
    )
}

comfort__model_type <- function(model) {
    comfort__check_model(model)
    model$type
}

comfort__check_model <- function(model) {
    if (
        !inherits(model, "PsyComfortModel") ||
            !model$type %in% c("pmv", "set", "adaptive", "heat_index")
    ) {
        stop("`model` must be created by comfort_model_*().", call. = FALSE)
    }
    invisible(model)
}

comfort__standard <- function(name, breaks, fills, alphas) {
    structure(
        list(name = name, breaks = breaks, fills = fills, alphas = alphas),
        class = c("PsyComfortStandard", "list")
    )
}

comfort__standard_alpha <- function(override, defaults, i) {
    if (is.null(override)) {
        return(defaults[[i]])
    }
    override <- suppressWarnings(as.numeric(override))
    if (!length(override) || any(!is.finite(override))) {
        stop("`alpha` must be finite when supplied.", call. = FALSE)
    }
    rep(override, length.out = length(defaults))[[i]]
}

comfort__check_standard <- function(standard) {
    if (
        !inherits(standard, "PsyComfortStandard") ||
            !standard$name %in% c("ashrae55_2017", "en15251_2007")
    ) {
        stop(
            "`standard` must be created by comfort_pmv_ashrae55() or ",
            "comfort_pmv_en15251().",
            call. = FALSE
        )
    }
    standard
}

# Validate that an object is a Givoni strategy created by the public constructor.
givoni__check_strategy <- function(strategy) {
    if (!inherits(strategy, "PsyComfortGivoniStrategy")) {
        stop(
            "`strategy` must be created by comfort_strategy_givoni().",
            call. = FALSE
        )
    }
    strategy$units <- match.arg(strategy$units, c("SI", "IP"))
    strategy
}

comfort__check_scalar_finite <- function(x, name, allow_null = FALSE) {
    if (is.null(x) && isTRUE(allow_null)) {
        return(NULL)
    }
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || !is.finite(x)) {
        stop(name, " must be a single finite number.", call. = FALSE)
    }
    x
}

comfort__check_flag <- function(x, name) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        stop(name, " must be `TRUE` or `FALSE`.", call. = FALSE)
    }
    x
}

# Internal contour/band generators may receive unordered break candidates from
# callers or defaults, so they normalize before constructing geometry.
comfort__check_breaks <- function(x, name, n_min = 2L) {
    x <- sort(unique(as.numeric(x)))
    if (length(x) < n_min || any(!is.finite(x))) {
        stop(
            name,
            " must contain finite increasing PMV boundaries.",
            call. = FALSE
        )
    }
    x
}

# Public comfort standards preserve the user's declared category order; sorting
# here would silently turn an invalid standard into a different one.
comfort__check_ordered_breaks <- function(x, name, n_min = 2L) {
    x <- as.numeric(x)
    if (length(x) < n_min || any(!is.finite(x)) || any(diff(x) <= 0)) {
        stop(
            name,
            " must contain finite strictly increasing PMV boundaries.",
            call. = FALSE
        )
    }
    x
}

comfort__layer_data <- function(data) {
    if (is.null(data)) {
        return(util__new_data_frame(list(.comfort = 1), n = 1L))
    }
    data
}

comfort__computed_xy_mapping <- function(mapping = NULL) {
    x <- y <- NULL
    xy <- ggplot2::aes(
        x = ggplot2::after_stat(x),
        y = ggplot2::after_stat(y)
    )
    if (is.null(mapping)) {
        return(xy)
    }
    utils::modifyList(mapping, xy)
}

comfort__recycle <- function(...) {
    args <- lapply(list(...), as.numeric)
    lens <- vapply(args, length, integer(1L))
    n <- max(lens)
    bad <- lens != 1L & lens != n
    if (any(bad)) {
        stop("Comfort inputs must have compatible lengths.", call. = FALSE)
    }
    lapply(args, rep, length.out = n)
}

comfort__between <- function(x, lower, upper) {
    is.finite(x) & x >= lower & x <= upper
}

comfort__to_si_temp <- function(x, units) {
    if (units == "IP") unit__c_from_f(x) else x
}

comfort__from_si_temp <- function(x, units) {
    if (units == "IP") unit__f_from_c(x) else x
}

comfort__to_si_speed <- function(x, units) {
    if (units == "IP") x / 3.281 else x
}

comfort__pressure_pa <- function(p_atm, units) {
    if (units == "IP") p_atm * 6894.757293168 else p_atm
}

comfort__p_sat_torr <- function(tdb) {
    exp(18.6686 - 4030.183 / (tdb + 235))
}
comfort__stat_units <- function(data, units) {
    if ("units" %in% names(data)) {
        unit__from_data(data)
    } else {
        match.arg(units, c("SI", "IP"))
    }
}

comfort__stat_pressure <- function(data, pres) {
    if ("pres" %in% names(data)) {
        pres <- unique(data$pres)
    }
    if (length(pres) != 1L || !is.finite(pres)) {
        stop(
            "`pres` must resolve to a single finite pressure value.",
            call. = FALSE
        )
    }
    pres
}

comfort__stat_context <- function(data, units, pres) {
    # ggpsychro() injects units/pres through the layer data; explicit Stat
    # parameters are only used when the layer is built outside that context.
    list(
        units = comfort__stat_units(data, units),
        pres = comfort__stat_pressure(data, pres)
    )
}
