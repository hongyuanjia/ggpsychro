#' @include comfort-api.R comfort-core.R comfort-pmv.R comfort-heat-index.R comfort-givoni.R
NULL

# All comfort stats route setup_data() through init_stat_data() so units/pres
# injected by ggpsychro() remain available when compute_panel() resolves model inputs.
# *_cache extra_params are private wrapper-local environments used to share
# expensive root/grid work between sibling layers without global state.
GeomComfortTile <- ggplot2::ggproto(
    "GeomComfortTile", ggplot2::GeomTile
)

#' @noRd
StatComfortBand <- ggplot2::ggproto(
    "StatComfortBand", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = ggplot2::after_stat(value),
        subgroup = ggplot2::after_stat(subgroup),
        alpha = 0.55
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "n", "levels", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, n = NULL, levels = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_band_data(
            model, metric, levels, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGrid <- ggplot2::ggproto(
    "StatComfortGrid", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = ggplot2::after_stat(value),
        width = ggplot2::after_stat(width),
        height = ggplot2::after_stat(height),
        alpha = 0.55
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "n", "gap", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, n = NULL, gap = 0,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_grid_data(
            model, metric, comfort_default_n(model, n), gap, units, pres,
            mollier, tdb_lim, hum_lim, na.rm = na.rm
        )
    }
)

#' @noRd
StatComfortContour <- ggplot2::ggproto(
    "StatComfortContour", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(colour = ggplot2::after_stat(level)),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "breaks", "n", "contour_method",
        "label_path", "units", "pres", "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, breaks = NULL,
                             n = NULL, contour_method = "auto",
                             label_path = FALSE, units, pres,
                             mollier = FALSE, tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_contour_data(
            model, metric, breaks, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim, contour_method = contour_method,
            label_path = label_path
        )
    }
)

#' @noRd
StatComfortPmvCurve <- ggplot2::ggproto(
    "StatComfortPmvCurve", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "levels", "n", "label_type", "label_hjust",
        "label_vjust", "reverse", "curve_cache", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             levels = seq(-3, 3, by = 0.5), n = 360,
                             label_type = c("none", "sensation", "boundary", "comfort"),
                             label_hjust = NULL, label_vjust = NULL,
                             reverse = FALSE, curve_cache = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        label_type <- match.arg(label_type)
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_pmv_curve_data(
            model, levels, n, units, pres, mollier, tdb_lim, hum_lim,
            label = label_type, label_hjust = label_hjust,
            label_vjust = label_vjust, reverse = reverse,
            curve_cache = curve_cache
        )
    }
)

#' @noRd
StatComfortPmvAxisLabel <- ggplot2::ggproto(
    "StatComfortPmvAxisLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "levels", "n", "axis_label_hjust", "curve_cache",
        "units", "pres", "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             levels = seq(-3, 3, by = 0.5), n = 360,
                             axis_label_hjust = ggplot2::waiver(),
                             curve_cache = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_pmv_axis_label_data(
            model, levels, n, units, pres, mollier, tdb_lim, hum_lim,
            axis_label_hjust = axis_label_hjust,
            curve_cache = curve_cache
        )
    }
)

#' @noRd
StatComfortPmvRootBand <- ggplot2::ggproto(
    "StatComfortPmvRootBand", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = ggplot2::after_stat(value),
        subgroup = ggplot2::after_stat(subgroup),
        alpha = 0.55
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "levels", "n", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, levels = NULL, n = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_pmv_rootband_data(
            model, metric, levels, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortZone <- ggplot2::ggproto(
    "StatComfortZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = "#70B77E",
        alpha = 0.2,
        subgroup = ggplot2::after_stat(subgroup)
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "range", "n", "gap",
        "rootband_levels", "rootband_cache", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, range = NULL,
                             n = NULL, gap = 0, rootband_levels = NULL,
                             rootband_cache = NULL, units, pres,
                             mollier = FALSE, tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_zone_data(
            model, metric, range, comfort_default_n(model, n), gap, units,
            pres, mollier, tdb_lim, hum_lim, na.rm = na.rm,
            rootband_levels = rootband_levels, rootband_cache = rootband_cache
        )
    }
)

#' @noRd
StatComfortHeatIndexZone <- ggplot2::ggproto(
    "StatComfortHeatIndexZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "n", "category_id", "grid_cache", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             model = comfort_model_heat_index(),
                             n = c(160, 100), category_id = NULL,
                             grid_cache = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_heat_index_zone_data(
            model, category_id, comfort_grid_n(n), units, pres,
            mollier, tdb_lim, hum_lim, grid_cache = grid_cache
        )
    }
)

#' @noRd
StatComfortHeatIndexContour <- ggplot2::ggproto(
    "StatComfortHeatIndexContour", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "n", "grid_cache", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             model = comfort_model_heat_index(),
                             n = c(160, 100), grid_cache = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_heat_index_contour_data(
            model, comfort_grid_n(n), units, pres, mollier, tdb_lim, hum_lim,
            grid_cache = grid_cache
        )
    }
)

#' @noRd
StatComfortHeatIndexLabel <- ggplot2::ggproto(
    "StatComfortHeatIndexLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "n", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             model = comfort_model_heat_index(),
                             n = c(160, 100), units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_heat_index_label_data(
            model, comfort_grid_n(n), units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniZone <- ggplot2::ggproto(
    "StatComfortGivoniZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "zone", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             zone = NULL, units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_givoni_zone_data(
            strategy, zone, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniLabel <- ggplot2::ggproto(
    "StatComfortGivoniLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "label_type", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             label_type = "point",
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_givoni_label_data(
            strategy, label_type, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniMeanOutdoor <- ggplot2::ggproto(
    "StatComfortGivoniMeanOutdoor", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_givoni_mean_outdoor_data(
            strategy, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniMeanOutdoorLabel <- ggplot2::ggproto(
    "StatComfortGivoniMeanOutdoorLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_givoni_mean_outdoor_label_data(
            strategy, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortState <- ggplot2::ggproto(
    "StatComfortState", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    required_aes = c("tdb"),

    optional_aes = psychro_state_properties(),

    extra_params = c("na.rm", "model", "units", "pres", "mollier"),

    compute_group = function(self, data, scales, model = comfort_model_pmv(),
                             units, pres, mollier = FALSE,
                             na.rm = FALSE) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        data <- psychro_compute_state(data, units, pres, mollier, na.rm = na.rm)
        if (!nrow(data)) {
            return(data)
        }

        rh <- comfort_relhum_from_humratio(data$tdb, data$humratio, units, pres)
        result <- comfort_apply_model(model, data$tdb, rh, units, pres)
        cbind(data, result)
    }
)
