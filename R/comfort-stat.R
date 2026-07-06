#' @include comfort-core.R comfort-grid.R comfort-band.R comfort-contour.R comfort-zone.R comfort-dispatch.R comfort-model.R
NULL

# Generic comfort stats route setup_data() through init_stat_data() so units/pres
# injected by ggpsychro() remain available when compute_panel() resolves inputs.
# Named tile geom lets comfort band layers select tile rendering explicitly.
GeomComfortTile <- ggplot2::ggproto(
    "GeomComfortTile",
    ggplot2::GeomTile
)

# Generic filled-band stat builds polygon bands from sampled comfort fields.
#' @noRd
StatComfortBand <- ggplot2::ggproto(
    "StatComfortBand",
    ggplot2::Stat,

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
        "na.rm",
        "model",
        "metric",
        "n",
        "levels",
        "units",
        "pres",
        "mollier",
        "tdb_lim",
        "hum_lim",
        "psychro_scales"
    ),

    compute_panel = function(
        self,
        data,
        scales,
        model = comfort_model_pmv(),
        metric = NULL,
        n = NULL,
        levels = NULL,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort__stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_band__data(
            model,
            metric,
            levels,
            comfort_grid__default_n(model, n),
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)

# Grid stat keeps the sampled cells intact for tile rendering.
#' @noRd
StatComfortGrid <- ggplot2::ggproto(
    "StatComfortGrid",
    ggplot2::Stat,

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
        "na.rm",
        "model",
        "metric",
        "n",
        "gap",
        "units",
        "pres",
        "mollier",
        "tdb_lim",
        "hum_lim",
        "psychro_scales"
    ),

    compute_panel = function(
        self,
        data,
        scales,
        model = comfort_model_pmv(),
        metric = NULL,
        n = NULL,
        gap = 0,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort__stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_grid__data(
            model,
            metric,
            comfort_grid__default_n(model, n),
            gap,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            na.rm = na.rm,
            psychro_scales = psychro_scales
        )
    }
)

# Contour stat traces metric isolines and can return textpath-ready label paths.
#' @noRd
StatComfortContour <- ggplot2::ggproto(
    "StatComfortContour",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(colour = ggplot2::after_stat(level)),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "model",
        "metric",
        "breaks",
        "n",
        "label_path",
        "units",
        "pres",
        "mollier",
        "tdb_lim",
        "hum_lim",
        "psychro_scales"
    ),

    compute_panel = function(
        self,
        data,
        scales,
        model = comfort_model_pmv(),
        metric = NULL,
        breaks = NULL,
        n = NULL,
        label_path = FALSE,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort__stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_contour__data(
            model,
            metric,
            breaks,
            comfort_grid__default_n(model, n),
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            label_path = label_path,
            psychro_scales = psychro_scales
        )
    }
)
# Zone stat converts a metric interval into a filled polygon region.
#' @noRd
StatComfortZone <- ggplot2::ggproto(
    "StatComfortZone",
    ggplot2::Stat,

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
        "na.rm",
        "model",
        "metric",
        "range",
        "n",
        "gap",
        "rootband_levels",
        "rootband_cache",
        "units",
        "pres",
        "mollier",
        "tdb_lim",
        "hum_lim",
        "psychro_scales"
    ),

    compute_panel = function(
        self,
        data,
        scales,
        model = comfort_model_pmv(),
        metric = NULL,
        range = NULL,
        n = NULL,
        gap = 0,
        rootband_levels = NULL,
        rootband_cache = NULL,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort__stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        comfort_zone__data(
            model,
            metric,
            range,
            comfort_grid__default_n(model, n),
            gap,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            na.rm = na.rm,
            rootband_levels = rootband_levels,
            rootband_cache = rootband_cache,
            psychro_scales = psychro_scales
        )
    }
)
# State stat evaluates a comfort model at user-supplied psychrometric states.
#' @noRd
StatComfortState <- ggplot2::ggproto(
    "StatComfortState",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    required_aes = c("tdb"),

    optional_aes = psychro_state_properties(),

    extra_params = c(
        "na.rm",
        "model",
        "units",
        "pres",
        "mollier",
        "psychro_scales"
    ),

    compute_group = function(
        self,
        data,
        scales,
        model = comfort_model_pmv(),
        units,
        pres,
        mollier = FALSE,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort__stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        data <- psychro_compute_state(
            data,
            units,
            pres,
            mollier,
            na.rm = na.rm,
            psychro_scales = psychro_scales
        )
        if (!nrow(data)) {
            return(data)
        }

        rh <- comfort_dispatch__relhum_from_humratio(
            data$tdb,
            data$humratio,
            units,
            pres
        )
        result <- comfort_dispatch__apply_model(
            model,
            data$tdb,
            rh,
            units,
            pres
        )
        cbind(data, result)
    }
)
