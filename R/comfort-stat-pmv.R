#' @include comfort-stat.R comfort-pmv.R
NULL

# PMV-specific stats trace roots for curves, axis labels, and polygon bands.
# Wrapper-local cache environments share expensive root solves between sibling layers.
# PMV curve stat traces constant-PMV roots and optionally emits label metadata.
#' @noRd
StatComfortPmvCurve <- ggplot2::ggproto(
    "StatComfortPmvCurve",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "model",
        "levels",
        "n",
        "label_type",
        "label_hjust",
        "label_vjust",
        "reverse",
        "curve_cache",
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
        levels = seq(-3, 3, by = 0.5),
        n = 360,
        label_type = c("none", "sensation", "boundary", "comfort"),
        label_hjust = NULL,
        label_vjust = NULL,
        reverse = FALSE,
        curve_cache = NULL,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        label_type <- match.arg(label_type)
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        pmv__curve_data(
            model,
            levels,
            n,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            label = label_type,
            label_hjust = label_hjust,
            label_vjust = label_vjust,
            reverse = reverse,
            curve_cache = curve_cache,
            psychro_scales = psychro_scales
        )
    }
)

# Axis label stat places PMV labels along the chart boundary.
#' @noRd
StatComfortPmvAxisLabel <- ggplot2::ggproto(
    "StatComfortPmvAxisLabel",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "model",
        "levels",
        "n",
        "axis_label_hjust",
        "curve_cache",
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
        levels = seq(-3, 3, by = 0.5),
        n = 360,
        axis_label_hjust = ggplot2::waiver(),
        curve_cache = NULL,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        pmv__axis_label_data(
            model,
            levels,
            n,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            axis_label_hjust = axis_label_hjust,
            curve_cache = curve_cache,
            psychro_scales = psychro_scales
        )
    }
)

# Root-band stat builds continuous PMV bands from solved curve boundaries.
#' @noRd
StatComfortPmvRootBand <- ggplot2::ggproto(
    "StatComfortPmvRootBand",
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
        "levels",
        "n",
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
        levels = NULL,
        n = NULL,
        units,
        pres,
        mollier = FALSE,
        tdb_lim = NULL,
        hum_lim = NULL,
        na.rm = FALSE,
        psychro_scales = NULL
    ) {
        ctx <- comfort_stat_context(data, units, pres)
        units <- ctx$units
        pres <- ctx$pres
        pmv__root_band_data(
            model,
            metric,
            levels,
            comfort_default_n(model, n),
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)
