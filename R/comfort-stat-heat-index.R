#' @include comfort-stat.R comfort-heat-index.R
NULL

# Heat-index stats build category regions, threshold contours, and foreground
# label anchors from the same sampled grid.
# Heat-index zone stat extracts one category region from the shared grid.
#' @noRd
StatComfortHeatIndexZone <- ggplot2::ggproto(
    "StatComfortHeatIndexZone",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "model",
        "n",
        "category_id",
        "grid_cache",
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
        model = comfort_model_heat_index(),
        n = c(160, 100),
        category_id = NULL,
        grid_cache = NULL,
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
        heat_index__zone_data(
            model,
            category_id,
            comfort_grid_n(n),
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            grid_cache = grid_cache,
            psychro_scales = psychro_scales
        )
    }
)

# Heat-index contour stat draws category threshold boundaries.
#' @noRd
StatComfortHeatIndexContour <- ggplot2::ggproto(
    "StatComfortHeatIndexContour",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "model",
        "n",
        "grid_cache",
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
        model = comfort_model_heat_index(),
        n = c(160, 100),
        grid_cache = NULL,
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
        heat_index__contour_data(
            model,
            comfort_grid_n(n),
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            grid_cache = grid_cache,
            psychro_scales = psychro_scales
        )
    }
)

# Heat-index label stat computes anchors consumed by foreground rendering.
#' @noRd
StatComfortHeatIndexLabel <- ggplot2::ggproto(
    "StatComfortHeatIndexLabel",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "model",
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
        model = comfort_model_heat_index(),
        n = c(160, 100),
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
        heat_index__label_data(
            model,
            comfort_grid_n(n),
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)
