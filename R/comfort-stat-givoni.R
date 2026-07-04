#' @include comfort-stat.R comfort-givoni.R
NULL

# Givoni stats adapt the strategy geometry and label anchors to the active
# psychrometric coordinate context.
# Givoni zone stat converts strategy zone specs into chart coordinates.
#' @noRd
StatComfortGivoniZone <- ggplot2::ggproto(
    "StatComfortGivoniZone",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "strategy",
        "zone",
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
        strategy = comfort_strategy_givoni(),
        zone = NULL,
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
        givoni__zone_data(
            strategy,
            zone,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)

# Givoni label stat emits path labels and point labels for strategy regions.
#' @noRd
StatComfortGivoniLabel <- ggplot2::ggproto(
    "StatComfortGivoniLabel",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "strategy",
        "label_type",
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
        strategy = comfort_strategy_givoni(),
        label_type = "point",
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
        givoni__label_data(
            strategy,
            label_type,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)

# Mean-outdoor stat draws the shifted comfort reference marker.
#' @noRd
StatComfortGivoniMeanOutdoor <- ggplot2::ggproto(
    "StatComfortGivoniMeanOutdoor",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "strategy",
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
        strategy = comfort_strategy_givoni(),
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
        givoni__mean_outdoor_data(
            strategy,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)

# Mean-outdoor label stat positions the reference marker annotation.
#' @noRd
StatComfortGivoniMeanOutdoorLabel <- ggplot2::ggproto(
    "StatComfortGivoniMeanOutdoorLabel",
    ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm",
        "strategy",
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
        strategy = comfort_strategy_givoni(),
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
        givoni__mean_outdoor_label_data(
            strategy,
            units,
            pres,
            mollier,
            tdb_lim,
            hum_lim,
            psychro_scales = psychro_scales
        )
    }
)
