# package options {{{
# psychrolib package env
psy_op <- get("PSYCHRO_OPT", envir = asNamespace("psychrolib"), inherits = FALSE)

GGPSY_OPT <- new.env(parent = emptyenv())
# dry-bulb temp limit in Celsius [SI]
GGPSY_OPT$tdb_min <- -50.0
GGPSY_OPT$tdb_max <- 100.0
# humidity ratio limit in g_H2O kg_Air-1 [SI]
GGPSY_OPT$hum_min <- 0.0
GGPSY_OPT$hum_max <- 60.0
# all known ggplot x aes
GGPSY_OPT$x_aes <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
    "xmax_final", "xlower", "xmiddle", "xupper", "x0")
# all known ggplot y aes
GGPSY_OPT$y_aes <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
    "ymax_final", "lower", "middle", "upper", "y0")
# }}}

# .layer_list {{{
.layer_list <- c(
    maskarea = "PsyLayerMaskArea",
    linestat = "PsyLayerLineSat",
    grid = "PsyLayerGrid"
)
# }}}

# .geom_list {{{
.geom_list <- c(
    linesat = "GeomLineSat",
    maskarea = "GeomMaskArea",
    grid_relhum = "GeomGridRelHum",
    grid_wetbulb = "GeomGridWetBulb",
    grid_vappres = "GeomGridVapPres",
    grid_specvol = "GeomGridSpecVol",
    grid_enthalpy = "GeomGridEnthalpy"
)
# }}}

# .unit_list {{{
.unit_list <- list(
    SI = list(
        drybulb = "degree * C",
        humratio = "g[m] * ' / ' * kg[da]",
        relhum = "%",
        wetbulb = "degree * C",
        vappres = "kPa",
        specvol = "m^3 * ' / ' * kg",
        enthalpy = "kJ * ' / ' * kg"
    ),
    IP = list(
        drybulb = "degree * F",
        humratio = "gr[m] * ' / ' * lb[da]",
        relhum = "%",
        wetbulb = "degree * F",
        vappres = "kPsi",
        specvol = "ft^3 * ' / ' * lb",
        enthalpy = "Btu * ' / ' * lb"
    ),
    parse = list(
        drybulb = TRUE,
        humratio = TRUE,
        relhum = FALSE,
        wetbulb = TRUE,
        vappres = FALSE,
        specvol = TRUE,
        enthalpy = TRUE
    )
)
# }}}

# get_tdb_limits {{{
get_tdb_limits <- function (units) {
    if (units == "SI") {
        c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max), "F")
    }
}
# }}}

# get_hum_limits {{{
get_hum_limits <- function (units) {
    if (units == "SI") {
        c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max), "Gr")
    }
}
# }}}

# get_units {{{
get_units <- function (data) {
    u <- unique(data$units)

    if (is.integer(u)) u <- decode_units(u)

    if (length(u) > 1L || (!u %in% c("SI", "IP"))) {
        stop("The system of units has to be either SI or IP.")
    }

    u
}
# }}}

# get_pres {{{
get_pres <- function (data) {
    # check pressure
    pres <- unique(data$pres)

    if (length(pres) > 1L) {
        warning(sprintf("Multiple atmosphere pressure value found. Only the first one will be used (%.f).", pres))
        pres <- pres[[1L]]
    }

    pres
}
# }}}
