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
GGPSY_OPT$x_aes <- getFromNamespace("ggplot_global", ns = asNamespace("ggplot2"))$x_aes
# all known ggplot y aes
GGPSY_OPT$y_aes <- getFromNamespace("ggplot_global", ns = asNamespace("ggplot2"))$y_aes
GGPSY_OPT$tdb_aes <- sub("x(.)", "tdb_\\1", GGPSY_OPT$x_aes)
GGPSY_OPT$tdb_aes <- sub("^x$", "tdb", GGPSY_OPT$tdb_aes)
GGPSY_OPT$hum_aes <- sub("tdb", "hum", GGPSY_OPT$tdb_aes)

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

get_tdb_limits <- function (units) {
    if (units == "SI") {
        c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$tdb_min, GGPSY_OPT$tdb_max), "F")
    }
}

get_hum_limits <- function (units) {
    if (units == "SI") {
        c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max)
    } else if (units == "IP") {
        bid_conv(c(GGPSY_OPT$hum_min, GGPSY_OPT$hum_max), "Gr")
    }
}
