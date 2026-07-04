# Package-wide psychrometric options are kept in a private environment so shared
# defaults can be updated internally without exporting mutable state.
GGPSY_OPT <- new.env(parent = emptyenv())

# Lower dry-bulb temperature limit in Celsius for the SI psychrolib domain.
GGPSY_OPT$tdb_min <- -50.0

# Upper dry-bulb temperature limit in Celsius for the SI psychrolib domain.
GGPSY_OPT$tdb_max <- 100.0

# Lower humidity-ratio display limit in g_H2O kg_Air-1 for SI charts.
GGPSY_OPT$hum_min <- 0.0

# Upper humidity-ratio display limit in g_H2O kg_Air-1 for SI charts.
GGPSY_OPT$hum_max <- 60.0

# ggplot2's known x-position aesthetics are reused for dry-bulb scale aliases.
GGPSY_OPT$x_aes <- utils::getFromNamespace(
    "ggplot_global",
    ns = asNamespace("ggplot2")
)$x_aes

# ggplot2's known y-position aesthetics are reused for humidity scale aliases.
GGPSY_OPT$y_aes <- utils::getFromNamespace(
    "ggplot_global",
    ns = asNamespace("ggplot2")
)$y_aes

# Dry-bulb aesthetics mirror ggplot2 x aesthetics with a psychrometric prefix.
GGPSY_OPT$tdb_aes <- sub("x(.)", "tdb_\\1", GGPSY_OPT$x_aes)

# The primary dry-bulb aesthetic keeps the short public name `tdb`.
GGPSY_OPT$tdb_aes <- sub("^x$", "tdb", GGPSY_OPT$tdb_aes)

# Humidity-ratio aesthetics parallel dry-bulb aesthetics for Mollier switching.
GGPSY_OPT$hum_aes <- sub("tdb", "hum", GGPSY_OPT$tdb_aes)

# Unit labels and parsing flags are centralized so axes, legends, and helpers
# use the same text for each psychrometric property.
GGPSY_UNIT_SPECS <- list(
    # SI display labels use plotmath strings for degree and subscript notation.
    SI = list(
        drybulb = "degree * C",
        humratio = "g[m] * ' / ' * kg[da]",
        relhum = "%",
        wetbulb = "degree * C",
        vappres = "kPa",
        specvol = "m^3 * ' / ' * kg",
        enthalpy = "kJ * ' / ' * kg"
    ),

    # IP display labels use grain, Fahrenheit, psi, and Btu conventions.
    IP = list(
        drybulb = "degree * F",
        humratio = "gr[m] * ' / ' * lb[da]",
        relhum = "%",
        wetbulb = "degree * F",
        vappres = "psi",
        specvol = "ft^3 * ' / ' * lb",
        enthalpy = "Btu * ' / ' * lb"
    ),

    # Parsing flags mark labels that should be rendered as plotmath expressions.
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
