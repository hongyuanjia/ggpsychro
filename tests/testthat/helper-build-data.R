built_data_layers <- function(built) {
    keep <- !vapply(built$plot$layers, function(layer) {
        inherits(layer$geom, "GeomPsychroSaturation")
    }, logical(1L))
    built$data[keep]
}

first_built_data <- function(built) {
    built_data_layers(built)[[1L]]
}
