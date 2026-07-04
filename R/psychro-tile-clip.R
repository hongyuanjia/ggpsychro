#' @include psychro-zone.R
NULL

# Tile clipping helpers convert rectangular bins into polygons constrained by
# the psychrometric saturation curve.
# Draw psychrometric tile polygons through ggplot2 polygon rendering.
psychro_tile_grob <- function(data, panel_params, coord, lineend, linejoin) {
    polygons <- psychro_tile_polygon_data(data, coord)
    if (!nrow(polygons)) {
        return(grid::nullGrob())
    }

    ggplot2::GeomPolygon$draw_panel(
        polygons,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin
    )
}

# Convert tile center/size rows into drawable polygon vertices.
psychro_tile_polygon_data <- function(data, coord, n = 16L) {
    if (!nrow(data)) {
        return(data[0, , drop = FALSE])
    }

    data <- psychro_tile_bounds(data)
    draw_rectangular <- isTRUE(coord$mollier) ||
        is.null(coord$units) ||
        is.null(coord$pressure)

    polygons <- lapply(seq_len(nrow(data)), function(i) {
        if (draw_rectangular) {
            psychro_tile_rectangle_polygon(data[i, , drop = FALSE], i)
        } else {
            psychro_tile_saturation_polygon(
                data[i, , drop = FALSE],
                coord$units,
                coord$pressure,
                i,
                n = n
            )
        }
    })
    polygons <- Filter(nrow, polygons)
    if (!length(polygons)) {
        return(data[0, , drop = FALSE])
    }

    out <- do.call(rbind, polygons)
    row.names(out) <- NULL
    out
}

# Ensure tile bounds are available even when ggplot2 supplied center geometry.
psychro_tile_bounds <- function(data) {
    if (!"xmin" %in% names(data)) {
        data$xmin <- data$x - data$width / 2
    }
    if (!"xmax" %in% names(data)) {
        data$xmax <- data$x + data$width / 2
    }
    if (!"ymin" %in% names(data)) {
        data$ymin <- data$y - data$height / 2
    }
    if (!"ymax" %in% names(data)) {
        data$ymax <- data$y + data$height / 2
    }
    data
}

# Build an uncut rectangular tile polygon.
psychro_tile_rectangle_polygon <- function(row, group) {
    if (!psychro_tile_has_area(row)) {
        return(row[0, , drop = FALSE])
    }

    out <- row[rep(1L, 4L), , drop = FALSE]
    out$x <- c(row$xmin, row$xmax, row$xmax, row$xmin)
    out$y <- c(row$ymin, row$ymin, row$ymax, row$ymax)
    out$group <- group
    out
}

# Build a tile polygon clipped by the saturation boundary.
psychro_tile_saturation_polygon <- function(
    row,
    units,
    pres,
    group,
    n = 16L,
    tolerance = 1e-10
) {
    if (!psychro_tile_has_area(row)) {
        return(row[0, , drop = FALSE])
    }

    saturation_min <- psychro_saturation_humratio(row$xmin, units, pres)
    saturation_max <- psychro_saturation_humratio(row$xmax, units, pres)
    if (is.finite(saturation_min) && row$ymax <= saturation_min + tolerance) {
        return(psychro_tile_rectangle_polygon(row, group))
    }
    if (is.finite(saturation_max) && row$ymin >= saturation_max - tolerance) {
        return(row[0, , drop = FALSE])
    }

    x <- psychro_tile_saturation_x(row, units, pres, n = n)
    saturation <- psychro_saturation_humratio(x, units, pres)
    upper <- pmin(row$ymax, saturation)
    keep <- is.finite(x) & is.finite(upper) & upper >= row$ymin - tolerance
    x <- x[keep]
    upper <- pmax(upper[keep], row$ymin)

    if (length(x) < 2L || all(upper <= row$ymin + tolerance)) {
        return(row[0, , drop = FALSE])
    }

    out <- row[rep(1L, length(x) * 2L), , drop = FALSE]
    out$x <- c(x, rev(x))
    out$y <- c(rep(row$ymin, length(x)), rev(upper))
    out$group <- group
    out
}

# Test whether a tile row has a finite positive area.
psychro_tile_has_area <- function(row) {
    vals <- unlist(row[c("xmin", "xmax", "ymin", "ymax")], use.names = FALSE)
    all(is.finite(vals)) && row$xmin < row$xmax && row$ymin < row$ymax
}

# Choose dry-bulb sample points where the tile may cross saturation.
psychro_tile_saturation_x <- function(
    row,
    units,
    pres,
    n = 16L,
    tolerance = 1e-10
) {
    x <- c(
        seq(row$xmin, row$xmax, length.out = max(2L, n)),
        row$xmin,
        row$xmax,
        psychro_tile_tdb_at_hum(c(row$ymin, row$ymax), units, pres)
    )
    x <- x[is.finite(x) & x >= row$xmin - tolerance & x <= row$xmax + tolerance]
    x <- sort(pmin(pmax(x, row$xmin), row$xmax))
    x[c(TRUE, diff(x) > tolerance)]
}

# Solve dry-bulb temperature at a given humidity ratio for clipping.
psychro_tile_tdb_at_hum <- function(hum, units, pres) {
    out <- rep(NA_real_, length(hum))
    keep <- is.finite(hum) & hum > 0
    if (any(keep)) {
        out[keep] <- psychrolib__with_units(
            units,
            GetTDewPointFromHumRatioOnly(hum[keep], pres)
        )
    }
    out
}
