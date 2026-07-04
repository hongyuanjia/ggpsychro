#' @include stat-psychro-bin.R psychro-tile-clip.R psychro-tile-grid.R
NULL

# Tile geom wrapper and ggproto class for psychrometric bin output.
#' @rdname stat_psychro_bin
#' @export
geom_psychro_tile <- function(
    mapping = NULL,
    data = NULL,
    stat = "psychro_bin",
    position = "identity",
    ...,
    gap = 0.08,
    boundary = c(0, 0),
    cell.grid = TRUE,
    cell.grid.colour = ggplot2::waiver(),
    cell.grid.linewidth = ggplot2::waiver(),
    cell.grid.linetype = ggplot2::waiver(),
    cell.grid.alpha = ggplot2::waiver(),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    params <- list(
        na.rm = na.rm,
        cell.grid = cell.grid,
        cell.grid.colour = cell.grid.colour,
        cell.grid.linewidth = cell.grid.linewidth,
        cell.grid.linetype = cell.grid.linetype,
        cell.grid.alpha = cell.grid.alpha,
        ...
    )
    if (identical(stat, "psychro_bin") || identical(stat, StatPsychroBin)) {
        params$gap <- gap
        params$boundary <- boundary
    }

    psychro_layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPsychroTile,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = params
    )
}

# Geom draws tiles as saturation-clipped polygons with an optional cell grid.
GeomPsychroTile <- ggproto(
    "GeomPsychroTile",
    ggplot2::GeomTile,
    extra_params = c(
        "na.rm",
        "cell.grid",
        "cell.grid.colour",
        "cell.grid.linewidth",
        "cell.grid.linetype",
        "cell.grid.alpha",
        "psychro.theme"
    ),
    default_aes = utils::modifyList(
        ggplot2::GeomTile$default_aes,
        ggplot2::aes(alpha = 0.85)
    ),
    draw_panel = function(
        self,
        data,
        panel_params,
        coord,
        lineend = "butt",
        linejoin = "mitre",
        cell.grid = TRUE,
        cell.grid.colour = ggplot2::waiver(),
        cell.grid.linewidth = ggplot2::waiver(),
        cell.grid.linetype = ggplot2::waiver(),
        cell.grid.alpha = ggplot2::waiver(),
        psychro.theme = NULL
    ) {
        tiles <- psychro_tile_grob(
            data,
            panel_params,
            coord,
            lineend = lineend,
            linejoin = linejoin
        )

        if (!isTRUE(cell.grid)) {
            return(tiles)
        }

        cell_grid <- psychro_tile_cell_grid_grob(
            data,
            panel_params,
            coord,
            theme = psychro.theme,
            colour = cell.grid.colour,
            linewidth = cell.grid.linewidth,
            linetype = cell.grid.linetype,
            alpha = cell.grid.alpha,
            lineend = lineend,
            linejoin = linejoin
        )

        grid::grobTree(tiles, cell_grid)
    }
)
