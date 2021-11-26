PsyLayout <- ggplot2::ggproto("PsyLayout", ggplot2::Layout,
    # Per panel scales
    panel_scales_rh = NULL,
    panel_scales_wb = NULL,
    panel_scales_vp = NULL,
    panel_scales_sv = NULL,
    panel_scales_en = NULL,

    train_position = function(self, data, x_scale, y_scale, rh_scale, wb_scale, vp_scale, sv_scale, en_scale) {
        # Initialise scales if needed, and possible.
        layout <- self$layout
        if (is.null(self$panel_scales_x)) {
            self$panel_scales_x <- self$facet$init_scales(layout, x_scale = x_scale,
                params = self$facet_params)$x
        }
        if (is.null(self$panel_scales_y)) {
            self$panel_scales_y <- self$facet$init_scales(layout, y_scale = y_scale,
                params = self$facet_params)$y
        }

        self$facet$train_scales(
            self$panel_scales_x,
            self$panel_scales_y,
            layout,
            data,
            self$facet_params
        )

        # Added for ggpsychro
        ind_panel <- seq_len(max(length(self$panel_scales_x), length(self$panel_scales_y)))
        if (!length(ind_panel)) ind_panel <- 1L

        self$panel_scales_rh <- lapply(ind_panel, function(i) rh_scale$clone())
        self$panel_scales_wb <- lapply(ind_panel, function(i) wb_scale$clone())
        self$panel_scales_vp <- lapply(ind_panel, function(i) vp_scale$clone())
        self$panel_scales_sv <- lapply(ind_panel, function(i) sv_scale$clone())
        self$panel_scales_en <- lapply(ind_panel, function(i) en_scale$clone())
    },

    setup_panel_params = function(self) {
        # Fudge for CoordFlip and CoordPolar - in place modification of
        # scales is not elegant, but it is pragmatic
        self$coord$modify_scales(self$panel_scales_x, self$panel_scales_y)

        scales_x <- self$panel_scales_x[self$layout$SCALE_X]
        scales_y <- self$panel_scales_y[self$layout$SCALE_Y]

        # Added for ggpsychro
        scales_rh <- self$panel_scales_rh[self$layout$SCALE_X]
        scales_wb <- self$panel_scales_wb[self$layout$SCALE_X]
        scales_vp <- self$panel_scales_vp[self$layout$SCALE_X]
        scales_sv <- self$panel_scales_sv[self$layout$SCALE_X]
        scales_en <- self$panel_scales_en[self$layout$SCALE_X]

        setup_panel_params <- function(scale_x, scale_y, scale_rh, scale_wb, scale_vp, scale_sv, scale_en) {
            self$coord$setup_panel_params(scale_x, scale_y, scale_rh, scale_wb, scale_vp, scale_sv, scale_en, params = self$coord_params)
        }
        self$panel_params <- Map(setup_panel_params, scales_x, scales_y, scales_rh, scales_wb, scales_vp, scales_sv, scales_en)

        invisible()
    }
)

create_layout <- function(facet = ggplot2::FacetNull, coord = ggplot2::CoordCartesian) {
    ggplot2::ggproto(NULL, PsyLayout, facet = facet, coord = coord)
}
