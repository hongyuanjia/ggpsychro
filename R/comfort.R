#' @include psychro-state.R stat-psychro-bin.R
NULL

#' Thermal comfort calculations
#'
#' These functions evaluate common comfort models without requiring Python at
#' runtime. Relative humidity is always supplied in percent. Temperature and
#' air-speed inputs follow `units`: SI uses degree C and m/s; IP uses degree F
#' and ft/s.
#'
#' @param tdb Dry-bulb air temperature.
#' @param tr Mean radiant temperature. Defaults to `tdb`.
#' @param vr Relative air speed for PMV.
#' @param v Air speed for SET and adaptive comfort.
#' @param rh Relative humidity in percent.
#' @param met Metabolic rate in met.
#' @param clo Clothing insulation in clo.
#' @param wme External work in met.
#' @param units Unit system, `"SI"` or `"IP"`.
#' @param limit_inputs If `TRUE`, values outside the model applicability range
#'   are returned as `NA`.
#' @param round_output If `TRUE`, round outputs like `pythermalcomfort`.
#'
#' @return A data frame with model outputs.
#'
#' @examples
#' comfort_pmv(25, rh = 50, met = 1.4, clo = 0.5)
#' comfort_set(25, rh = 50)
#' comfort_adaptive(25, t_running = 20)
#'
#' @export
comfort_pmv <- function(tdb, tr = tdb, vr = 0.1, rh, met = 1.2,
                        clo = 0.5, wme = 0, units = c("SI", "IP"),
                        limit_inputs = TRUE, round_output = TRUE) {
    units <- match.arg(units)
    x <- comfort_recycle(
        tdb = tdb, tr = tr, vr = vr, rh = rh, met = met, clo = clo, wme = wme
    )

    tdb_si <- comfort_to_si_temp(x$tdb, units)
    tr_si <- comfort_to_si_temp(x$tr, units)
    vr_si <- comfort_to_si_speed(x$vr, units)

    pmv <- comfort_pmv_vec(tdb_si, tr_si, vr_si, x$rh, x$met, x$clo, x$wme)

    ppd <- 100 - 95 * exp(-0.03353 * pmv^4 - 0.2179 * pmv^2)

    if (isTRUE(limit_inputs)) {
        valid <- comfort_between(tdb_si, 10, 30) &
            comfort_between(tr_si, 10, 40) &
            comfort_between(vr_si, 0, 1) &
            comfort_between(x$rh, 0, 100) &
            comfort_between(x$met, 0.8, 4) &
            comfort_between(x$clo, 0, 2) &
            comfort_between(pmv, -2, 2)
        pmv[!valid] <- NA_real_
        ppd[!valid] <- NA_real_
    }

    if (isTRUE(round_output)) {
        pmv <- round(pmv, 2L)
        ppd <- round(ppd, 1L)
    }

    new_data_frame(list(
        pmv = pmv,
        ppd = ppd,
        tsv = comfort_pmv_tsv(pmv)
    ))
}

#' @rdname comfort_pmv
#' @param body_surface_area Body surface area in square meters for SET.
#' @param p_atm Atmospheric pressure in Pa.
#' @param position Body position, `"standing"` or `"sitting"`.
#' @export
comfort_set <- function(tdb, tr = tdb, v = 0.1, rh, met = 1.2,
                        clo = 0.5, wme = 0, units = c("SI", "IP"),
                        limit_inputs = TRUE, round_output = TRUE,
                        body_surface_area = 1.8258, p_atm = 101325,
                        position = c("standing", "sitting")) {
    units <- match.arg(units)
    position <- match.arg(position)
    x <- comfort_recycle(
        tdb = tdb, tr = tr, v = v, rh = rh, met = met, clo = clo, wme = wme
    )

    tdb_si <- comfort_to_si_temp(x$tdb, units)
    tr_si <- comfort_to_si_temp(x$tr, units)
    v_si <- comfort_to_si_speed(x$v, units)

    set <- comfort_set_vec(
        tdb_si, tr_si, v_si, x$rh, x$met, x$clo, x$wme,
        body_surface_area = body_surface_area,
        p_atm = p_atm,
        position = position
    )

    if (isTRUE(limit_inputs)) {
        valid <- comfort_between(tdb_si, 10, 40) &
            comfort_between(tr_si, 10, 40) &
            comfort_between(v_si, 0, 2) &
            comfort_between(x$rh, 0, 100) &
            comfort_between(x$met, 1, 4) &
            comfort_between(x$clo, 0, 1.5)
        set[!valid] <- NA_real_
    }

    set <- comfort_from_si_temp(set, units)
    if (isTRUE(round_output)) {
        set <- round(set, 1L)
    }

    new_data_frame(list(set = set))
}

#' @rdname comfort_pmv
#' @param t_running Running mean outdoor temperature for adaptive comfort.
#' @param standard Adaptive comfort standard, either `"ashrae55"` or
#'   `"en16798"`.
#' @param category Comfort category. For ASHRAE 55 use `"80"` or `"90"`; for
#'   EN 16798 use `"I"`, `"II"`, or `"III"`.
#' @export
comfort_adaptive <- function(tdb, tr = tdb, t_running, v = 0.1,
                             standard = c("ashrae55", "en16798"),
                             category = NULL, units = c("SI", "IP"),
                             limit_inputs = TRUE, round_output = TRUE) {
    units <- match.arg(units)
    standard <- match.arg(standard)
    x <- comfort_recycle(tdb = tdb, tr = tr, t_running = t_running, v = v)

    tdb_si <- comfort_to_si_temp(x$tdb, units)
    tr_si <- comfort_to_si_temp(x$tr, units)
    t_running_si <- comfort_to_si_temp(x$t_running, units)
    v_si <- comfort_to_si_speed(x$v, units)

    if (standard == "ashrae55") {
        out <- comfort_adaptive_ashrae(
            tdb_si, tr_si, t_running_si, v_si,
            category = category, limit_inputs = limit_inputs,
            round_output = round_output
        )
    } else {
        out <- comfort_adaptive_en(
            tdb_si, tr_si, t_running_si, v_si,
            category = category, limit_inputs = limit_inputs,
            round_output = round_output
        )
    }

    temp_cols <- setdiff(names(out), c("standard", "acceptability",
        "acceptability_80", "acceptability_90",
        "acceptability_cat_i", "acceptability_cat_ii", "acceptability_cat_iii"
    ))
    for (col in temp_cols) {
        if (is.numeric(out[[col]])) {
            out[[col]] <- comfort_from_si_temp(out[[col]], units)
        }
    }

    out
}

#' Comfort model objects
#'
#' Model objects capture the fixed inputs used by comfort layers. They can be
#' reused across overlays, contours, zones, and point states.
#'
#' @inheritParams comfort_pmv
#' @param model PMV model/version label. Currently `"7730-2005"` is implemented.
#' @param round_output If `TRUE`, round model outputs. Comfort plot layers use
#'   unrounded values by default so contours and zones remain smooth.
#'
#' @return A comfort model object.
#' @export
comfort_model_pmv <- function(tr = NULL, vr = 0.1, met = 1.2, clo = 0.5,
                              wme = 0, model = "7730-2005",
                              limit_inputs = FALSE, round_output = FALSE) {
    model <- match.arg(model, "7730-2005")
    comfort_model(
        "pmv",
        list(tr = tr, vr = vr, met = met, clo = clo, wme = wme,
             model = model, limit_inputs = limit_inputs,
             round_output = round_output)
    )
}

#' @rdname comfort_model_pmv
#' @export
comfort_model_set <- function(tr = NULL, v = 0.1, met = 1.2, clo = 0.5,
                              wme = 0, limit_inputs = FALSE,
                              body_surface_area = 1.8258, p_atm = NULL,
                              position = c("standing", "sitting"),
                              round_output = FALSE) {
    position <- match.arg(position)
    comfort_model(
        "set",
        list(tr = tr, v = v, met = met, clo = clo, wme = wme,
             limit_inputs = limit_inputs, body_surface_area = body_surface_area,
             p_atm = p_atm, position = position, round_output = round_output)
    )
}

#' @rdname comfort_model_pmv
#' @param t_running Running mean outdoor temperature.
#' @param standard Adaptive comfort standard.
#' @param category Adaptive comfort category.
#' @export
comfort_model_adaptive <- function(t_running, tr = NULL, v = 0.1,
                                   standard = c("ashrae55", "en16798"),
                                   category = NULL, limit_inputs = TRUE,
                                   round_output = FALSE) {
    standard <- match.arg(standard)
    comfort_model(
        "adaptive",
        list(t_running = t_running, tr = tr, v = v, standard = standard,
             category = category, limit_inputs = limit_inputs,
             round_output = round_output)
    )
}

#' PMV-based comfort standards
#'
#' These helpers describe static PMV-based comfort zones. They are distinct from
#' adaptive comfort models such as [comfort_model_adaptive()], which use running
#' mean outdoor temperature and produce operative-temperature bands.
#'
#' @param range PMV comfort interval for ASHRAE 55.
#' @param breaks PMV boundaries for EN 15251 comfort bands.
#'
#' @return A comfort standard object.
#'
#' @export
comfort_standard_ashrae55_2017 <- function(range = c(-0.5, 0.5)) {
    range <- comfort_check_breaks(range, "`range`", n_min = 2L)
    if (length(range) != 2L) {
        stop("`range` must contain exactly two PMV boundaries.", call. = FALSE)
    }
    comfort_standard(
        "ashrae55_2017",
        breaks = range,
        fills = "#5BD96A",
        alphas = 0.58
    )
}

#' @rdname comfort_standard_ashrae55_2017
#' @export
comfort_standard_en15251_2007 <- function(breaks = c(-0.7, -0.2, 0.2, 0.7)) {
    breaks <- comfort_check_breaks(breaks, "`breaks`", n_min = 4L)
    if (length(breaks) != 4L) {
        stop("`breaks` must contain four PMV boundaries.", call. = FALSE)
    }
    comfort_standard(
        "en15251_2007",
        breaks = breaks,
        fills = c("#9BE89D", "#39D84A", "#9BE89D"),
        alphas = c(0.34, 0.58, 0.34)
    )
}

#' Comfort overlays for psychrometric charts
#'
#' `geom_comfort_overlay()` samples a psychrometric panel on a dry-bulb by
#' humidity-ratio grid and draws the selected comfort metric as filled contour
#' bands by default. The default model is ISO 7730 PMV.
#' `geom_comfort_contour()` draws PMV contours with root-traced curves by
#' default, and uses the same grid as the overlay for other metrics.
#' `geom_comfort_zone()` draws a comfort region, and `stat_comfort_state()`
#' evaluates comfort fields at supplied states.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @param model A comfort model object.
#' @param metric Comfort metric to draw. Defaults to `"pmv"` for PMV, `"set"`
#'   for SET, and `"acceptability"` for adaptive comfort.
#' @param n Grid resolution in dry-bulb and humidity-ratio directions. If
#'   `NULL`, a model-specific default is used.
#' @param method Overlay drawing method. `"auto"` uses root-traced filled bands
#'   for PMV and isobands for other metrics; `"rootband"` forces PMV root-traced
#'   bands; `"isoband"` draws filled contour bands; `"tile"` keeps the
#'   rectangular tile fallback.
#' @param levels Number of filled contour bands, or a numeric vector of band
#'   breaks. Ignored for `method = "tile"`.
#' @param gap Relative gap between generated tiles for `method = "tile"`.
#' @param alpha Overlay transparency. Defaults to `0.55` so psychrometric
#'   chart grid and relative-humidity curves remain visible beneath the
#'   comfort overlay. For [geom_comfort_standard_zone()], an `alpha` supplied
#'   through `...` overrides the standard-specific defaults.
#' @param breaks Contour break values.
#' @param contour_method Contour drawing method. `"auto"` uses root-traced
#'   curves for PMV and isobands for other metrics.
#' @param range Comfort value interval for PMV and SET zones.
#' @param standard A PMV-based comfort standard object.
#'
#' @export
geom_comfort_overlay <- function(mapping = NULL, data = NULL,
                                 stat = NULL,
                                 position = "identity", ...,
                                 model = comfort_model_pmv(),
                                 metric = NULL, n = NULL,
                                 method = c("auto", "rootband", "isoband", "tile"),
                                 levels = NULL, gap = 0, alpha = 0.55,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
    method <- match.arg(method)
    overlay_metric <- comfort_model_metric(model, metric)
    if (method == "auto") {
        method <- if (comfort_model_type(model) == "pmv" && overlay_metric == "pmv") {
            "rootband"
        } else {
            "isoband"
        }
    }
    if (is.null(stat)) {
        stat <- switch(method,
            rootband = StatComfortPmvRootBand,
            isoband = StatComfortBand,
            tile = StatComfortGrid
        )
    }
    geom <- if (method == "tile") GeomComfortTile else "polygon"
    params <- list(
        na.rm = na.rm, model = model, metric = metric, n = n,
        alpha = alpha, ...
    )
    if (method %in% c("rootband", "isoband")) {
        params$levels <- levels
        if (is.null(params$colour)) {
            params$colour <- NA
        }
    } else {
        params$gap <- gap
    }

    psychro_layer(
        stat = stat, data = comfort_layer_data(data), mapping = mapping,
        geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = params
    )
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_contour <- function(mapping = NULL, data = NULL,
                                 stat = StatComfortContour,
                                 position = "identity", ...,
                                 model = comfort_model_pmv(),
                                 metric = "pmv", breaks = NULL,
                                 n = NULL,
                                 contour_method = c("auto", "root", "isoband"),
                                 na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
    contour_method <- match.arg(contour_method)
    psychro_layer(
        stat = stat, data = comfort_layer_data(data), mapping = mapping, geom = "path",
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm, model = model, metric = metric,
            breaks = breaks, n = n, contour_method = contour_method, ...
        )
    )
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_zone <- function(mapping = NULL, data = NULL,
                              stat = StatComfortZone,
                              position = "identity", ...,
                              model = comfort_model_pmv(), metric = NULL,
                              range = NULL, n = NULL, gap = 0,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
    geom <- "polygon"
    params <- list(na.rm = na.rm, model = model, metric = metric, range = range,
        n = n, gap = gap, ...)
    if (is.null(params$colour)) {
        params$colour <- NA
    }

    psychro_layer(
        stat = stat, data = comfort_layer_data(data), mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = params
    )
}

#' @rdname geom_comfort_overlay
#' @param label_sensation If `TRUE`, label integer PMV curves with thermal
#'   sensation text.
#' @param label_axis If `TRUE`, label each PMV curve near the x-axis.
#' @param axis_label_hjust,axis_label_vjust Position adjustment for PMV numeric
#'   labels near the x-axis. By default, [ggplot2::waiver()] computes a shared
#'   baseline above the x-axis and offsets labels above their PMV lines.
#' @param sensation_label_hjust,sensation_label_vjust Position and vertical
#'   adjustment for thermal sensation labels.
#' @param axis_label_size,sensation_label_size Text size for PMV numeric and
#'   thermal sensation labels.
#' @param padding Gap padding around labels, passed as a grid unit.
#' @export
geom_comfort_pmv_lines <- function(mapping = NULL, data = NULL,
                                   position = "identity", ...,
                                   model = comfort_model_pmv(),
                                   levels = seq(-3, 3, by = 0.5),
                                   n = 360, label_sensation = TRUE,
                                   label_axis = TRUE,
                                   axis_label_hjust = ggplot2::waiver(),
                                   axis_label_vjust = ggplot2::waiver(),
                                   sensation_label_hjust = 0.5,
                                   sensation_label_vjust = 0.5,
                                   axis_label_size = NULL,
                                   sensation_label_size = NULL,
                                   padding = grid::unit(1, "pt"),
                                   na.rm = FALSE,
                                   show.legend = NA, inherit.aes = TRUE) {
    label <- hjust <- vjust <- NULL
    params <- list(...)
    text_size <- params$size
    params$size <- NULL
    if (is.null(axis_label_size)) {
        axis_label_size <- if (is.null(text_size)) 2.8 else text_size
    }
    if (is.null(sensation_label_size)) {
        sensation_label_size <- if (is.null(text_size)) 3 else text_size
    }

    params$na.rm <- na.rm
    params$model <- model
    params$levels <- levels
    params$n <- n

    if (is.null(params$colour) && is.null(params$color)) {
        params$colour <- "#4A4A4A"
    }
    if (is.null(params$linewidth)) {
        params$linewidth <- 0.45
    }

    sensation_levels <- comfort_pmv_sensation_levels(levels)
    line_levels <- if (isTRUE(label_sensation)) {
        setdiff(levels, sensation_levels)
    } else {
        levels
    }

    layers <- list()
    if (length(line_levels)) {
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve, data = comfort_layer_data(data),
            mapping = mapping, geom = "path", position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = utils::modifyList(params, list(
                levels = line_levels, label_type = "none"
            ))
        )
    }

    if (isTRUE(label_sensation) && length(sensation_levels)) {
        sensation_params <- params
        sensation_params$size <- sensation_label_size
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvCurve, data = comfort_layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            ),
            geom = geomtextpath::GeomTextpath, position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = utils::modifyList(sensation_params, list(
                levels = sensation_levels, label_type = "sensation",
                label_hjust = sensation_label_hjust,
                label_vjust = sensation_label_vjust,
                reverse = TRUE, gap = TRUE, padding = padding,
                upright = TRUE, remove_long = FALSE
            ))
        )
    }

    if (isTRUE(label_axis)) {
        axis_params <- params
        axis_params$size <- axis_label_size
        axis_params$linewidth <- NULL
        axis_params$linetype <- NULL
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortPmvAxisLabel, data = comfort_layer_data(data),
            mapping = ggplot2::aes(
                label = ggplot2::after_stat(label)
            ),
            geom = geomtextpath::GeomTextpath, position = position,
            show.legend = FALSE,
            inherit.aes = inherit.aes,
            params = utils::modifyList(axis_params, list(
                axis_label_hjust = axis_label_hjust,
                hjust = comfort_pmv_axis_label_text_hjust(axis_label_hjust),
                vjust = comfort_pmv_axis_label_text_vjust(
                    axis_label_vjust, axis_label_size
                ),
                text_only = TRUE, upright = TRUE,
                remove_long = FALSE
            ))
        )
    }

    layers
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_standard_zone <- function(standard = comfort_standard_ashrae55_2017(),
                                       mapping = NULL, data = NULL,
                                       position = "identity", ...,
                                       model = comfort_model_pmv(), n = 360,
                                       na.rm = FALSE, show.legend = NA,
                                       inherit.aes = TRUE) {
    label <- hjust <- vjust <- NULL
    standard <- comfort_check_standard(standard)
    params <- list(...)
    params$na.rm <- na.rm
    params$model <- model
    params$n <- n

    layers <- list()
    ranges <- cbind(
        standard$breaks[-length(standard$breaks)],
        standard$breaks[-1L]
    )
    alpha_override <- params$alpha
    for (i in seq_len(nrow(ranges))) {
        band_params <- params
        band_params$range <- ranges[i, ]
        band_params$fill <- standard$fills[[i]]
        band_params$alpha <- comfort_standard_alpha(
            alpha_override, standard$alphas, i
        )
        if (is.null(band_params$colour)) {
            band_params$colour <- NA
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortZone, data = comfort_layer_data(data),
            mapping = mapping, geom = "polygon", position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = band_params
        )
    }

    line_params <- params
    line_params$levels <- standard$breaks
    if (is.null(line_params$colour) && is.null(line_params$color)) {
        line_params$colour <- "#4A4A4A"
    }
    if (is.null(line_params$linewidth)) {
        line_params$linewidth <- 0.45
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve, data = comfort_layer_data(data),
        mapping = mapping, geom = "path", position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = c(line_params, list(label_type = "none"))
    )

    boundary_params <- line_params
    if (is.null(boundary_params$size)) {
        boundary_params$size <- 2.9
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve, data = comfort_layer_data(data),
        mapping = ggplot2::aes(
            label = ggplot2::after_stat(label),
            hjust = ggplot2::after_stat(hjust),
            vjust = ggplot2::after_stat(vjust)
        ),
        geom = geomtextpath::GeomTextpath, position = position,
        show.legend = FALSE, inherit.aes = FALSE,
        params = c(boundary_params, list(
            label_type = "boundary", text_only = TRUE, upright = TRUE,
            remove_long = TRUE
        ))
    )

    comfort_params <- line_params
    comfort_params$levels <- 0
    if (is.null(comfort_params$size)) {
        comfort_params$size <- 3.2
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortPmvCurve, data = comfort_layer_data(data),
        mapping = ggplot2::aes(
            label = ggplot2::after_stat(label),
            hjust = ggplot2::after_stat(hjust),
            vjust = ggplot2::after_stat(vjust)
        ),
        geom = geomtextpath::GeomTextpath, position = position,
        show.legend = FALSE, inherit.aes = FALSE,
        params = c(comfort_params, list(
            label_type = "comfort", text_only = TRUE, upright = TRUE,
            remove_long = TRUE
        ))
    )

    layers
}

#' @rdname geom_comfort_overlay
#' @export
stat_comfort_state <- function(mapping = NULL, data = NULL, geom = "point",
                               position = "identity", ...,
                               model = comfort_model_pmv(), na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE) {
    psychro_layer(
        stat = StatComfortState, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, model = model, ...)
    )
}

#' Comfort PMV fill scale
#'
#' A diverging blue-white-red fill scale centered on PMV 0.
#'
#' @param ... Passed to [ggplot2::scale_fill_gradient2()].
#' @param limits Scale limits.
#' @param low,mid,high Endpoint and midpoint colours.
#' @param midpoint Scale midpoint.
#' @param oob Out-of-bounds handler.
#'
#' @export
scale_fill_comfort_pmv <- function(..., limits = c(-3, 3),
                                   low = "#3B5FFF", mid = "#F7F7F7",
                                   high = "#FF3B30", midpoint = 0,
                                   oob = scales::squish) {
    ggplot2::scale_fill_gradient2(
        ..., low = low, mid = mid, high = high,
        midpoint = midpoint, limits = limits, oob = oob
    )
}

GeomComfortTile <- ggplot2::ggproto(
    "GeomComfortTile", ggplot2::GeomTile
)

#' @noRd
StatComfortBand <- ggplot2::ggproto(
    "StatComfortBand", ggplot2::Stat,

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
        "na.rm", "model", "metric", "n", "levels", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, n = NULL, levels = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_band_data(
            model, metric, levels, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGrid <- ggplot2::ggproto(
    "StatComfortGrid", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = ggplot2::after_stat(value),
        width = ggplot2::after_stat(width),
        height = ggplot2::after_stat(height),
        alpha = 0.55
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "n", "gap", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, n = NULL, gap = 0,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_grid_data(
            model, metric, comfort_default_n(model, n), gap, units, pres,
            mollier, tdb_lim, hum_lim, na.rm = na.rm
        )
    }
)

#' @noRd
StatComfortContour <- ggplot2::ggproto(
    "StatComfortContour", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(colour = ggplot2::after_stat(level)),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "breaks", "n", "contour_method",
        "units", "pres", "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = "pmv", breaks = NULL,
                             n = NULL, contour_method = "auto", units, pres,
                             mollier = FALSE, tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_contour_data(
            model, metric, breaks, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim, contour_method = contour_method
        )
    }
)

#' @noRd
StatComfortPmvCurve <- ggplot2::ggproto(
    "StatComfortPmvCurve", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "levels", "n", "label_type", "label_hjust",
        "label_vjust", "reverse", "units", "pres", "mollier", "tdb_lim",
        "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             levels = seq(-3, 3, by = 0.5), n = 360,
                             label_type = c("none", "sensation", "boundary", "comfort"),
                             label_hjust = NULL, label_vjust = NULL,
                             reverse = FALSE,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        label_type <- match.arg(label_type)
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_pmv_curve_data(
            model, levels, n, units, pres, mollier, tdb_lim, hum_lim,
            label = label_type, label_hjust = label_hjust,
            label_vjust = label_vjust, reverse = reverse
        )
    }
)

#' @noRd
StatComfortPmvAxisLabel <- ggplot2::ggproto(
    "StatComfortPmvAxisLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "levels", "n", "axis_label_hjust",
        "units", "pres", "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             levels = seq(-3, 3, by = 0.5), n = 360,
                             axis_label_hjust = ggplot2::waiver(),
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_pmv_axis_label_data(
            model, levels, n, units, pres, mollier, tdb_lim, hum_lim,
            axis_label_hjust = axis_label_hjust
        )
    }
)

#' @noRd
StatComfortPmvRootBand <- ggplot2::ggproto(
    "StatComfortPmvRootBand", ggplot2::Stat,

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
        "na.rm", "model", "metric", "levels", "n", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, levels = NULL, n = NULL,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_pmv_rootband_data(
            model, metric, levels, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortZone <- ggplot2::ggproto(
    "StatComfortZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
        fill = "#70B77E",
        alpha = 0.2,
        subgroup = ggplot2::after_stat(subgroup)
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "metric", "range", "n", "gap", "units",
        "pres", "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, range = NULL,
                             n = NULL, gap = 0, units, pres,
                             mollier = FALSE, tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_zone_data(
            model, metric, range, comfort_default_n(model, n), gap, units,
            pres, mollier, tdb_lim, hum_lim, na.rm = na.rm
        )
    }
)

#' @noRd
StatComfortState <- ggplot2::ggproto(
    "StatComfortState", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    required_aes = c("tdb"),

    optional_aes = psychro_state_properties(),

    extra_params = c("na.rm", "model", "units", "pres", "mollier"),

    compute_group = function(self, data, scales, model = comfort_model_pmv(),
                             units, pres, mollier = FALSE,
                             na.rm = FALSE) {
        units <- get_units(data)
        pres <- comfort_stat_pressure(data, pres)
        data <- psychro_compute_state(data, units, pres, mollier, na.rm = na.rm)
        if (!nrow(data)) {
            return(data)
        }

        rh <- comfort_relhum_from_humratio(data$tdb, data$humratio, units, pres)
        result <- comfort_apply_model(model, data$tdb, rh, units, pres)
        cbind(data, result)
    }
)

comfort_model <- function(type, params) {
    structure(
        list(type = type, params = params),
        class = c("PsyComfortModel", "list")
    )
}

comfort_model_type <- function(model) {
    comfort_check_model(model)
    model$type
}

comfort_check_model <- function(model) {
    if (!inherits(model, "PsyComfortModel") ||
            !model$type %in% c("pmv", "set", "adaptive")) {
        stop("`model` must be created by comfort_model_*().", call. = FALSE)
    }
    invisible(model)
}

comfort_standard <- function(name, breaks, fills, alphas) {
    structure(
        list(name = name, breaks = breaks, fills = fills, alphas = alphas),
        class = c("PsyComfortStandard", "list")
    )
}

comfort_standard_alpha <- function(override, defaults, i) {
    if (is.null(override)) {
        return(defaults[[i]])
    }
    override <- suppressWarnings(as.numeric(override))
    if (!length(override) || any(!is.finite(override))) {
        stop("`alpha` must be finite when supplied.", call. = FALSE)
    }
    rep(override, length.out = length(defaults))[[i]]
}

comfort_check_standard <- function(standard) {
    if (!inherits(standard, "PsyComfortStandard") ||
            !standard$name %in% c("ashrae55_2017", "en15251_2007")) {
        stop("`standard` must be created by comfort_standard_*().",
            call. = FALSE)
    }
    standard
}

comfort_check_breaks <- function(x, name, n_min = 2L) {
    x <- sort(unique(as.numeric(x)))
    if (length(x) < n_min || any(!is.finite(x))) {
        stop(name, " must contain finite increasing PMV boundaries.",
            call. = FALSE)
    }
    x
}

comfort_layer_data <- function(data) {
    if (is.null(data)) {
        return(new_data_frame(list(.comfort = 1), n = 1L))
    }
    data
}

comfort_recycle <- function(...) {
    args <- lapply(list(...), as.numeric)
    lens <- vapply(args, length, integer(1L))
    n <- max(lens)
    bad <- lens != 1L & lens != n
    if (any(bad)) {
        stop("Comfort inputs must have compatible lengths.", call. = FALSE)
    }
    lapply(args, rep, length.out = n)
}

comfort_between <- function(x, lower, upper) {
    is.finite(x) & x >= lower & x <= upper
}

comfort_to_si_temp <- function(x, units) {
    if (units == "IP") get_c_from_f(x) else x
}

comfort_from_si_temp <- function(x, units) {
    if (units == "IP") get_f_from_c(x) else x
}

comfort_to_si_speed <- function(x, units) {
    if (units == "IP") x / 3.281 else x
}

comfort_pressure_pa <- function(p_atm, units) {
    if (units == "IP") p_atm * 6894.757293168 else p_atm
}

comfort_pmv_vec <- function(tdb, tr, vr, rh, met, clo, wme) {
    .Call(
        C_comfort_pmv_vec,
        as.numeric(tdb), as.numeric(tr), as.numeric(vr), as.numeric(rh),
        as.numeric(met), as.numeric(clo), as.numeric(wme)
    )
}

comfort_set_vec <- function(tdb, tr, v, rh, met, clo, wme,
                            body_surface_area, p_atm, position) {
    .Call(
        C_comfort_set_vec,
        as.numeric(tdb), as.numeric(tr), as.numeric(v), as.numeric(rh),
        as.numeric(met), as.numeric(clo), as.numeric(wme),
        as.numeric(body_surface_area), as.numeric(p_atm),
        isTRUE(position == "sitting")
    )
}

comfort_pmv_one <- function(tdb, tr, vr, rh, met, clo, wme) {
    if (!all(is.finite(c(tdb, tr, vr, rh, met, clo, wme)))) {
        return(NA_real_)
    }

    pa <- rh * 10 * exp(16.6536 - 4030.183 / (tdb + 235))
    icl <- 0.155 * clo
    m <- met * 58.15
    w <- wme * 58.15
    mw <- m - w
    f_cl <- if (icl <= 0.078) 1 + 1.29 * icl else 1.05 + 0.645 * icl
    hcf <- 12.1 * sqrt(vr)
    taa <- tdb + 273
    tra <- tr + 273
    t_cla <- taa + (35.5 - tdb) / (3.5 * icl + 0.1)

    p1 <- icl * f_cl
    p2 <- p1 * 3.96
    p3 <- p1 * 100
    p4 <- p1 * taa
    p5 <- (308.7 - 0.028 * mw) + p2 * (tra / 100)^4
    xn <- t_cla / 100
    xf <- t_cla / 50

    i <- 0L
    while (abs(xn - xf) > 0.00015 && i < 150L) {
        xf <- (xf + xn) / 2
        hcn <- 2.38 * abs(100 * xf - taa)^0.25
        hc <- max(hcf, hcn)
        xn <- (p5 + p4 * hc - p2 * xf^4) / (100 + p3 * hc)
        i <- i + 1L
    }

    tcl <- 100 * xn - 273
    hl1 <- 3.05e-3 * (5733 - 6.99 * mw - pa)
    hl2 <- if (mw > 58.15) 0.42 * (mw - 58.15) else 0
    hl3 <- 1.7e-5 * m * (5867 - pa)
    hl4 <- 0.0014 * m * (34 - tdb)
    hl5 <- 3.96 * f_cl * (xn^4 - (tra / 100)^4)
    hl6 <- f_cl * hc * (tcl - tdb)
    ts <- 0.303 * exp(-0.036 * m) + 0.028

    ts * (mw - hl1 - hl2 - hl3 - hl4 - hl5 - hl6)
}

comfort_pmv_tsv <- function(pmv) {
    labels <- c(
        "Cold", "Cool", "Slightly Cool", "Neutral",
        "Slightly Warm", "Warm", "Hot"
    )
    out <- labels[findInterval(pmv, c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)) + 1L]
    out[is.na(pmv)] <- NA_character_
    out
}

comfort_set_one <- function(tdb, tr, v, rh, met, clo, wme,
                            body_surface_area, p_atm, position) {
    if (!all(is.finite(c(tdb, tr, v, rh, met, clo, wme,
            body_surface_area, p_atm)))) {
        return(NA_real_)
    }

    air_speed <- max(v, 0.1)
    k_clo <- 0.25
    body_weight <- 70
    met_factor <- 58.2
    sbc <- 5.6697e-8
    c_sw <- 170
    c_dil <- 120
    c_str <- 0.5
    temp_skin_neutral <- 33.7
    temp_core_neutral <- 36.8
    temp_body_neutral <- 36.49
    skin_blood_flow_neutral <- 6.3

    temp_skin <- temp_skin_neutral
    temp_core <- temp_core_neutral
    m_bl <- skin_blood_flow_neutral
    rm <- (met - wme) * met_factor
    m <- met * met_factor
    pressure_in_atmospheres <- p_atm / 101325
    vapor_pressure <- rh * comfort_p_sat_torr(tdb) / 100
    length_time_simulation <- 60
    n_simulation <- 1L
    alfa <- 0.1
    e_skin <- 0.1 * met
    r_clo <- 0.155 * clo
    f_a_cl <- 1 + 0.15 * clo
    lr <- 2.2 / pressure_in_atmospheres

    if (clo <= 0) {
        w_max <- 0.38 * air_speed^(-0.29)
        i_cl <- 1
    } else {
        w_max <- 0.59 * air_speed^(-0.08)
        i_cl <- 0.45
    }

    h_cc <- 3 * pressure_in_atmospheres^0.53
    h_fc <- 8.600001 * (air_speed * pressure_in_atmospheres)^0.53
    h_cc <- max(h_cc, h_fc)
    if (met > 0.85) {
        h_cc <- max(h_cc, 5.66 * (met - 0.85)^0.39)
    }
    h_r <- 4.7
    h_t <- h_r + h_cc
    r_a <- 1 / (f_a_cl * h_t)
    t_op <- (h_r * tr + h_cc * tdb) / h_t
    temp_body <- alfa * temp_skin + (1 - alfa) * temp_core
    q_res <- 0.0023 * m * (44 - vapor_pressure)
    c_res <- 0.0014 * m * (34 - tdb)
    q_sensible <- 0
    e_rsw <- 0
    e_max <- 0
    w <- 0

    while (n_simulation < length_time_simulation) {
        n_simulation <- n_simulation + 1L

        t_cl <- (r_a * temp_skin + r_clo * t_op) / (r_a + r_clo)
        for (i in seq_len(150L)) {
            if (isTRUE(position == "sitting")) {
                h_r <- 4 * 0.95 * sbc * ((t_cl + tr) / 2 + 273.15)^3 * 0.7
            } else {
                h_r <- 4 * 0.95 * sbc * ((t_cl + tr) / 2 + 273.15)^3 * 0.73
            }
            h_t <- h_r + h_cc
            r_a <- 1 / (f_a_cl * h_t)
            t_op <- (h_r * tr + h_cc * tdb) / h_t
            t_cl_new <- (r_a * temp_skin + r_clo * t_op) / (r_a + r_clo)
            if (abs(t_cl_new - t_cl) <= 0.01) {
                t_cl <- t_cl_new
                break
            }
            t_cl <- t_cl_new
        }

        q_sensible <- (temp_skin - t_op) / (r_a + r_clo)
        hf_cs <- (temp_core - temp_skin) * (5.28 + 1.163 * m_bl)
        s_core <- m - hf_cs - q_res - c_res - wme
        s_skin <- hf_cs - q_sensible - e_skin
        tc_sk <- 0.97 * alfa * body_weight
        tc_cr <- 0.97 * (1 - alfa) * body_weight
        d_t_sk <- (s_skin * body_surface_area) / (tc_sk * 60)
        d_t_cr <- (s_core * body_surface_area) / (tc_cr * 60)
        temp_skin <- temp_skin + d_t_sk
        temp_core <- temp_core + d_t_cr
        temp_body <- alfa * temp_skin + (1 - alfa) * temp_core
        sk_sig <- temp_skin - temp_skin_neutral
        warm_sk <- max(sk_sig, 0)
        cold_sk <- max(-sk_sig, 0)
        c_reg_sig <- temp_core - temp_core_neutral
        c_warm <- max(c_reg_sig, 0)
        c_cold <- max(-c_reg_sig, 0)
        bdsig <- temp_body - temp_body_neutral
        warm_b <- max(bdsig, 0)
        m_bl <- (skin_blood_flow_neutral + c_dil * c_warm) /
            (1 + c_str * cold_sk)
        m_bl <- max(0.5, min(90, m_bl))
        reg_sw <- c_sw * warm_b * exp(warm_sk / 10.7)
        reg_sw <- min(reg_sw, 500)
        e_rsw <- 0.68 * reg_sw
        r_ea <- 1 / (lr * f_a_cl * h_cc)
        r_ecl <- r_clo / (lr * i_cl)
        e_req <- rm - q_res - c_res - q_sensible
        e_max <- (comfort_p_sat_torr(temp_skin) - vapor_pressure) / (r_ea + r_ecl)
        if (e_max == 0) {
            e_max <- 0.001
        }
        pr_sw <- e_rsw / e_max
        w <- 0.06 + 0.94 * pr_sw
        e_diff <- w * e_max - e_rsw
        if (w > w_max) {
            w <- w_max
            pr_sw <- w_max / 0.94
            e_rsw <- pr_sw * e_max
            e_diff <- 0.06 * (1 - pr_sw) * e_max
        }
        if (e_max < 0) {
            e_diff <- 0
            e_rsw <- 0
            w <- w_max
        }
        e_skin <- e_rsw + e_diff
        m_shiv <- 19.4 * cold_sk * c_cold
        m <- rm + m_shiv
        alfa <- 0.0417737 + 0.7451833 / (m_bl + 0.585417)
    }

    q_skin <- q_sensible + e_skin
    p_ssk <- comfort_p_sat_torr(temp_skin)
    h_r_s <- h_r
    h_c_s <- 3 * pressure_in_atmospheres^0.53
    if (met > 0.85) {
        h_c_s <- max(h_c_s, 5.66 * (met - 0.85)^0.39)
    }
    h_c_s <- max(h_c_s, 3.0)
    h_t_s <- h_c_s + h_r_s
    r_clo_s <- 1.52 / ((met - wme / met_factor) + 0.6944) - 0.1835
    r_clo_s <- max(r_clo_s, 0)
    r_cl_s <- 0.155 * r_clo_s
    f_a_cl_s <- 1 + k_clo * r_clo_s
    fcls <- 1 / (1 + 0.155 * f_a_cl_s * h_t_s * r_clo_s)
    ims <- 0.45
    i_m_s <- ims * h_c_s / h_t_s * (1 - fcls) / (h_c_s / h_t_s - fcls * ims)
    r_a_s <- 1 / (f_a_cl_s * h_t_s)
    r_ea_s <- 1 / (lr * f_a_cl_s * h_c_s)
    r_ecl_s <- r_cl_s / (lr * i_m_s)
    h_d_s <- 1 / (r_a_s + r_cl_s)
    h_e_s <- 1 / (r_ea_s + r_ecl_s)

    delta <- 0.0001
    dx <- 100
    set_old <- round(temp_skin - q_skin / h_d_s, 2L)
    while (abs(dx) > 0.01) {
        err_1 <- q_skin - h_d_s * (temp_skin - set_old) -
            w * h_e_s * (p_ssk - 0.5 * comfort_p_sat_torr(set_old))
        err_2 <- q_skin - h_d_s * (temp_skin - (set_old + delta)) -
            w * h_e_s * (p_ssk - 0.5 * comfort_p_sat_torr(set_old + delta))
        set_new <- set_old - delta * err_1 / (err_2 - err_1)
        dx <- set_new - set_old
        set_old <- set_new
    }

    set_old
}

comfort_p_sat_torr <- function(tdb) {
    exp(18.6686 - 4030.183 / (tdb + 235))
}

comfort_adaptive_ashrae <- function(tdb, tr, t_running, v, category,
                                    limit_inputs, round_output) {
    category <- comfort_adaptive_category(category, c("80", "90"), "80")
    to <- comfort_operative_temp(tdb, tr, v, standard = "ashrae")
    t_cmf <- 0.31 * t_running + 17.8
    if (isTRUE(round_output)) {
        t_cmf <- round(t_cmf, 1L)
    }
    ce <- comfort_adaptive_cooling_effect(v, to)

    out <- new_data_frame(list(
        standard = rep("ashrae55", length(tdb)),
        tmp_cmf = t_cmf,
        tmp_cmf_80_low = t_cmf - 3.5,
        tmp_cmf_80_up = t_cmf + 3.5 + ce,
        tmp_cmf_90_low = t_cmf - 2.5,
        tmp_cmf_90_up = t_cmf + 2.5 + ce
    ))
    out$acceptability_80 <- to >= out$tmp_cmf_80_low & to <= out$tmp_cmf_80_up
    out$acceptability_90 <- to >= out$tmp_cmf_90_low & to <= out$tmp_cmf_90_up
    out$lower <- out[[paste0("tmp_cmf_", category, "_low")]]
    out$upper <- out[[paste0("tmp_cmf_", category, "_up")]]
    out$acceptability <- out[[paste0("acceptability_", category)]]

    if (isTRUE(limit_inputs)) {
        valid <- comfort_between(tdb, 10, 40) &
            comfort_between(tr, 10, 40) &
            comfort_between(v, 0, 2) &
            comfort_between(t_running, 10, 33.5)
        out[!valid, names(out) != "standard"] <- NA
    }
    if (isTRUE(round_output)) {
        temp_cols <- names(out)[vapply(out, is.numeric, logical(1L))]
        out[temp_cols] <- lapply(out[temp_cols], round, digits = 1L)
    }
    out
}

comfort_adaptive_en <- function(tdb, tr, t_running, v, category,
                                limit_inputs, round_output) {
    category <- comfort_adaptive_category(category, c("I", "II", "III"), "II")
    category_key <- switch(category, I = "cat_i", II = "cat_ii", III = "cat_iii")
    to <- comfort_operative_temp(tdb, tr, v, standard = "iso")
    t_cmf <- 0.33 * t_running + 18.8
    ce <- comfort_adaptive_cooling_effect(v, to)

    out <- new_data_frame(list(
        standard = rep("en16798", length(tdb)),
        tmp_cmf = t_cmf,
        tmp_cmf_cat_i_low = t_cmf - 3.0,
        tmp_cmf_cat_i_up = t_cmf + 2.0 + ce,
        tmp_cmf_cat_ii_low = t_cmf - 4.0,
        tmp_cmf_cat_ii_up = t_cmf + 3.0 + ce,
        tmp_cmf_cat_iii_low = t_cmf - 5.0,
        tmp_cmf_cat_iii_up = t_cmf + 4.0 + ce
    ))
    out$acceptability_cat_i <- to >= out$tmp_cmf_cat_i_low & to <= out$tmp_cmf_cat_i_up
    out$acceptability_cat_ii <- to >= out$tmp_cmf_cat_ii_low & to <= out$tmp_cmf_cat_ii_up
    out$acceptability_cat_iii <- to >= out$tmp_cmf_cat_iii_low & to <= out$tmp_cmf_cat_iii_up
    out$lower <- out[[paste0("tmp_cmf_", category_key, "_low")]]
    out$upper <- out[[paste0("tmp_cmf_", category_key, "_up")]]
    out$acceptability <- out[[paste0("acceptability_", category_key)]]

    if (isTRUE(limit_inputs)) {
        valid <- comfort_between(tdb, 10, 40) &
            comfort_between(tr, 10, 40) &
            comfort_between(v, 0, 2) &
            comfort_between(t_running, 10, 33.5)
        out[!valid, names(out) != "standard"] <- NA
    }
    if (isTRUE(round_output)) {
        temp_cols <- names(out)[vapply(out, is.numeric, logical(1L))]
        out[temp_cols] <- lapply(out[temp_cols], round, digits = 1L)
    }
    out
}

comfort_adaptive_category <- function(category, choices, default) {
    if (is.null(category)) {
        return(default)
    }
    category <- as.character(category)
    match.arg(category, choices)
}

comfort_operative_temp <- function(tdb, tr, v, standard) {
    if (standard == "iso") {
        out <- rep(NA_real_, length(v))
        valid <- is.finite(tdb) & is.finite(tr) & is.finite(v) & v >= 0
        if (any(valid)) {
            speed_weight <- sqrt(10 * v[valid])
            out[valid] <- (tdb[valid] * speed_weight + tr[valid]) /
                (1 + speed_weight)
        }
        return(out)
    }

    a <- ifelse(v < 0.2, 0.5, ifelse(v < 0.6, 0.6, 0.7))
    a * tdb + (1 - a) * tr
}

comfort_adaptive_cooling_effect <- function(v, to) {
    ce <- numeric(length(v))
    active <- is.finite(to) & is.finite(v) & to >= 25 & v >= 0.6
    ce[active] <- 1.2
    ce[active & v >= 0.9] <- 1.8
    ce[active & v >= 1.2] <- 2.2
    ce
}

comfort_stat_units <- function(data, units) {
    if ("units" %in% names(data)) {
        get_units(data)
    } else {
        match.arg(units, c("SI", "IP"))
    }
}

comfort_stat_pressure <- function(data, pres) {
    if ("pres" %in% names(data)) {
        pres <- unique(data$pres)
    }
    if (length(pres) != 1L || !is.finite(pres)) {
        stop("`pres` must resolve to a single finite pressure value.", call. = FALSE)
    }
    pres
}

comfort_grid_n <- function(n) {
    if (!is.numeric(n) || length(n) < 1L || length(n) > 2L ||
            any(!is.finite(n)) || any(n < 2)) {
        stop("`n` must be one or two finite numbers greater than 1.", call. = FALSE)
    }
    as.integer(rep(n, length.out = 2L))
}

comfort_default_n <- function(model, n = NULL) {
    if (!is.null(n)) {
        return(n)
    }
    switch(comfort_model_type(model),
        pmv = c(360L, 220L),
        set = c(80L, 50L),
        adaptive = c(240L, 160L)
    )
}

comfort_pmv_curve_n <- function(n) {
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 8) {
        stop("`n` must be a single finite number greater than or equal to 8.",
            call. = FALSE)
    }
    as.integer(n)
}

comfort_grid_limits <- function(units, tdb_lim, hum_lim) {
    default <- default_psychro_limits(units)
    list(
        tdb = if (is.null(tdb_lim)) default$tdb else tdb_lim,
        hum = if (is.null(hum_lim)) default$hum else hum_lim
    )
}

comfort_grid_matrix <- function(model, metric, n, units, pres, tdb_lim, hum_lim,
                                at = c("centers", "nodes"),
                                boundary = c("na", "saturation")) {
    at <- match.arg(at)
    boundary <- match.arg(boundary)
    n <- comfort_grid_n(n)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    tdb_edges <- seq(lim$tdb[[1L]], lim$tdb[[2L]], length.out = n[[1L]] + 1L)
    hum_display_edges <- seq(lim$hum[[1L]], lim$hum[[2L]],
        length.out = n[[2L]] + 1L)
    humratio_edges <- narrow_hum(hum_display_edges, units)
    if (at == "nodes") {
        tdb <- tdb_edges
        humratio <- humratio_edges
    } else {
        tdb <- (tdb_edges[-1L] + tdb_edges[-length(tdb_edges)]) / 2
        humratio <- (humratio_edges[-1L] + humratio_edges[-length(humratio_edges)]) / 2
    }
    grid <- expand.grid(tdb = tdb, humratio = humratio)

    humratio_eval <- grid$humratio
    if (boundary == "saturation") {
        saturation <- psychro_saturation_humratio(grid$tdb, units, pres)
        saturation_eps <- pmax(abs(saturation), 1) * sqrt(.Machine$double.eps)
        humratio_eval <- pmin(humratio_eval, saturation - saturation_eps)
        humratio_eval <- pmax(humratio_eval, 0)
    }

    rh <- comfort_relhum_from_humratio(grid$tdb, humratio_eval, units, pres)
    metric <- comfort_model_metric(model, metric)
    result <- comfort_apply_model(model, grid$tdb, rh, units, pres)
    value <- comfort_metric_value(result, metric)
    value[!comfort_valid_grid_rh(rh)] <- NA_real_
    matrix_value <- matrix(value, nrow = length(tdb), ncol = length(humratio))

    list(
        tdb = tdb,
        humratio = humratio,
        tdb_edges = tdb_edges,
        humratio_edges = humratio_edges,
        value = matrix_value,
        metric = metric
    )
}

comfort_grid_data <- function(model, metric, n, gap, units, pres, mollier,
                              tdb_lim, hum_lim, na.rm = FALSE) {
    m <- comfort_grid_matrix(model, metric, n, units, pres, tdb_lim, hum_lim)
    gap <- psychro_bin_gap(gap)
    sat <- psychro_saturation_humratio(m$tdb_edges, units, pres)

    nx <- length(m$tdb)
    ny <- length(m$humratio)
    ix <- rep(seq_len(nx), times = ny)
    iy <- rep(seq_len(ny), each = nx)

    x0 <- m$tdb_edges[ix]
    x1 <- m$tdb_edges[ix + 1L]
    y0 <- m$humratio_edges[iy]
    y1 <- m$humratio_edges[iy + 1L]
    s0 <- sat[ix]
    s1 <- sat[ix + 1L]
    x_width <- x1 - x0
    y_height <- y1 - y0

    keep <- is.finite(s0) & is.finite(s1) &
        x_width > 0 & y_height > 0 &
        y0 < pmax(s0, s1)
    if (!any(keep)) {
        return(comfort_empty_tile())
    }

    value <- as.vector(m$value)
    missing <- keep & !is.finite(value)
    if (any(missing)) {
        value[missing] <- comfort_grid_boundary_values(
            model, m$metric, units, pres,
            x0[missing], x1[missing], y0[missing], y1[missing],
            s0[missing], s1[missing]
        )
    }

    keep <- keep & is.finite(value)
    if (!any(keep)) {
        return(comfort_empty_tile())
    }

    out <- new_data_frame(list(
        tdb = (x0[keep] + x1[keep]) / 2,
        humratio = (y0[keep] + y1[keep]) / 2,
        width = x_width[keep] * (1 - gap),
        height = y_height[keep] * (1 - gap),
        value = value[keep],
        metric = rep(m$metric, sum(keep)),
        group = seq_len(sum(keep))
    ))
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_pmv_curve_data <- function(model, levels, n, units, pres, mollier,
                                   tdb_lim, hum_lim,
                                   label = c("none", "sensation", "boundary",
                                       "comfort"),
                                   label_hjust = NULL, label_vjust = NULL,
                                   reverse = FALSE) {
    label <- match.arg(label)
    levels <- comfort_check_breaks(levels, "`levels`", n_min = 1L)
    n <- comfort_pmv_curve_n(n)
    model <- comfort_pmv_curve_model(model)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    humratio <- seq(
        narrow_hum(lim$hum[[1L]], units),
        narrow_hum(lim$hum[[2L]], units),
        length.out = n
    )

    curves <- vector("list", length(levels))
    for (i in seq_along(levels)) {
        roots <- comfort_pmv_curve_roots(
            model, levels[[i]], humratio, lim$tdb, units, pres
        )
        sat_roots <- comfort_pmv_curve_saturation_roots(
            model, levels[[i]], lim$tdb, lim$hum, units, pres, n
        )
        roots <- comfort_pmv_curve_merge_roots(roots, sat_roots)
        if (!length(roots$tdb)) {
            next
        }
        curves[[i]] <- new_data_frame(list(
            tdb = roots$tdb,
            humratio = roots$humratio,
            level = levels[[i]],
            value = levels[[i]],
            group = i,
            linetype = comfort_pmv_linetype(levels[[i]]),
            label = comfort_pmv_curve_label(levels[[i]], label),
            hjust = comfort_pmv_curve_hjust(label, label_hjust),
            vjust = comfort_pmv_curve_vjust(
                levels[[i]], label, label_vjust, mollier = mollier
            ),
            metric = "pmv"
        ))
    }
    curves <- curves[!vapply(curves, is.null, logical(1L))]
    if (!length(curves)) {
        return(comfort_empty_pmv_curve())
    }
    out <- do.call(rbind, curves)
    row.names(out) <- NULL

    if (label != "none") {
        out <- out[!is.na(out$label), , drop = FALSE]
    }
    if (!nrow(out)) {
        return(comfort_empty_pmv_curve())
    }
    if (isTRUE(reverse)) {
        out <- comfort_pmv_reverse_groups(out)
    }
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_pmv_sensation_levels <- function(levels) {
    levels[!is.na(vapply(levels, comfort_pmv_sensation_label, character(1L)))]
}

comfort_pmv_axis_label_data <- function(model, levels, n, units, pres,
                                        mollier, tdb_lim, hum_lim,
                                        axis_label_hjust = ggplot2::waiver()) {
    levels <- comfort_check_breaks(levels, "`levels`", n_min = 1L)
    n <- comfort_pmv_curve_n(n)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    hum_lim_narrow <- narrow_hum(lim$hum, units)
    label_start <- hum_lim_narrow[[1L]] +
        diff(hum_lim_narrow) * comfort_pmv_axis_label_offset(axis_label_hjust)
    label_end <- hum_lim_narrow[[1L]] +
        diff(hum_lim_narrow) * comfort_pmv_axis_label_end(axis_label_hjust)
    curves <- comfort_pmv_curve_data(
        model, levels, n, units, pres, FALSE, tdb_lim, hum_lim, label = "none"
    )
    if (!nrow(curves)) {
        return(comfort_empty_pmv_curve())
    }

    labels <- vector("list", length(levels))
    for (i in seq_along(levels)) {
        curve <- curves[curves$level == levels[[i]], , drop = FALSE]
        curve <- curve[order(curve$humratio, curve$tdb), , drop = FALSE]
        segment <- comfort_pmv_axis_label_segment(
            curve, label_start, label_end
        )
        if (is.null(segment)) {
            next
        }
        labels[[i]] <- new_data_frame(list(
            tdb = segment$tdb,
            humratio = segment$humratio,
            level = levels[[i]],
            value = levels[[i]],
            group = i,
            label = comfort_format_pmv_level(levels[[i]]),
            hjust = comfort_pmv_axis_label_text_hjust(axis_label_hjust),
            vjust = 0.5,
            metric = "pmv"
        ))
    }
    labels <- labels[!vapply(labels, is.null, logical(1L))]
    if (!length(labels)) {
        return(comfort_empty_pmv_curve())
    }
    out <- do.call(rbind, labels)
    row.names(out) <- NULL
    out <- comfort_pmv_reverse_groups(out)
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_pmv_axis_label_segment <- function(curve, label_start, label_end) {
    if (nrow(curve) < 2L || !is.finite(label_start) ||
        !is.finite(label_end) || label_end <= label_start) {
        return(NULL)
    }

    curve <- curve[order(curve$humratio, curve$tdb), , drop = FALSE]
    if (label_start < min(curve$humratio) || label_start > max(curve$humratio)) {
        return(NULL)
    }

    label_end <- min(label_end, max(curve$humratio))
    if (label_end <= label_start) {
        return(NULL)
    }

    humratio <- unique(c(
        label_start,
        curve$humratio[curve$humratio > label_start &
            curve$humratio < label_end],
        label_end
    ))
    tdb <- stats::approx(
        curve$humratio, curve$tdb, xout = humratio,
        rule = 2, ties = "ordered"
    )$y
    keep <- is.finite(tdb) & is.finite(humratio)
    if (sum(keep) < 2L) {
        return(NULL)
    }

    list(tdb = tdb[keep], humratio = humratio[keep])
}

comfort_pmv_axis_label_text_hjust <- function(axis_label_hjust) {
    if (comfort_is_waiver(axis_label_hjust)) {
        return(0.95)
    }
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(1 - max(0, min(0.2, axis_label_hjust[[1L]])))
    }
    0.95
}

comfort_pmv_axis_label_text_vjust <- function(axis_label_vjust, size = NULL) {
    if (comfort_is_waiver(axis_label_vjust)) {
        size <- if (is.null(size)) 2.8 else as.numeric(size)[[1L]]
        offset <- max(3.5, size * ggplot2::.pt * 0.42)
        return(grid::unit(offset, "pt"))
    }
    if (grid::is.unit(axis_label_vjust)) {
        return(axis_label_vjust)
    }
    if (is.numeric(axis_label_vjust) && length(axis_label_vjust)) {
        return(axis_label_vjust[[1L]])
    }
    0.5
}

comfort_pmv_axis_label_offset <- function(axis_label_hjust) {
    if (comfort_is_waiver(axis_label_hjust)) {
        return(0.025)
    }
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(max(0, min(0.08, axis_label_hjust[[1L]])))
    }
    0.025
}

comfort_pmv_axis_label_end <- function(axis_label_hjust) {
    if (is.numeric(axis_label_hjust) && length(axis_label_hjust)) {
        return(min(0.16, max(0, axis_label_hjust[[1L]]) + 0.055))
    }
    0.07
}

comfort_is_waiver <- function(x) {
    inherits(x, "waiver")
}

comfort_pmv_reverse_groups <- function(data) {
    pieces <- lapply(split(data, data$group), function(x) {
        x[rev(seq_len(nrow(x))), , drop = FALSE]
    })
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
    out
}

comfort_pmv_rootband_data <- function(model, metric, levels, n, units, pres,
                                      mollier, tdb_lim, hum_lim) {
    metric <- comfort_model_metric(model, metric)
    if (comfort_model_type(model) != "pmv" || metric != "pmv") {
        stop("Root-traced comfort overlay bands are only available for PMV.",
            call. = FALSE)
    }

    model <- comfort_pmv_curve_model(model)
    breaks <- comfort_pmv_rootband_breaks(levels)
    n <- comfort_grid_n(n)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    humratio <- comfort_pmv_rootband_humratio(
        model, breaks, n[[1L]], units, pres, lim$tdb, lim$hum
    )
    domain <- comfort_pmv_rootband_domain(humratio, lim$tdb, units, pres)
    valid <- domain$valid
    if (!any(valid)) {
        return(comfort_empty_band())
    }

    humratio <- humratio[valid]
    xlo <- domain$xlo[valid]
    xhi <- domain$xhi[valid]
    pmv_lo <- comfort_pmv_value_at(model, xlo, humratio, units, pres)
    pmv_hi <- comfort_pmv_value_at(model, xhi, humratio, units, pres)
    valid <- is.finite(pmv_lo) & is.finite(pmv_hi) & pmv_lo <= pmv_hi
    if (!any(valid)) {
        return(comfort_empty_band())
    }

    humratio <- humratio[valid]
    xlo <- xlo[valid]
    xhi <- xhi[valid]
    pmv_lo <- pmv_lo[valid]
    pmv_hi <- pmv_hi[valid]

    roots <- lapply(breaks, function(level) {
        roots <- comfort_pmv_curve_root_vector(
            model, level, humratio, lim$tdb, units, pres
        )
        sat_roots <- comfort_pmv_curve_saturation_roots(
            model, level, lim$tdb, lim$hum, units, pres, n[[1L]]
        )
        key <- match(round(sat_roots$humratio, 12L), round(humratio, 12L))
        ok <- !is.na(key)
        roots[key[ok]] <- sat_roots$tdb[ok]
        roots
    })
    names(roots) <- as.character(breaks)

    lows <- c(-Inf, breaks)
    highs <- c(breaks, Inf)
    values <- c(breaks[[1L]], (breaks[-length(breaks)] + breaks[-1L]) / 2,
        breaks[[length(breaks)]])
    overlap <- diff(range(lim$tdb)) / 10000

    polys <- list()
    for (i in seq_along(values)) {
        low <- lows[[i]]
        high <- highs[[i]]
        band <- comfort_pmv_rootband_edges(
            low, high, breaks, roots, xlo, xhi, pmv_lo, pmv_hi, overlap
        )
        if (!any(band$keep)) {
            next
        }

        runs <- split(which(band$keep), cumsum(c(TRUE, diff(which(band$keep)) != 1L)))
        for (run in runs) {
            if (length(run) < 2L) {
                next
            }
            left <- band$left[run]
            right <- band$right[run]
            y <- humratio[run]
            width_tol <- comfort_pmv_rootband_width_tol(left, right)
            ok <- is.finite(left) & is.finite(right) & is.finite(y) &
                left <= right + width_tol
            if (sum(ok) < 2L) {
                next
            }
            left <- left[ok]
            right <- right[ok]
            right <- pmax(right, left)
            y <- y[ok]
            group <- length(polys) + 1L
            polys[[group]] <- new_data_frame(list(
                tdb = c(left, rev(right)),
                humratio = c(y, rev(y)),
                edge = c(rep("left", length(left)), rep("right", length(right))),
                edge_level = c(band$left_level[run][ok], rev(band$right_level[run][ok])),
                level = sprintf("%s:%s",
                    comfort_format_band_level(low),
                    comfort_format_band_level(high)
                ),
                level_low = low,
                level_high = high,
                level_mid = values[[i]],
                value = values[[i]],
                group = group,
                subgroup = 1L,
                metric = "pmv"
            ))
        }
    }

    if (!length(polys)) {
        return(comfort_empty_band())
    }

    out <- do.call(rbind, polys)
    row.names(out) <- NULL
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_empty_pmv_curve <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        level = numeric(), value = numeric(), group = integer(),
        linetype = character(), label = character(), hjust = numeric(),
        vjust = numeric(), metric = character()
    ))
}

comfort_pmv_band_data <- function(model, range, n, units, pres, mollier,
                                  tdb_lim, hum_lim) {
    range <- comfort_check_breaks(range, "`range`", n_min = 2L)
    if (length(range) != 2L) {
        stop("`range` must contain exactly two PMV boundaries.", call. = FALSE)
    }

    bands <- comfort_pmv_rootband_data(
        model, "pmv", range, n, units, pres, mollier, tdb_lim, hum_lim
    )
    if (!nrow(bands)) {
        return(comfort_empty_band())
    }

    keep <- is.finite(bands$level_low) & is.finite(bands$level_high) &
        abs(bands$level_low - range[[1L]]) <= 1e-8 &
        abs(bands$level_high - range[[2L]]) <= 1e-8
    out <- bands[keep, , drop = FALSE]
    if (!nrow(out)) {
        return(comfort_empty_band())
    }
    out$value <- mean(range)
    out$level_mid <- mean(range)
    out
}

comfort_pmv_rootband_breaks <- function(levels) {
    if (is.null(levels)) {
        return(seq(-3, 3, by = 0.25))
    }
    if (length(levels) == 1L) {
        n <- as.integer(levels[[1L]])
        if (!is.finite(n) || n < 1L) {
            stop("`levels` must be a positive band count or numeric breaks.",
                call. = FALSE)
        }
        return(seq(-3, 3, length.out = n + 1L))
    }
    comfort_check_breaks(levels, "`levels`", n_min = 2L)
}

comfort_format_band_level <- function(level) {
    if (!is.finite(level)) {
        return(if (level < 0) "-Inf" else "Inf")
    }
    comfort_format_pmv_level(level)
}

comfort_pmv_rootband_humratio <- function(model, breaks, n, units, pres,
                                          tdb_lim, hum_lim) {
    hum <- seq(
        narrow_hum(hum_lim[[1L]], units),
        narrow_hum(hum_lim[[2L]], units),
        length.out = n
    )
    sat <- psychro_saturation_humratio(tdb_lim, units, pres)
    hum <- c(hum, sat[is.finite(sat)])
    for (level in breaks) {
        roots <- comfort_pmv_curve_saturation_roots(
            model, level, tdb_lim, hum_lim, units, pres, n
        )
        hum <- c(hum, roots$humratio)
    }
    hum <- sort(unique(round(hum[is.finite(hum)], 12L)))
    hum_lim <- narrow_hum(hum_lim, units)
    hum[hum >= hum_lim[[1L]] & hum <= hum_lim[[2L]]]
}

comfort_pmv_rootband_domain <- function(humratio, tdb_lim, units, pres) {
    xlo <- rep(tdb_lim[[1L]], length(humratio))
    positive <- humratio > 0
    if (any(positive)) {
        dew <- with_units(units, GetTDewPointFromHumRatioOnly(
            humratio[positive], rep(as.numeric(pres), length.out = sum(positive))
        ))
        xlo[positive] <- pmax(xlo[positive], dew)
    }
    xhi <- rep(tdb_lim[[2L]], length(humratio))
    valid <- is.finite(xlo) & is.finite(xhi) & humratio >= 0 & xlo < xhi
    list(xlo = xlo, xhi = xhi, valid = valid)
}

comfort_pmv_curve_root_vector <- function(model, level, humratio, tdb_lim,
                                          units, pres) {
    out <- rep(NA_real_, length(humratio))
    roots <- comfort_pmv_curve_roots(model, level, humratio, tdb_lim, units, pres)
    if (!length(roots$tdb)) {
        return(out)
    }
    key <- match(round(roots$humratio, 12L), round(humratio, 12L))
    ok <- !is.na(key)
    out[key[ok]] <- roots$tdb[ok]
    out
}

comfort_pmv_rootband_edges <- function(low, high, breaks, roots,
                                       xlo, xhi, pmv_lo, pmv_hi,
                                       overlap = 0) {
    value_tol <- 1e-5
    keep <- is.finite(pmv_lo) & is.finite(pmv_hi) &
        pmv_hi >= low - value_tol & pmv_lo <= high + value_tol
    left <- rep(NA_real_, length(xlo))
    right <- rep(NA_real_, length(xlo))
    left_level <- rep(NA_real_, length(xlo))
    right_level <- rep(NA_real_, length(xlo))
    if (!any(keep)) {
        return(list(
            left = left, right = right, keep = keep,
            left_level = left_level, right_level = right_level
        ))
    }

    if (is.finite(low)) {
        low_root <- roots[[as.character(low)]]
        use_root <- pmv_lo < low - value_tol &
            pmv_hi >= low - value_tol & is.finite(low_root)
        on_domain <- is.finite(pmv_lo) & abs(pmv_lo - low) <= value_tol
        left <- ifelse(use_root, low_root, xlo)
        left_level[use_root | on_domain] <- low
    } else {
        left <- xlo
    }

    if (is.finite(high)) {
        high_root <- roots[[as.character(high)]]
        use_root <- pmv_lo <= high + value_tol &
            pmv_hi > high + value_tol & is.finite(high_root)
        on_domain <- is.finite(pmv_lo) & abs(pmv_lo - high) <= value_tol
        right <- ifelse(use_root, high_root, xhi)
        right[on_domain] <- xlo[on_domain]
        right_level[use_root | on_domain] <- high
    } else {
        right <- xhi
    }

    inner_left <- is.finite(low) & is.finite(left) & abs(left - xlo) > overlap
    inner_right <- is.finite(high) & is.finite(right) & abs(right - xhi) > overlap
    left[inner_left] <- pmax(xlo[inner_left], left[inner_left] - overlap)
    right[inner_right] <- pmin(xhi[inner_right], right[inner_right] + overlap)
    width_tol <- comfort_pmv_rootband_width_tol(left, right)
    keep <- keep & is.finite(left) & is.finite(right) &
        left <= right + width_tol
    right[keep] <- pmax(right[keep], left[keep])
    list(
        left = left, right = right, keep = keep,
        left_level = left_level, right_level = right_level
    )
}

comfort_pmv_rootband_width_tol <- function(left, right) {
    x <- c(left, right)
    x <- x[is.finite(x)]
    if (!length(x)) {
        return(sqrt(.Machine$double.eps))
    }
    sqrt(.Machine$double.eps) * max(1, max(abs(x)))
}

comfort_pmv_curve_model <- function(model) {
    comfort_check_model(model)
    if (model$type != "pmv") {
        stop("Root-traced PMV curves require `comfort_model_pmv()`.",
            call. = FALSE)
    }
    model$params$round_output <- FALSE
    model
}

comfort_pmv_native_params <- function(model, units, pres) {
    p <- model$params
    if (isTRUE(p$limit_inputs)) {
        return(NULL)
    }
    numeric_params <- c("vr", "met", "clo", "wme")
    for (param in numeric_params) {
        value <- p[[param]]
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
            return(NULL)
        }
    }
    tr <- NA_real_
    if (!is.null(p$tr)) {
        if (!is.numeric(p$tr) || length(p$tr) != 1L || !is.finite(p$tr)) {
            return(NULL)
        }
        tr <- comfort_to_si_temp(as.numeric(p$tr), units)
    }
    list(
        tr = tr,
        vr = comfort_to_si_speed(as.numeric(p$vr), units),
        met = as.numeric(p$met),
        clo = as.numeric(p$clo),
        wme = as.numeric(p$wme),
        pressure = comfort_pressure_pa(as.numeric(pres), units),
        min_hum_ratio = psychrolib_options()$MIN_HUM_RATIO
    )
}

comfort_pmv_native_curve_roots <- function(model, level, humratio, tdb_lim,
                                           units, pres) {
    p <- comfort_pmv_native_params(model, units, pres)
    if (is.null(p)) {
        return(NULL)
    }
    roots <- .Call(
        C_comfort_pmv_curve_roots,
        as.numeric(level), as.numeric(humratio),
        as.numeric(comfort_to_si_temp(tdb_lim, units)),
        as.numeric(p$pressure), as.numeric(p$tr), as.numeric(p$vr),
        as.numeric(p$met), as.numeric(p$clo), as.numeric(p$wme),
        as.numeric(p$min_hum_ratio)
    )
    roots$tdb <- comfort_from_si_temp(roots$tdb, units)
    roots
}

comfort_pmv_native_saturation_roots <- function(model, level, tdb_lim, hum_lim,
                                                units, pres, n) {
    p <- comfort_pmv_native_params(model, units, pres)
    if (is.null(p)) {
        return(NULL)
    }
    roots <- .Call(
        C_comfort_pmv_saturation_roots,
        as.numeric(level),
        as.numeric(comfort_to_si_temp(tdb_lim, units)),
        as.numeric(narrow_hum(hum_lim, units)),
        as.integer(max(as.integer(n), 80L)),
        as.numeric(p$pressure), as.numeric(p$tr), as.numeric(p$vr),
        as.numeric(p$met), as.numeric(p$clo), as.numeric(p$wme),
        as.numeric(p$min_hum_ratio)
    )
    roots$tdb <- comfort_from_si_temp(roots$tdb, units)
    roots
}

comfort_pmv_curve_roots <- function(model, level, humratio, tdb_lim,
                                    units, pres) {
    roots <- comfort_pmv_native_curve_roots(
        model, level, humratio, tdb_lim, units, pres
    )
    if (!is.null(roots)) {
        return(roots)
    }
    comfort_pmv_curve_roots_r(model, level, humratio, tdb_lim, units, pres)
}

comfort_pmv_curve_roots_r <- function(model, level, humratio, tdb_lim,
                                      units, pres) {
    xlo <- rep(tdb_lim[[1L]], length(humratio))
    positive <- humratio > 0
    if (any(positive)) {
        dew <- with_units(units, GetTDewPointFromHumRatioOnly(
            humratio[positive], rep(as.numeric(pres), length.out = sum(positive))
        ))
        xlo[positive] <- pmax(xlo[positive], dew)
    }
    xhi <- rep(tdb_lim[[2L]], length(humratio))
    saturation_hi <- psychro_saturation_humratio(xhi, units, pres)
    valid <- is.finite(xlo) & is.finite(xhi) & xlo < xhi &
        humratio >= 0 & humratio <= saturation_hi
    if (!any(valid)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    lo <- xlo[valid]
    hi <- xhi[valid]
    hum <- humratio[valid]
    flo <- comfort_pmv_value_at(model, lo, hum, units, pres) - level
    fhi <- comfort_pmv_value_at(model, hi, hum, units, pres) - level
    bracket <- is.finite(flo) & is.finite(fhi) & flo * fhi <= 0
    if (!any(bracket)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    lo <- lo[bracket]
    hi <- hi[bracket]
    hum <- hum[bracket]
    flo <- flo[bracket]
    fhi <- fhi[bracket]

    exact_lo <- abs(flo) < 1e-8
    exact_hi <- abs(fhi) < 1e-8
    solved <- exact_lo | exact_hi
    root <- numeric(length(hum))
    root[exact_lo] <- lo[exact_lo]
    root[!exact_lo & exact_hi] <- hi[!exact_lo & exact_hi]

    active <- !solved
    for (i in seq_len(44L)) {
        if (!any(active)) {
            break
        }
        mid <- (lo[active] + hi[active]) / 2
        fmid <- comfort_pmv_value_at(model, mid, hum[active], units, pres) - level
        same <- sign(fmid) == sign(flo[active])
        same[!is.finite(same)] <- FALSE
        idx <- which(active)
        lo[idx[same]] <- mid[same]
        flo[idx[same]] <- fmid[same]
        hi[idx[!same]] <- mid[!same]
        fhi[idx[!same]] <- fmid[!same]
    }
    root[active] <- (lo[active] + hi[active]) / 2

    finite <- is.finite(root)
    list(tdb = root[finite], humratio = hum[finite])
}

comfort_pmv_curve_saturation_roots <- function(model, level, tdb_lim, hum_lim,
                                               units, pres, n) {
    roots <- comfort_pmv_native_saturation_roots(
        model, level, tdb_lim, hum_lim, units, pres, n
    )
    if (!is.null(roots)) {
        return(roots)
    }
    comfort_pmv_curve_saturation_roots_r(
        model, level, tdb_lim, hum_lim, units, pres, n
    )
}

comfort_pmv_curve_saturation_roots_r <- function(model, level, tdb_lim, hum_lim,
                                                 units, pres, n) {
    n <- max(as.integer(n), 80L)
    tdb <- seq(tdb_lim[[1L]], tdb_lim[[2L]], length.out = n)
    hum <- psychro_saturation_humratio(tdb, units, pres)
    hum_lim <- narrow_hum(hum_lim, units)
    valid <- is.finite(tdb) & is.finite(hum) &
        hum >= hum_lim[[1L]] & hum <= hum_lim[[2L]]
    if (!any(valid)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    value <- comfort_pmv_value_at(model, tdb, hum, units, pres) - level
    valid <- valid & is.finite(value)
    if (!any(valid)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    roots <- tdb[valid & abs(value) < 1e-8]
    segment <- which(
        valid[-length(valid)] & valid[-1L] &
            value[-length(value)] * value[-1L] < 0
    )
    if (length(segment)) {
        roots <- c(roots, vapply(segment, function(i) {
            lo <- tdb[[i]]
            hi <- tdb[[i + 1L]]
            flo <- value[[i]]
            for (j in seq_len(44L)) {
                mid <- (lo + hi) / 2
                fmid <- comfort_pmv_value_at(
                    model, mid, psychro_saturation_humratio(mid, units, pres),
                    units, pres
                ) - level
                if (!is.finite(fmid)) {
                    break
                }
                if (sign(fmid) == sign(flo)) {
                    lo <- mid
                    flo <- fmid
                } else {
                    hi <- mid
                }
            }
            (lo + hi) / 2
        }, numeric(1L)))
    }

    roots <- roots[is.finite(roots)]
    if (!length(roots)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    hum <- psychro_saturation_humratio(roots, units, pres)
    keep <- is.finite(hum) & hum >= hum_lim[[1L]] & hum <= hum_lim[[2L]]
    list(tdb = roots[keep], humratio = hum[keep])
}

comfort_pmv_curve_merge_roots <- function(...) {
    roots <- list(...)
    tdb <- unlist(lapply(roots, `[[`, "tdb"), use.names = FALSE)
    humratio <- unlist(lapply(roots, `[[`, "humratio"), use.names = FALSE)
    keep <- is.finite(tdb) & is.finite(humratio)
    if (!any(keep)) {
        return(list(tdb = numeric(), humratio = numeric()))
    }

    tdb <- tdb[keep]
    humratio <- humratio[keep]
    ord <- order(humratio, tdb)
    tdb <- tdb[ord]
    humratio <- humratio[ord]
    key <- paste(round(tdb, 8L), round(humratio, 12L), sep = ":")
    keep <- !duplicated(key)
    list(tdb = tdb[keep], humratio = humratio[keep])
}

comfort_pmv_value_at <- function(model, tdb, humratio, units, pres) {
    rh <- comfort_relhum_from_humratio(tdb, humratio, units, pres)
    rh <- comfort_clip_grid_rh(rh)
    out <- rep(NA_real_, length(tdb))
    valid <- comfort_valid_grid_rh(rh)
    if (any(valid)) {
        out[valid] <- comfort_metric_value(
            comfort_apply_model(model, tdb[valid], rh[valid], units, pres),
            "pmv"
        )
    }
    out
}

comfort_pmv_linetype <- function(level) {
    if (abs(level) < 1e-8) "dashed" else "solid"
}

comfort_pmv_curve_label <- function(level, label) {
    switch(label,
        none = NA_character_,
        sensation = comfort_pmv_sensation_label(level),
        boundary = paste("PMV", comfort_format_pmv_level(level)),
        comfort = "COMFORT"
    )
}

comfort_pmv_curve_hjust <- function(label, override = NULL) {
    if (!is.null(override)) {
        return(override)
    }
    switch(label,
        none = 0.5,
        sensation = 0.52,
        boundary = 0.045,
        comfort = 0.52
    )
}

comfort_pmv_curve_vjust <- function(level, label, override = NULL,
                                    mollier = FALSE) {
    if (!is.null(override)) {
        return(override)
    }
    if (label == "boundary") {
        if (isTRUE(mollier)) {
            return(if (level <= 0) 1.25 else -0.25)
        }
        return(if (level <= 0) -0.25 else 1.25)
    }
    if (label == "comfort") {
        return(0.5)
    }
    if (abs(level) < 1e-8) {
        return(0.5)
    }
    0.5
}

comfort_pmv_sensation_label <- function(level) {
    if (abs(level - round(level)) > 1e-8) {
        return(NA_character_)
    }
    labels <- c(
        "-3" = "COLD", "-2" = "COOL", "-1" = "SLIGHTLY COOL",
        "0" = "NEUTRAL", "1" = "SLIGHTLY WARM", "2" = "WARM",
        "3" = "HOT"
    )
    labels[[as.character(as.integer(round(level)))]] %||% NA_character_
}

comfort_format_pmv_level <- function(level) {
    ifelse(level > 0, sprintf("+%.1f", level), sprintf("%.1f", level))
}

comfort_band_data <- function(model, metric, levels, n, units, pres, mollier,
                              tdb_lim, hum_lim) {
    m <- comfort_grid_matrix(
        model, metric, n, units, pres, tdb_lim, hum_lim,
        at = "nodes", boundary = "saturation"
    )
    breaks <- comfort_band_breaks(m$metric, m$value, levels)
    if (length(breaks) < 2L) {
        return(comfort_empty_band())
    }

    bands <- isoband::isobands(
        x = m$tdb, y = m$humratio, z = t(m$value),
        levels_low = breaks[-length(breaks)],
        levels_high = breaks[-1L]
    )
    comfort_isoband_data(
        bands, breaks[-length(breaks)], breaks[-1L],
        m$metric, mollier, geom = "polygon"
    )
}

comfort_empty_band <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        level = character(), level_low = numeric(), level_high = numeric(),
        level_mid = numeric(), value = numeric(), group = character(),
        subgroup = integer(), metric = character()
    ))
}

comfort_empty_tile <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        width = numeric(), height = numeric(),
        value = numeric(), metric = character(), group = integer()
    ))
}

comfort_grid_boundary_values <- function(model, metric, units, pres,
                                         x0, x1, y0, y1, s0, s1) {
    tdb <- (x0 + x1) / 2
    sat_mid <- (s0 + s1) / 2
    humratio <- pmin((y0 + y1) / 2, sat_mid - sqrt(.Machine$double.eps))

    outside_mid <- humratio <= y0
    if (any(outside_mid)) {
        eps <- sqrt(.Machine$double.eps)
        use_left <- s0[outside_mid] >= s1[outside_mid]
        tdb[outside_mid] <- ifelse(
            use_left,
            x0[outside_mid] + (x1[outside_mid] - x0[outside_mid]) * eps,
            x1[outside_mid] - (x1[outside_mid] - x0[outside_mid]) * eps
        )
        humratio[outside_mid] <- y0[outside_mid] + (y1[outside_mid] - y0[outside_mid]) * eps
    }

    rh <- comfort_relhum_from_humratio(tdb, humratio, units, pres)
    rh <- comfort_clip_grid_rh(rh)
    out <- rep(NA_real_, length(tdb))
    valid <- comfort_valid_grid_rh(rh)
    if (any(valid)) {
        out[valid] <- comfort_metric_value(
            comfort_apply_model(model, tdb[valid], rh[valid], units, pres),
            metric
        )
    }
    out
}

comfort_contour_data <- function(model, metric, breaks, n, units, pres,
                                 mollier, tdb_lim, hum_lim,
                                 contour_method = c("auto", "root", "isoband")) {
    contour_method <- match.arg(contour_method)
    metric <- comfort_model_metric(model, metric)
    if (contour_method == "auto") {
        contour_method <- if (comfort_model_type(model) == "pmv" && metric == "pmv") {
            "root"
        } else {
            "isoband"
        }
    }
    if (contour_method == "root") {
        if (comfort_model_type(model) != "pmv" || metric != "pmv") {
            stop("Root-traced contours are only available for PMV.",
                call. = FALSE)
        }
        if (is.null(breaks)) {
            breaks <- comfort_contour_breaks("pmv", numeric())
        }
        return(comfort_pmv_curve_data(
            model, breaks, n[[1L]], units, pres, mollier, tdb_lim, hum_lim,
            label = "none"
        ))
    }

    m <- comfort_grid_matrix(
        model, metric, n, units, pres, tdb_lim, hum_lim,
        at = "nodes", boundary = "saturation"
    )
    z <- m$value
    if (is.null(breaks)) {
        breaks <- comfort_contour_breaks(m$metric, z)
    }
    breaks <- breaks[is.finite(breaks)]
    if (!length(breaks)) {
        return(comfort_empty_contour())
    }

    lines <- isoband::isolines(
        x = m$tdb, y = m$humratio, z = t(z), levels = breaks
    )
    comfort_isoband_data(lines, breaks, breaks, m$metric, mollier, geom = "path")
}

comfort_empty_contour <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        level = numeric(), group = character(), metric = character()
    ))
}

comfort_contour_breaks <- function(metric, z) {
    if (metric == "pmv") {
        return(seq(-3, 3, by = 0.5))
    }
    pretty(range(z, finite = TRUE), n = 8)
}

comfort_band_breaks <- function(metric, z, levels = NULL) {
    if (identical(metric, "acceptability") && is.null(levels)) {
        return(c(-0.5, 0.5, 1.5))
    }

    if (!is.null(levels) && length(levels) > 1L) {
        breaks <- sort(unique(as.numeric(levels)))
        return(breaks[is.finite(breaks)])
    }

    n <- if (is.null(levels)) 64L else as.integer(levels[[1L]])
    if (!is.finite(n) || n < 1L) {
        stop("`levels` must be a positive count or a numeric break vector.",
            call. = FALSE)
    }

    z_range <- range(z, finite = TRUE)
    if (!all(is.finite(z_range))) {
        return(numeric())
    }
    if (z_range[[1L]] == z_range[[2L]]) {
        z_range <- z_range + c(-0.5, 0.5)
    } else {
        pad <- diff(z_range) * 1e-9
        z_range <- z_range + c(-pad, pad)
    }
    seq(z_range[[1L]], z_range[[2L]], length.out = n + 1L)
}

comfort_isoband_data <- function(iso, low, high, metric, mollier,
                                 geom = c("polygon", "path")) {
    geom <- match.arg(geom)
    lengths <- vapply(iso, function(x) length(x$x), integer(1L))
    if (!any(lengths)) {
        return(if (geom == "polygon") comfort_empty_band() else comfort_empty_contour())
    }

    out <- vector("list", length(iso))
    for (i in seq_along(iso)) {
        item <- iso[[i]]
        n <- length(item$x)
        if (!n) {
            next
        }

        if (geom == "polygon") {
            level_low <- low[[i]]
            level_high <- high[[i]]
            level_mid <- (level_low + level_high) / 2
            out[[i]] <- new_data_frame(list(
                tdb = item$x,
                humratio = item$y,
                level = sprintf("%s:%s", level_low, level_high),
                level_low = level_low,
                level_high = level_high,
                level_mid = level_mid,
                value = level_mid,
                group = i,
                subgroup = item$id,
                metric = metric
            ))
        } else {
            level <- low[[i]]
            out[[i]] <- new_data_frame(list(
                tdb = item$x,
                humratio = item$y,
                level = level,
                group = i * 100000L + item$id,
                metric = metric
            ))
        }
    }
    out <- do.call(rbind, out[!vapply(out, is.null, logical(1L))])
    row.names(out) <- NULL
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_zone_data <- function(model, metric, range, n, gap, units, pres,
                              mollier, tdb_lim, hum_lim, na.rm = FALSE) {
    if (comfort_model_type(model) == "adaptive") {
        return(comfort_zone_adaptive(model, units, mollier, tdb_lim, hum_lim))
    }

    metric <- comfort_model_metric(model, metric)
    range <- comfort_zone_range(model, metric, range)
    if (comfort_model_type(model) == "pmv" && metric == "pmv") {
        return(comfort_pmv_band_data(
            model, range, n[[1L]], units, pres, mollier, tdb_lim, hum_lim
        ))
    }
    comfort_band_data(model, metric, range, n, units, pres, mollier,
        tdb_lim, hum_lim)
}

comfort_zone_range <- function(model, metric, range) {
    if (!is.null(range)) {
        if (!is.numeric(range) || length(range) != 2L ||
                any(!is.finite(range)) || range[[1L]] >= range[[2L]]) {
            stop("`range` must be a finite increasing pair.", call. = FALSE)
        }
        return(range)
    }
    switch(metric,
        pmv = c(-0.5, 0.5),
        set = c(22.2, 25.6),
        stop("A comfort `range` is required for this metric.", call. = FALSE)
    )
}

comfort_zone_adaptive <- function(model, units, mollier, tdb_lim, hum_lim) {
    p <- model$params
    if (!is.null(p$tr)) {
        stop(
            "Adaptive comfort zones require `tr = NULL` so chart dry-bulb ",
            "temperature can represent operative temperature.",
            call. = FALSE
        )
    }

    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    mid <- mean(lim$tdb)
    zone <- comfort_apply_model(model, mid, rh = 50, units = units, pres = 101325)
    lower <- max(zone$lower[[1L]], lim$tdb[[1L]])
    upper <- min(zone$upper[[1L]], lim$tdb[[2L]])
    if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
        return(new_data_frame(list(
            tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
            group = character(), subgroup = integer(), value = numeric()
        )))
    }

    hum <- narrow_hum(lim$hum, units)
    out <- new_data_frame(list(
        tdb = c(lower, upper, upper, lower),
        humratio = c(hum[[1L]], hum[[1L]], hum[[2L]], hum[[2L]]),
        group = 1L,
        subgroup = 1L,
        value = 1,
        width = NA_real_,
        height = NA_real_
    ))
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_model_metric <- function(model, metric = NULL) {
    comfort_check_model(model)
    if (!is.null(metric)) {
        return(as.character(metric)[[1L]])
    }
    switch(model$type,
        pmv = "pmv",
        set = "set",
        adaptive = "acceptability"
    )
}

comfort_metric_value <- function(result, metric) {
    if (!metric %in% names(result)) {
        stop("Metric `", metric, "` is not produced by this comfort model.",
            call. = FALSE)
    }
    value <- result[[metric]]
    if (is.logical(value)) {
        return(as.numeric(value))
    }
    as.numeric(value)
}

comfort_apply_model <- function(model, tdb, rh, units, pres) {
    comfort_check_model(model)
    p <- model$params
    tr <- if (is.null(p$tr)) tdb else p$tr

    switch(model$type,
        pmv = comfort_pmv(
            tdb = tdb, tr = tr, vr = p$vr, rh = rh, met = p$met,
            clo = p$clo, wme = p$wme, units = units,
            limit_inputs = p$limit_inputs, round_output = p$round_output
        ),
        set = comfort_set(
            tdb = tdb, tr = tr, v = p$v, rh = rh, met = p$met,
            clo = p$clo, wme = p$wme, units = units,
            limit_inputs = p$limit_inputs,
            round_output = p$round_output,
            body_surface_area = p$body_surface_area,
            p_atm = if (is.null(p$p_atm)) comfort_pressure_pa(pres, units) else p$p_atm,
            position = p$position
        ),
        adaptive = comfort_adaptive(
            tdb = tdb, tr = tr, t_running = p$t_running, v = p$v,
            standard = p$standard, category = p$category, units = units,
            limit_inputs = p$limit_inputs, round_output = p$round_output
        )
    )
}

comfort_relhum_from_humratio <- function(tdb, humratio, units, pres) {
    rh <- with_units(
        units,
        psychrolib::GetRelHumFromHumRatio(tdb, humratio, pres)
    )
    rh * 100
}

comfort_clip_grid_rh <- function(rh) {
    tol <- 1e-3
    rh[is.finite(rh) & rh < 0 & rh >= -tol] <- 0
    rh[is.finite(rh) & rh > 100 & rh <= 100 + tol] <- 100
    rh
}

comfort_valid_grid_rh <- function(rh) {
    is.finite(rh) & rh >= 0 & rh <= 100
}
