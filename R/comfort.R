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
#' @param solar_exposure Relative solar exposure for heat index, from 0 to 1.
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
#' comfort_heat_index(32, rh = 70)
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

#' @rdname comfort_pmv
#' @export
comfort_heat_index <- function(tdb, rh, solar_exposure = 0,
                               units = c("SI", "IP"),
                               limit_inputs = TRUE,
                               round_output = TRUE) {
    units <- match.arg(units)
    x <- comfort_recycle(
        tdb = tdb, rh = rh, solar_exposure = solar_exposure
    )

    tdb_si <- comfort_to_si_temp(x$tdb, units)
    tdb_f <- get_f_from_c(tdb_si)
    exposure <- pmin(pmax(x$solar_exposure, 0), 1)
    heat_index_f <- comfort_heat_index_f(tdb_f, x$rh, exposure)

    if (isTRUE(limit_inputs)) {
        valid <- comfort_between(tdb_si, -50, 100) &
            comfort_between(x$rh, 0, 100) &
            is.finite(exposure)
        heat_index_f[!valid] <- NA_real_
    }

    category <- comfort_heat_index_category(heat_index_f)
    heat_index <- if (units == "IP") heat_index_f else get_c_from_f(heat_index_f)
    if (isTRUE(round_output)) {
        heat_index <- round(heat_index, 1L)
    }

    new_data_frame(list(
        heat_index = heat_index,
        category = category$category,
        category_id = category$category_id
    ))
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
#'
#' @examples
#' # Create a PMV model object.
#' comfort_model_pmv(met = 1.4, clo = 0.5)
#'
#' # Create a SET model object.
#' comfort_model_set(v = 0.2)
#'
#' # Create an adaptive comfort model object.
#' comfort_model_adaptive(t_running = 22)
#'
#' # Create a heat-index model object.
#' comfort_model_heat_index(solar_exposure = 0.5)
#'
#' # Draw a PMV overlay using custom activity and clothing assumptions.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(
#'         model = comfort_model_pmv(met = 1.4, clo = 0.5),
#'         n = c(45, 30)
#'     )
#'
#' # Draw SET as the filled comfort metric.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(
#'         model = comfort_model_set(v = 0.2),
#'         metric = "set",
#'         n = c(45, 30)
#'     )
#'
#' # Draw the adaptive acceptability region.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_zone(
#'         model = comfort_model_adaptive(t_running = 22),
#'         metric = "acceptability",
#'         n = c(45, 30),
#'         alpha = 0.3
#'     )
#'
#' # Draw heat-index categories with a solar exposure adjustment.
#' ggpsychro(tdb_lim = c(25, 45), hum_lim = c(0, 32)) +
#'     geom_comfort_heat_index(
#'         model = comfort_model_heat_index(solar_exposure = 0.5),
#'         n = c(55, 35)
#'     )
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

#' @rdname comfort_model_pmv
#' @export
comfort_model_heat_index <- function(solar_exposure = 0,
                                     limit_inputs = TRUE,
                                     round_output = FALSE) {
    solar_exposure <- as.numeric(solar_exposure)
    if (length(solar_exposure) != 1L || !is.finite(solar_exposure) ||
            solar_exposure < 0 || solar_exposure > 1) {
        stop("`solar_exposure` must be a single finite value from 0 to 1.",
            call. = FALSE)
    }
    comfort_model(
        "heat_index",
        list(solar_exposure = solar_exposure, limit_inputs = limit_inputs,
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
#' @examples
#' # Create the ASHRAE 55 PMV comfort interval.
#' comfort_standard_ashrae55_2017()
#'
#' # Create the EN 15251 PMV comfort bands.
#' comfort_standard_en15251_2007()
#'
#' # Draw the ASHRAE 55 comfort zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_standard_zone(
#'         standard = comfort_standard_ashrae55_2017(),
#'         n = 80
#'     )
#'
#' # Draw the EN 15251 comfort bands.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_standard_zone(
#'         standard = comfort_standard_en15251_2007(),
#'         n = 80
#'     )
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

#' Givoni bioclimatic strategy
#'
#' `comfort_strategy_givoni()` stores the fixed inputs used by
#' `geom_comfort_givoni()`. The strategy geometry follows Marsh's Givoni
#' Bioclimatic Chart overlay: the mean outdoor temperature shifts the base
#' comfort zone, and zones are drawn in dry-bulb/relative-humidity space before
#' conversion to humidity ratio.
#'
#' @param mean_outdoor Mean outdoor temperature.
#' @param units Unit system for `mean_outdoor`, `"SI"` or `"IP"`.
#'
#' @return A Givoni comfort strategy object.
#'
#' @examples
#' # Create a Givoni strategy for a warm outdoor mean.
#' comfort_strategy_givoni(mean_outdoor = 22)
#'
#' # Draw the Givoni strategy overlay for that outdoor mean.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         strategy = comfort_strategy_givoni(mean_outdoor = 22),
#'         show_labels = FALSE
#'     )
#'
#' @export
comfort_strategy_givoni <- function(mean_outdoor = 19,
                                    units = c("SI", "IP")) {
    units <- match.arg(units)
    mean_outdoor <- as.numeric(mean_outdoor)
    if (length(mean_outdoor) != 1L || !is.finite(mean_outdoor)) {
        stop("`mean_outdoor` must be a single finite temperature.",
            call. = FALSE)
    }
    structure(
        list(mean_outdoor = mean_outdoor, units = units),
        class = c("PsyComfortGivoniStrategy", "list")
    )
}

#' Comfort zone style element
#'
#' `element_comfort_zone()` creates a small style object for comfort strategy
#' zones. It is used by `geom_comfort_givoni()` through the `zone_style`
#' argument to override Marsh-style defaults for individual zones.
#'
#' @param fill,colour,color,linewidth,linetype,alpha,linejoin Zone drawing
#'   properties. Values left as [ggplot2::waiver()] inherit the layer default.
#'
#' @return A comfort zone style element.
#'
#' @examples
#' # Fill and outline the comfort zone with custom colours.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         show_labels = FALSE,
#'         zone_style = list(
#'             comfort = element_comfort_zone(
#'                 fill = "#6FCF97",
#'                 colour = "#1B7F4A",
#'                 alpha = 0.35
#'             )
#'         )
#'     )
#'
#' # Emphasize the air-conditioning region with a light fill.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         show_labels = FALSE,
#'         zone_style = list(
#'             air_conditioning = element_comfort_zone(
#'                 fill = "#7BC8F6",
#'                 colour = "#1B5E8C",
#'                 alpha = 0.18,
#'                 linetype = "solid"
#'             )
#'         )
#'     )
#'
#' # Restyle a line-only region without filling it.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(
#'         show_labels = FALSE,
#'         zone_style = list(
#'             winter = element_comfort_zone(
#'                 colour = "#C44536",
#'                 linewidth = 1.2,
#'                 linetype = "dashed"
#'             )
#'         )
#'     )
#'
#' @export
element_comfort_zone <- function(fill = ggplot2::waiver(),
                                 colour = ggplot2::waiver(),
                                 linewidth = ggplot2::waiver(),
                                 linetype = ggplot2::waiver(),
                                 alpha = ggplot2::waiver(),
                                 linejoin = ggplot2::waiver(),
                                 color = NULL) {
    if (!is.null(color)) {
        colour <- color
    }
    structure(
        list(
            fill = fill,
            colour = colour,
            linewidth = linewidth,
            linetype = linetype,
            alpha = alpha,
            linejoin = linejoin
        ),
        class = c("PsyComfortZoneElement", "list")
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
#' @param label A single logical value. If `TRUE`, label contour lines with
#'   their level values.
#' @param label_size Text size for contour labels. Defaults to `2.8`.
#' @param range Comfort value interval for PMV and SET zones.
#' @param standard A PMV-based comfort standard object.
#' @param show_labels If `TRUE`, draw overlay labels.
#' @param strategy A Givoni bioclimatic strategy object.
#' @param show_pmv If `TRUE`, draw the PMV comfort background under the Givoni
#'   strategy outlines.
#' @param pmv_model PMV model used when `show_pmv = TRUE`.
#' @param zone_alpha Alpha for the filled Givoni comfort zone. Other Givoni
#'   strategy regions are drawn as outlines.
#' @param zone_style Optional named list of per-zone style overrides for
#'   `geom_comfort_givoni()`. Names must match Givoni zone ids such as
#'   `"comfort"`, `"winter"`, or `"air_conditioning"`. Values can be created
#'   with [element_comfort_zone()], [ggplot2::element_polygon()], or ordinary
#'   named lists with fields `fill`, `colour`/`color`, `linewidth`, `linetype`,
#'   `alpha`, and `linejoin`.
#'
#' @examples
#' # Draw filled PMV comfort bands.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(name = "PMV")
#'
#' # Draw labelled PMV contour lines.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_contour(
#'         breaks = c(-1, 0, 1),
#'         label = TRUE,
#'         n = 80
#'     )
#'
#' # Draw the neutral PMV comfort zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_zone(
#'         range = c(-0.5, 0.5),
#'         n = c(45, 30),
#'         alpha = 0.3
#'     )
#'
#' # Draw PMV curves and thermal sensation labels.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv_lines(levels = seq(-2, 2, by = 1), n = 100)
#'
#' # Draw a PMV-based comfort standard zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_standard_zone(
#'         standard = comfort_standard_ashrae55_2017(),
#'         n = 80
#'     )
#'
#' # Draw heat-index categories on hot conditions.
#' ggpsychro(tdb_lim = c(25, 45), hum_lim = c(0, 32)) +
#'     geom_comfort_heat_index(n = c(55, 35), show_labels = FALSE)
#'
#' # Draw Givoni bioclimatic strategy zones.
#' ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
#'     geom_comfort_givoni(show_labels = FALSE)
#'
#' # Evaluate PMV at supplied state points.
#' states <- data.frame(
#'     tdb = c(24, 28, 31),
#'     relhum = c(45, 55, 65)
#' )
#' ggpsychro(states, tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     stat_comfort_state(
#'         aes(tdb = tdb, relhum = relhum, colour = after_stat(pmv)),
#'         size = 3
#'     )
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
geom_comfort_heat_index <- function(mapping = NULL, data = NULL,
                                    position = "identity", ...,
                                    model = comfort_model_heat_index(),
                                    n = c(160, 100), alpha = 0.55,
                                    show_labels = TRUE,
                                    na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE) {
    label <- angle <- NULL
    layer_mapping <- comfort_computed_xy_mapping(mapping)
    params <- list(...)
    zone_specs <- comfort_heat_index_zone_specs()
    layers <- vector("list", length(zone_specs))
    for (i in seq_along(zone_specs)) {
        spec <- zone_specs[[i]]
        zone_params <- utils::modifyList(params, list(
            na.rm = na.rm, model = model, n = n, category_id = spec$id,
            alpha = alpha, fill = spec$fill, colour = NA
        ))
        layers[[i]] <- psychro_layer(
            stat = StatComfortHeatIndexZone, data = comfort_layer_data(data),
            mapping = layer_mapping, geom = "polygon", position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = zone_params
        )
    }

    line_params <- params
    if (is.null(line_params$colour) && is.null(line_params$color)) {
        line_params$colour <- "#4A4A4A"
    }
    if (is.null(line_params$linewidth)) {
        line_params$linewidth <- 0.45
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortHeatIndexContour, data = comfort_layer_data(data),
        mapping = layer_mapping, geom = "path", position = position,
        show.legend = FALSE, inherit.aes = inherit.aes,
        params = utils::modifyList(line_params, list(
            na.rm = na.rm, model = model, n = n
        ))
    )

    if (isTRUE(show_labels)) {
        text_params <- params
        if (is.null(text_params$colour) && is.null(text_params$color)) {
            text_params$colour <- "#444444"
        }
        if (is.null(text_params$fontface)) {
            text_params$fontface <- "bold"
        }
        if (is.null(text_params$size)) {
            text_params$size <- 3
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortHeatIndexLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle)
            )),
            geom = GeomComfortNullText, position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = list(
                na.rm = na.rm, model = model, n = n, alpha = 0
            )
        )
        layers[[length(layers) + 1L]] <- comfort_heat_index_foreground_labels(
            model, n, text_params
        )
    }

    layers
}

#' @rdname geom_comfort_overlay
#' @export
geom_comfort_contour <- function(mapping = NULL, data = NULL,
                                 stat = StatComfortContour,
                                 position = "identity", ...,
                                 model = comfort_model_pmv(),
                                 metric = NULL, breaks = NULL,
                                 n = NULL,
                                 contour_method = c("auto", "root", "isoband"),
                                 label = FALSE, label_size = NULL,
                                 na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
    contour_method <- match.arg(contour_method)
    assert_flag(label)
    if (!is.null(label_size)) {
        assert_number(label_size, lower = 0, .var.name = "label_size")
    }

    params <- list(
        na.rm = na.rm, model = model, metric = metric,
        breaks = breaks, n = n, contour_method = contour_method, ...
    )

    if (!isTRUE(label)) {
        params$label_path <- FALSE
        return(psychro_layer(
            stat = stat, data = comfort_layer_data(data), mapping = mapping, geom = "path",
            position = position, show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = params
        ))
    }

    label_params <- comfort_contour_label_params(params, label_size)
    label_params$label_path <- TRUE
    psychro_layer(
        stat = stat, data = comfort_layer_data(data),
        mapping = comfort_contour_label_mapping(mapping),
        geom = geomtextpath::GeomTextpath,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = label_params
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
geom_comfort_givoni <- function(strategy = comfort_strategy_givoni(),
                                mapping = NULL, data = NULL,
                                position = "identity", ...,
                                alpha = 0.55, show_labels = TRUE,
                                show_pmv = FALSE,
                                pmv_model = comfort_model_pmv(),
                                zone_alpha = 0.2, zone_style = NULL,
                                na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE) {
    label <- angle <- hjust <- vjust <- NULL
    strategy <- comfort_check_givoni_strategy(strategy)
    layer_mapping <- comfort_computed_xy_mapping(mapping)
    params <- list(...)
    zone_specs <- comfort_givoni_zone_specs()
    zone_specs <- zone_specs[zone_specs$draw_zone, , drop = FALSE]
    zone_style <- comfort_givoni_check_zone_style(zone_style, zone_specs$zone)
    layers <- list()
    if (isTRUE(show_pmv)) {
        layers[[length(layers) + 1L]] <- geom_comfort_overlay(
            data = data,
            model = pmv_model,
            alpha = alpha,
            method = "rootband",
            na.rm = na.rm,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
        layers[[length(layers) + 1L]] <- scale_fill_comfort_pmv(guide = "none")
    }
    for (i in seq_len(nrow(zone_specs))) {
        spec <- zone_specs[i, , drop = FALSE]
        zone_params <- comfort_givoni_zone_params(
            spec, params, zone_style, zone_alpha, na.rm, strategy
        )
        zone_is_filled <- comfort_zone_fill_is_set(zone_params$fill)
        zone_geom <- if (zone_is_filled) "polygon" else "path"
        if (!zone_is_filled) {
            zone_params$fill <- NULL
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniZone, data = comfort_layer_data(data),
            mapping = layer_mapping, geom = zone_geom, position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = zone_params
        )
    }

    mean_params <- params
    if (is.null(mean_params$colour) && is.null(mean_params$color)) {
        mean_params$colour <- "#444444"
    }
    if (is.null(mean_params$linewidth)) {
        mean_params$linewidth <- 0.8
    }
    layers[[length(layers) + 1L]] <- psychro_layer(
        stat = StatComfortGivoniMeanOutdoor,
        data = comfort_layer_data(data), mapping = layer_mapping,
        geom = "path", position = position, show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = utils::modifyList(mean_params, list(
            na.rm = na.rm, strategy = strategy, linetype = "dotted"
        ))
    )

    if (isTRUE(show_labels)) {
        label_params <- params
        if (is.null(label_params$colour) && is.null(label_params$color)) {
            label_params$colour <- "#444444"
        }
        if (is.null(label_params$fontface)) {
            label_params$fontface <- "bold"
        }
        if (is.null(label_params$size)) {
            label_params$size <- 2.7
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            )),
            geom = geomtextpath::GeomTextpath, position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = utils::modifyList(label_params, list(
                na.rm = na.rm, strategy = strategy, label_type = "path",
                text_only = TRUE, upright = FALSE, remove_long = FALSE
            ))
        )
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle)
            )),
            geom = "text", position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = utils::modifyList(label_params, list(
                na.rm = na.rm, strategy = strategy, label_type = "point"
            ))
        )
        mean_label_params <- params
        if (is.null(mean_label_params$colour) &&
                is.null(mean_label_params$color)) {
            mean_label_params$colour <- "#444444"
        }
        if (is.null(mean_label_params$fontface)) {
            mean_label_params$fontface <- "bold"
        }
        if (is.null(mean_label_params$size)) {
            mean_label_params$size <- 2.7
        }
        layers[[length(layers) + 1L]] <- psychro_layer(
            stat = StatComfortGivoniMeanOutdoorLabel,
            data = comfort_layer_data(data),
            mapping = comfort_computed_xy_mapping(ggplot2::aes(
                label = ggplot2::after_stat(label),
                angle = ggplot2::after_stat(angle),
                hjust = ggplot2::after_stat(hjust),
                vjust = ggplot2::after_stat(vjust)
            )),
            geom = "text", position = position,
            show.legend = FALSE, inherit.aes = FALSE,
            params = utils::modifyList(mean_label_params, list(
                na.rm = na.rm, strategy = strategy
            ))
        )
    }
    layers[[length(layers) + 1L]] <- comfort_givoni_foreground_marker(
        strategy = strategy,
        show_label = isTRUE(show_labels),
        colour = mean_params$colour %||% mean_params$color %||% "#444444",
        linewidth = mean_params$linewidth %||% 0.8,
        linetype = "dotted",
        label_size = if (exists("mean_label_params", inherits = FALSE)) {
            mean_label_params$size %||% 2.7
        } else {
            2.7
        },
        fontface = if (exists("mean_label_params", inherits = FALSE)) {
            mean_label_params$fontface %||% "bold"
        } else {
            "bold"
        }
    )

    layers
}

comfort_givoni_foreground_marker <- function(strategy, show_label, colour,
                                             linewidth, linetype, label_size,
                                             fontface) {
    structure(
        list(
            type = "givoni_mean_outdoor",
            strategy = strategy,
            show_label = show_label,
            colour = colour,
            linewidth = linewidth,
            linetype = linetype,
            label_size = label_size,
            fontface = fontface
        ),
        class = "PsyComfortForeground"
    )
}

comfort_heat_index_foreground_labels <- function(model, n, params) {
    structure(
        list(
            type = "heat_index_labels",
            model = model,
            n = n,
            colour = params$colour %||% params$color %||% "#444444",
            alpha = params$alpha %||% NA_real_,
            size = params$size %||% 3,
            family = params$family %||% "",
            fontface = params$fontface %||% "bold",
            lineheight = params$lineheight %||% 1.2,
            hjust = params$hjust %||% 0.5,
            vjust = params$vjust %||% 0.5
        ),
        class = "PsyComfortForeground"
    )
}

GeomComfortNullText <- ggplot2::ggproto(
    "GeomComfortNullText", ggplot2::GeomText,
    draw_panel = function(data, panel_params, coord, ...) {
        grid::nullGrob()
    }
)

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
#' @examples
#' # Use the default PMV colour scale.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(name = "PMV")
#'
#' # Focus the legend on the usual comfort range.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(limits = c(-1.5, 1.5), name = "PMV")
#'
#' # Use a custom diverging palette.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_overlay(n = c(45, 30)) +
#'     scale_fill_comfort_pmv(
#'         low = "#2166AC",
#'         mid = "white",
#'         high = "#B2182B",
#'         name = "PMV"
#'     )
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
        "label_path", "units", "pres", "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales, model = comfort_model_pmv(),
                             metric = NULL, breaks = NULL,
                             n = NULL, contour_method = "auto",
                             label_path = FALSE, units, pres,
                             mollier = FALSE, tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_contour_data(
            model, metric, breaks, comfort_default_n(model, n), units, pres,
            mollier, tdb_lim, hum_lim, contour_method = contour_method,
            label_path = label_path
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
StatComfortHeatIndexZone <- ggplot2::ggproto(
    "StatComfortHeatIndexZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(
    ),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "n", "category_id", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             model = comfort_model_heat_index(),
                             n = c(160, 100), category_id = 1L,
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_heat_index_zone_data(
            model, category_id, comfort_grid_n(n), units, pres,
            mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortHeatIndexContour <- ggplot2::ggproto(
    "StatComfortHeatIndexContour", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "n", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             model = comfort_model_heat_index(),
                             n = c(160, 100), units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_contour_data(
            model, "heat_index", comfort_heat_index_thresholds(units),
            comfort_grid_n(n), units, pres, mollier, tdb_lim, hum_lim,
            contour_method = "isoband"
        )
    }
)

#' @noRd
StatComfortHeatIndexLabel <- ggplot2::ggproto(
    "StatComfortHeatIndexLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "model", "n", "units", "pres",
        "mollier", "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             model = comfort_model_heat_index(),
                             n = c(160, 100), units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_heat_index_label_data(
            model, comfort_grid_n(n), units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniZone <- ggplot2::ggproto(
    "StatComfortGivoniZone", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "zone", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             zone = NULL, units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_givoni_zone_data(
            strategy, zone, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniLabel <- ggplot2::ggproto(
    "StatComfortGivoniLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "label_type", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             label_type = "point",
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_givoni_label_data(
            strategy, label_type, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniMeanOutdoor <- ggplot2::ggproto(
    "StatComfortGivoniMeanOutdoor", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_givoni_mean_outdoor_data(
            strategy, units, pres, mollier, tdb_lim, hum_lim
        )
    }
)

#' @noRd
StatComfortGivoniMeanOutdoorLabel <- ggplot2::ggproto(
    "StatComfortGivoniMeanOutdoorLabel", ggplot2::Stat,

    setup_data = function(self, data, params) {
        init_stat_data(data, params)
    },

    default_aes = ggplot2::aes(),

    dropped_aes = c("pres", "units"),

    extra_params = c(
        "na.rm", "strategy", "units", "pres", "mollier",
        "tdb_lim", "hum_lim"
    ),

    compute_panel = function(self, data, scales,
                             strategy = comfort_strategy_givoni(),
                             units, pres, mollier = FALSE,
                             tdb_lim = NULL, hum_lim = NULL,
                             na.rm = FALSE) {
        units <- comfort_stat_units(data, units)
        pres <- comfort_stat_pressure(data, pres)
        comfort_givoni_mean_outdoor_label_data(
            strategy, units, pres, mollier, tdb_lim, hum_lim
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
            !model$type %in% c("pmv", "set", "adaptive", "heat_index")) {
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

comfort_check_givoni_strategy <- function(strategy) {
    if (!inherits(strategy, "PsyComfortGivoniStrategy")) {
        stop("`strategy` must be created by comfort_strategy_givoni().",
            call. = FALSE)
    }
    strategy$units <- match.arg(strategy$units, c("SI", "IP"))
    strategy
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

comfort_computed_xy_mapping <- function(mapping = NULL) {
    x <- y <- NULL
    xy <- ggplot2::aes(
        x = ggplot2::after_stat(x),
        y = ggplot2::after_stat(y)
    )
    if (is.null(mapping)) {
        return(xy)
    }
    utils::modifyList(mapping, xy)
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

comfort_heat_index_f <- function(tdb_f, rh, solar_exposure) {
    hi <- tdb_f
    valid <- is.finite(tdb_f) & is.finite(rh) & is.finite(solar_exposure)
    warm <- valid & tdb_f > 40
    if (any(warm)) {
        simple <- 0.5 * (
            tdb_f[warm] + (61 + 1.2 * (tdb_f[warm] - 68) + 0.094 * rh[warm])
        )
        idx <- which(warm)
        hi[idx] <- simple

        roth <- idx[simple > 79]
        if (length(roth)) {
            t <- tdb_f[roth]
            r <- rh[roth]
            value <- 2.04901523 * t - 42.379 + 10.14333127 * r -
                0.22475541 * t * r - 0.00683783 * t^2 -
                0.05481717 * r^2 + 0.00122874 * t^2 * r +
                0.00085282 * t * r^2 - 0.00000199 * t^2 * r^2

            low_rh <- r <= 13 & t >= 80 & t <= 112
            if (any(low_rh)) {
                value[low_rh] <- value[low_rh] -
                    (13 - r[low_rh]) / 4 *
                    sqrt((17 - abs(t[low_rh] - 95)) / 17)
            }
            high_rh <- r > 85 & t >= 80 & t <= 87
            if (any(high_rh)) {
                value[high_rh] <- value[high_rh] +
                    (r[high_rh] - 85) / 10 * ((87 - t[high_rh]) / 5)
            }
            hi[roth] <- value
        }
    }

    hi[valid] <- hi[valid] + 8 * solar_exposure[valid]
    hi[!valid] <- NA_real_
    hi
}

comfort_heat_index_thresholds <- function(units) {
    units <- match.arg(units, c("SI", "IP"))
    if (units == "IP") {
        c(80, 90, 103, 125)
    } else {
        get_c_from_f(c(80, 90, 103, 125))
    }
}

comfort_heat_index_category <- function(heat_index_f) {
    labels <- c(
        "none", "caution", "extreme caution", "danger", "extreme danger"
    )
    id <- findInterval(heat_index_f, c(80, 90, 103, 125))
    id[!is.finite(heat_index_f)] <- NA_integer_
    category <- labels[id + 1L]
    category[is.na(id)] <- NA_character_
    new_data_frame(list(category = category, category_id = id))
}

comfort_heat_index_zone_specs <- function() {
    list(
        list(id = 1L, label = "CAUTION", fill = "#FFE66D"),
        list(id = 2L, label = "EXTREME CAUTION", fill = "#FFB347"),
        list(id = 3L, label = "DANGER", fill = "#FF7043"),
        list(id = 4L, label = "EXTREME DANGER", fill = "#D73027")
    )
}

comfort_heat_index_zone_data <- function(model, category_id, n, units, pres,
                                         mollier, tdb_lim, hum_lim) {
    thresholds <- comfort_heat_index_thresholds(units)
    if (!category_id %in% seq_along(thresholds)) {
        return(comfort_empty_band())
    }

    m <- comfort_grid_matrix(
        model, "heat_index", n, units, pres, tdb_lim, hum_lim,
        at = "nodes", boundary = "saturation"
    )
    z_range <- range(m$value, finite = TRUE)
    if (!all(is.finite(z_range))) {
        return(comfort_empty_band())
    }

    low <- thresholds[[category_id]]
    high <- if (category_id < length(thresholds)) {
        thresholds[[category_id + 1L]]
    } else {
        max(z_range[[2L]], low) + max(1, abs(low)) * 1e-6
    }
    if (high <= low || z_range[[2L]] < low) {
        return(comfort_empty_band())
    }
    high <- min(high, z_range[[2L]] + max(1, abs(z_range[[2L]])) * 1e-6)

    bands <- isoband::isobands(
        x = m$tdb, y = m$humratio, z = t(m$value),
        levels_low = low, levels_high = high
    )
    out <- comfort_isoband_data(
        bands, low, high, m$metric, mollier, geom = "polygon"
    )
    if (nrow(out)) {
        out$category_id <- category_id
        out$category <- comfort_heat_index_zone_specs()[[category_id]]$label
    }
    out
}

comfort_heat_index_label_data <- function(model, n, units, pres, mollier,
                                          tdb_lim, hum_lim) {
    m <- comfort_grid_matrix(
        model, "heat_index", n, units, pres, tdb_lim, hum_lim,
        at = "centers", boundary = "saturation"
    )
    specs <- comfort_heat_index_zone_specs()
    thresholds <- comfort_heat_index_thresholds(units)
    labels <- vector("list", length(specs))
    values <- as.vector(m$value)
    grid <- expand.grid(tdb = m$tdb, humratio = m$humratio)
    for (i in seq_along(specs)) {
        low <- thresholds[[i]]
        high <- if (i < length(thresholds)) thresholds[[i + 1L]] else Inf
        target <- if (is.finite(high)) (low + high) / 2 else low + 4
        keep <- is.finite(values) & values >= low & values < high
        if (!any(keep)) {
            next
        }
        idx <- which(keep)[which.min(abs(values[keep] - target))]
        labels[[i]] <- new_data_frame(list(
            tdb = grid$tdb[[idx]],
            humratio = grid$humratio[[idx]],
            label = specs[[i]]$label,
            category_id = specs[[i]]$id,
            category = specs[[i]]$label,
            angle = 0,
            group = i
        ))
    }
    labels <- labels[!vapply(labels, is.null, logical(1L))]
    if (!length(labels)) {
        return(new_data_frame(list(
            tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
            label = character(), category_id = integer(), category = character(),
            angle = numeric(), group = integer()
        )))
    }
    out <- do.call(rbind, labels)
    row.names(out) <- NULL
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
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
        adaptive = c(240L, 160L),
        heat_index = c(160L, 100L)
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
    # Saturation roots are matched back to horizontal slices after root finding.
    # Keep a small PMV tolerance so boundary cap points are not dropped by
    # roundoff, which would leave visible holes just under the saturation line.
    value_tol <- 5e-5
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
    breaks <- comfort_band_breaks(m$metric, m$value, levels, units)
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
                                 contour_method = c("auto", "root", "isoband"),
                                 label_path = FALSE) {
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
        breaks <- comfort_contour_breaks("pmv", numeric(), units)
        }
        out <- comfort_pmv_curve_data(
            model, breaks, n[[1L]], units, pres, mollier, tdb_lim, hum_lim,
            label = "none"
        )
        out <- comfort_add_contour_labels(out)
        if (isTRUE(label_path)) {
            out <- comfort_orient_contour_label_paths(out)
        }
        return(out)
    }

    m <- comfort_grid_matrix(
        model, metric, n, units, pres, tdb_lim, hum_lim,
        at = "nodes", boundary = "saturation"
    )
    z <- m$value
    if (is.null(breaks)) {
        breaks <- comfort_contour_breaks(m$metric, z, units)
    }
    breaks <- breaks[is.finite(breaks)]
    if (!length(breaks)) {
        return(comfort_empty_contour())
    }

    lines <- isoband::isolines(
        x = m$tdb, y = m$humratio, z = t(z), levels = breaks
    )
    out <- comfort_isoband_data(lines, breaks, breaks, m$metric, mollier, geom = "path")
    out <- comfort_add_contour_labels(out)
    if (isTRUE(label_path)) {
        out <- comfort_orient_contour_label_paths(out)
    }
    out
}

comfort_empty_contour <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        level = numeric(), value = numeric(), group = character(),
        label = character(), metric = character()
    ))
}

comfort_add_contour_labels <- function(data) {
    if (!nrow(data)) {
        return(data)
    }
    data$value <- data$level
    data$label <- comfort_format_contour_level(data$level, data$metric)
    data
}

comfort_orient_contour_label_paths <- function(data) {
    if (!nrow(data) || !"group" %in% names(data)) {
        return(data)
    }

    x_scale <- diff(range(data$x, finite = TRUE))
    y_scale <- diff(range(data$y, finite = TRUE))
    if (!is.finite(x_scale) || x_scale <= 0) {
        x_scale <- 1
    }
    if (!is.finite(y_scale) || y_scale <= 0) {
        y_scale <- 1
    }

    groups <- split(seq_len(nrow(data)), data$group)
    out <- lapply(groups, function(i) {
        group_data <- data[i, , drop = FALSE]
        if (nrow(group_data) < 2L) {
            return(group_data)
        }

        dx <- (group_data$x[[nrow(group_data)]] - group_data$x[[1L]]) / x_scale
        dy <- (group_data$y[[nrow(group_data)]] - group_data$y[[1L]]) / y_scale
        if (!is.finite(dx)) {
            dx <- 0
        }
        if (!is.finite(dy)) {
            dy <- 0
        }

        reverse <- if (abs(dy) >= abs(dx)) dy < 0 else dx < 0
        if (isTRUE(reverse)) {
            group_data <- group_data[rev(seq_len(nrow(group_data))), , drop = FALSE]
        }
        group_data
    })

    out <- do.call(rbind, out)
    row.names(out) <- NULL
    out
}

comfort_format_contour_level <- function(level, metric) {
    metric <- rep(metric, length.out = length(level))
    out <- scales::number(level, accuracy = NULL, trim = TRUE)
    pmv <- metric == "pmv"
    out[pmv] <- comfort_format_pmv_level(level[pmv])
    out
}

comfort_contour_label_mapping <- function(mapping) {
    label <- NULL
    out <- mapping %||% ggplot2::aes()
    label_mapping <- ggplot2::aes(label = ggplot2::after_stat(label))
    out$label <- label_mapping$label
    out
}

comfort_contour_label_params <- function(params, label_size = NULL) {
    params$size <- label_size %||% params$size %||% 2.8
    params$text_only <- FALSE
    params$upright <- FALSE
    params$remove_long <- TRUE
    params$gap <- TRUE
    params$padding <- params$padding %||% grid::unit(1, "pt")
    params
}

comfort_contour_breaks <- function(metric, z, units = "SI") {
    if (metric == "pmv") {
        return(seq(-3, 3, by = 0.5))
    }
    if (metric == "heat_index") {
        return(comfort_heat_index_thresholds(units))
    }
    pretty(range(z, finite = TRUE), n = 8)
}

comfort_band_breaks <- function(metric, z, levels = NULL, units = "SI") {
    if (identical(metric, "acceptability") && is.null(levels)) {
        return(c(-0.5, 0.5, 1.5))
    }

    if (!is.null(levels) && length(levels) > 1L) {
        breaks <- sort(unique(as.numeric(levels)))
        return(breaks[is.finite(breaks)])
    }

    if (identical(metric, "heat_index") && is.null(levels)) {
        z_range <- range(z, finite = TRUE)
        if (!all(is.finite(z_range))) {
            return(numeric())
        }
        eps <- max(1, abs(z_range)) * 1e-9
        breaks <- c(
            z_range[[1L]] - eps,
            comfort_heat_index_thresholds(units),
            z_range[[2L]] + eps
        )
        breaks <- sort(unique(breaks[breaks > z_range[[1L]] - 2 * eps &
            breaks < z_range[[2L]] + 2 * eps]))
        return(breaks)
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
    range <- comfort_zone_range(model, metric, range, units)
    if (comfort_model_type(model) == "pmv" && metric == "pmv") {
        return(comfort_pmv_band_data(
            model, range, n[[1L]], units, pres, mollier, tdb_lim, hum_lim
        ))
    }
    comfort_band_data(model, metric, range, n, units, pres, mollier,
        tdb_lim, hum_lim)
}

comfort_zone_range <- function(model, metric, range, units = "SI") {
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
        heat_index = comfort_heat_index_thresholds(units)[c(1L, 2L)],
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

comfort_givoni_base_temp <- function(strategy) {
    mean_outdoor_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    round(17.6 + 0.31 * mean_outdoor_si - 3.5, 1L)
}

comfort_givoni_zone_specs <- function() {
    new_data_frame(list(
        zone = c(
            "comfort", "natural_ventilation", "internal_gains",
            "passive_solar_heating", "active_solar_heating",
            "evaporative_cooling", "mass_cooling",
            "mass_cooling_night_ventilation", "winter",
            "air_conditioning", "air_conditioning_dehumidification",
            "humidification"
        ),
        label = c(
            "COMFORT\nZONE", "NATURAL VENTILATION", "INTERNAL\nGAINS",
            "PASSIVE SOLAR\nHEATING", "ACTIVE\nSOLAR\nHEATING",
            "EVAPORATIVE COOLING", "MASS COOLING",
            "MASS COOLING &\nNIGHT VENTILATION", "WINTER",
            "AIR-CONDITIONING", "AIR-CONDITIONING &\nDEHUMIDIFICATION",
            "HUMIDIFICATION"
        ),
        fill = c(
            "#5BD96A", rep(NA_character_, 11L)
        ),
        linetype = c(
            "solid", "solid", "solid", "solid", "solid", "solid",
            "solid", "solid", "dashed", "longdash", "longdash", "solid"
        ),
        filled = c(TRUE, rep(FALSE, 11L)),
        draw_zone = c(
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            FALSE, FALSE
        )
    ))
}

comfort_zone_style_fields <- function() {
    c("fill", "colour", "color", "linewidth", "linetype", "alpha", "linejoin")
}

comfort_zone_fill_is_set <- function(fill) {
    !is.null(fill) && length(fill) == 1L && !is.na(fill)
}

comfort_givoni_check_zone_style <- function(zone_style, zone_names) {
    if (is.null(zone_style)) {
        return(list())
    }
    if (!is.list(zone_style) || is.null(names(zone_style)) ||
            any(!nzchar(names(zone_style)))) {
        stop(
            "`zone_style` must be a named list of comfort zone style ",
            "overrides.",
            call. = FALSE
        )
    }
    unknown <- setdiff(names(zone_style), zone_names)
    if (length(unknown)) {
        stop(
            "Unknown Givoni zone style name: ",
            paste(unknown, collapse = ", "),
            call. = FALSE
        )
    }
    zone_style
}

comfort_zone_style_to_params <- function(style) {
    if (inherits(style, "PsyComfortZoneElement") ||
            inherits(style, "ggplot2::element_polygon")) {
        out <- list(
            fill = style$fill,
            colour = style$colour,
            linewidth = style$linewidth,
            linetype = style$linetype,
            linejoin = style$linejoin
        )
        if (!is.null(style$alpha)) {
            out$alpha <- style$alpha
        }
    } else if (is.list(style)) {
        out <- style
    } else {
        stop(
            "`zone_style` values must be created by element_comfort_zone(), ",
            "ggplot2::element_polygon(), or ordinary named lists.",
            call. = FALSE
        )
    }

    if (is.null(names(out))) {
        stop("Zone style lists must be named.", call. = FALSE)
    }
    unknown <- setdiff(names(out), comfort_zone_style_fields())
    if (length(unknown)) {
        stop(
            "Unknown comfort zone style field: ",
            paste(unknown, collapse = ", "),
            call. = FALSE
        )
    }
    if (!is.null(out$color)) {
        out$colour <- out$color
        out$color <- NULL
    }
    out <- out[!vapply(out, ggplot2::is_waiver, logical(1L))]
    out <- out[!vapply(out, is.null, logical(1L))]
    out
}

comfort_givoni_zone_params <- function(spec, params, zone_style, zone_alpha,
                                       na.rm, strategy) {
    zone_is_filled <- isTRUE(spec$filled[[1L]])
    defaults <- list(
        na.rm = na.rm,
        strategy = strategy,
        zone = spec$zone[[1L]],
        fill = if (zone_is_filled) spec$fill[[1L]] else NA_character_,
        colour = "#444444",
        linewidth = 0.9,
        linetype = spec$linetype[[1L]],
        alpha = if (zone_is_filled) zone_alpha else 1
    )
    out <- utils::modifyList(defaults, params)
    style <- zone_style[[spec$zone[[1L]]]]
    if (!is.null(style)) {
        style_params <- comfort_zone_style_to_params(style)
        out <- utils::modifyList(out, style_params)
        if (!("alpha" %in% names(style_params)) &&
                "fill" %in% names(style_params) &&
                comfort_zone_fill_is_set(style_params$fill)) {
            out$alpha <- zone_alpha
        }
    }
    out
}

comfort_givoni_empty_zone <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        zone = character(), label = character(), group = integer(),
        subgroup = integer()
    ))
}

comfort_givoni_empty_label <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(), x = numeric(), y = numeric(),
        zone = character(), label = character(), angle = numeric(),
        hjust = numeric(), vjust = numeric(), group = integer()
    ))
}

comfort_givoni_humratio <- function(tdb_si, rh, pressure_pa) {
    with_units("SI", psychrolib::GetHumRatioFromRelHum(tdb_si, rh / 100, pressure_pa))
}

comfort_givoni_hum_gkg <- function(tdb_si, rh, pressure_pa) {
    comfort_givoni_humratio(tdb_si, rh, pressure_pa) * 1000
}

comfort_givoni_point <- function(tdb_si, hum_gkg) {
    new_data_frame(list(tdb_si = tdb_si, humratio = hum_gkg / 1000))
}

comfort_givoni_rh_path <- function(t0, rh0, t1, rh1, pressure_pa,
                                   max_gkg = Inf, n = NULL) {
    if (is.null(n)) {
        n <- max(8L, ceiling(abs(t1 - t0) * 4L) + 1L)
    }
    tdb <- seq(t0, t1, length.out = n)
    rh <- seq(rh0, rh1, length.out = n)
    hum_gkg <- pmin(
        comfort_givoni_hum_gkg(tdb, rh, pressure_pa),
        max_gkg
    )
    new_data_frame(list(tdb_si = tdb, humratio = hum_gkg / 1000))
}

comfort_givoni_polygon <- function(zone, base, pressure_pa, tdb_max_si,
                                   hum_min_gkg) {
    if (zone %in% c("air_conditioning_dehumidification", "humidification")) {
        return(comfort_givoni_point(numeric(), numeric()))
    }
    hum20 <- function(tdb) comfort_givoni_hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) comfort_givoni_hum_gkg(tdb, 30, pressure_pa)
    hum50 <- function(tdb) comfort_givoni_hum_gkg(tdb, 50, pressure_pa)
    hum80 <- function(tdb) comfort_givoni_hum_gkg(tdb, 80, pressure_pa)
    hum100 <- function(tdb) comfort_givoni_hum_gkg(tdb, 100, pressure_pa)
    max_comfort_gkg <- min(16, hum80(base + 5))
    bottom20 <- hum20(base)
    evap_left <- base + 2.4528 * (bottom20 - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)

    parts <- switch(zone,
        comfort = list(
            comfort_givoni_rh_path(base, 80, base + 5, 80, pressure_pa, 16),
            comfort_givoni_point(base + 7, min(max_comfort_gkg, hum50(base + 7))),
            comfort_givoni_point(base + 7, hum20(base + 7)),
            comfort_givoni_rh_path(base + 7, 20, base, 20, pressure_pa),
            comfort_givoni_point(base, hum80(base))
        ),
        natural_ventilation = list(
            comfort_givoni_rh_path(base, 100, base + 7, 100, pressure_pa),
            comfort_givoni_point(base + 12, hum50(base + 12)),
            comfort_givoni_point(base + 12, hum20(base + 12)),
            comfort_givoni_rh_path(base + 12, 20, base, 20, pressure_pa),
            comfort_givoni_point(base, hum100(base))
        ),
        internal_gains = list(
            comfort_givoni_rh_path(base - 2.5, 20, base - 7, 20, pressure_pa),
            comfort_givoni_point(base - 7.5, hum20(base - 7.5)),
            comfort_givoni_point(base - 7.5, min(hum80(base - 7.5), 16)),
            comfort_givoni_rh_path(base - 7.5, 80, base - 2.5, 80,
                pressure_pa, 16)
        ),
        passive_solar_heating = list(
            comfort_givoni_point(base + 3.5, 0),
            comfort_givoni_point(base - 12, 0),
            comfort_givoni_point(base - 12, hum100(base - 12)),
            comfort_givoni_rh_path(base - 12, 100, base - 1, 100, pressure_pa)
        ),
        active_solar_heating = list(
            comfort_givoni_point(base - 13, 0),
            comfort_givoni_point(base - 16, 0),
            comfort_givoni_point(base - 16, hum100(base - 16)),
            comfort_givoni_rh_path(base - 16, 100, base - 13, 100, pressure_pa)
        ),
        evaporative_cooling = list(
            comfort_givoni_point(base + 5, max_comfort_gkg),
            comfort_givoni_point(base + 16, min(max_comfort_gkg, hum30(base + 16))),
            comfort_givoni_point(base + 19, min(max_comfort_gkg, hum20(base + 19))),
            comfort_givoni_point(base + 21, min(max_comfort_gkg,
                comfort_givoni_hum_gkg(base + 21, 10, pressure_pa))),
            comfort_givoni_point(base + 21, 0),
            comfort_givoni_point(evap_left, 0),
            comfort_givoni_point(base, bottom20)
        ),
        mass_cooling = list(
            comfort_givoni_point(base + 5, max_comfort_gkg),
            comfort_givoni_point(base + 13, max_comfort_gkg),
            comfort_givoni_point(base + 17, min(max_comfort_gkg, hum30(base + 17))),
            comfort_givoni_point(base + 17, min(max_comfort_gkg, hum20(base))),
            comfort_givoni_point(base, min(max_comfort_gkg, hum20(base)))
        ),
        mass_cooling_night_ventilation = list(
            comfort_givoni_point(base + 13, max_comfort_gkg),
            comfort_givoni_point(base + 20, max_comfort_gkg),
            comfort_givoni_point(base + 24, min(max_comfort_gkg, hum20(base + 24))),
            comfort_givoni_point(base + 24, min(max_comfort_gkg, hum20(base))),
            comfort_givoni_point(base, min(max_comfort_gkg, hum20(base)))
        ),
        winter = list(
            comfort_givoni_rh_path(base - 0.5, 20, base - 2, 20, pressure_pa),
            comfort_givoni_point(base - 2, hum20(base - 2)),
            comfort_givoni_point(base - 2, min(hum80(base - 2), 16)),
            comfort_givoni_rh_path(base - 2, 80, base - 0.5, 80,
                pressure_pa, 16)
        ),
        air_conditioning = list(
            comfort_givoni_point(base + 20, max_comfort_gkg),
            comfort_givoni_point(right_air, max_comfort_gkg),
            comfort_givoni_point(right_air, 0),
            comfort_givoni_point(base + 21, 0)
        ),
        air_conditioning_dehumidification = list(
            comfort_givoni_point(base + 20, max_comfort_gkg),
            comfort_givoni_point(right_air, max_comfort_gkg),
            comfort_givoni_point(right_air, 30),
            comfort_givoni_point(base + 20, 30)
        ),
        humidification = list(
            comfort_givoni_point(base - 12, 0),
            comfort_givoni_point(base, 0),
            comfort_givoni_point(base, hum20(base)),
            comfort_givoni_point(base - 12, hum20(base - 12))
        ),
        stop("Unknown Givoni zone: ", zone, call. = FALSE)
    )

    out <- do.call(rbind, parts)
    row.names(out) <- NULL
    out
}

comfort_givoni_zone_data <- function(strategy, zone, units, pres, mollier,
                                     tdb_lim, hum_lim) {
    strategy <- comfort_check_givoni_strategy(strategy)
    specs <- comfort_givoni_zone_specs()
    if (is.null(zone)) {
        zone <- specs$zone[specs$draw_zone]
    }
    zone <- match.arg(zone, specs$zone, several.ok = TRUE)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    base <- comfort_givoni_base_temp(strategy)
    tdb_max_si <- comfort_to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- narrow_hum(lim$hum[[1L]], units) * 1000

    pieces <- vector("list", length(zone))
    for (i in seq_along(zone)) {
        poly <- comfort_givoni_polygon(zone[[i]], base, pressure_pa,
            tdb_max_si, hum_min_gkg)
        if (!nrow(poly)) {
            next
        }
        spec <- specs[match(zone[[i]], specs$zone), , drop = FALSE]
        pieces[[i]] <- new_data_frame(list(
            tdb = comfort_from_si_temp(poly$tdb_si, units),
            humratio = poly$humratio,
            zone = spec$zone,
            label = spec$label,
            group = i,
            subgroup = 1L
        ))
    }
    pieces <- pieces[!vapply(pieces, is.null, logical(1L))]
    if (!length(pieces)) {
        return(comfort_givoni_empty_zone())
    }
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
    out <- comfort_givoni_clip_humratio(out, lim$hum, units)
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_givoni_label_path_entry <- function(zone, label, path,
                                            hjust = 0.5, vjust = 0.5,
                                            group = 1L) {
    if (!nrow(path)) {
        return(comfort_givoni_empty_label())
    }
    new_data_frame(list(
        tdb_si = path$tdb_si,
        humratio = path$humratio,
        zone = zone,
        label = label,
        angle = NA_real_,
        hjust = hjust,
        vjust = vjust,
        group = group
    ))
}

comfort_givoni_label_path_specs <- function(base, pressure_pa, tdb_max_si,
                                            hum_min_gkg) {
    max_comfort_gkg <- min(16,
        comfort_givoni_hum_gkg(base + 5, 80, pressure_pa)
    )
    hum20 <- function(tdb) comfort_givoni_hum_gkg(tdb, 20, pressure_pa)
    hum30 <- function(tdb) comfort_givoni_hum_gkg(tdb, 30, pressure_pa)
    hum80 <- function(tdb) comfort_givoni_hum_gkg(tdb, 80, pressure_pa)
    hum100 <- function(tdb) comfort_givoni_hum_gkg(tdb, 100, pressure_pa)
    evap_left <- base + 2.4528 * (hum20(base) - hum_min_gkg)
    right_air <- max(tdb_max_si, base + 25.5)
    air_label_x <- min(right_air - 1.0, tdb_max_si - 0.8)
    air_label_x <- max(air_label_x, base + 21)

    paths <- list(
        comfort_givoni_label_path_entry(
            "natural_ventilation", "NATURAL VENTILATION",
            comfort_givoni_rh_path(base, 100, base + 7, 100, pressure_pa),
            hjust = 0.5, vjust = 1.8, group = 1L
        ),
        comfort_givoni_label_path_entry(
            "internal_gains", "INTERNAL GAINS",
            rbind(
                comfort_givoni_point(base - 7.5,
                    min(hum80(base - 7.5), 16)),
                comfort_givoni_point(base - 7.5, hum20(base - 7.5))
            ),
            hjust = 0.5, vjust = -0.25, group = 2L
        ),
        comfort_givoni_label_path_entry(
            "passive_solar_heating", "PASSIVE SOLAR",
            rbind(
                comfort_givoni_point(base - 12, hum100(base - 12)),
                comfort_givoni_point(base - 12, 0)
            ),
            hjust = 0.5, vjust = -0.25, group = 3L
        ),
        comfort_givoni_label_path_entry(
            "active_solar_heating", "ACTIVE SOLAR",
            rbind(
                comfort_givoni_point(base - 16, hum100(base - 16)),
                comfort_givoni_point(base - 16, 0)
            ),
            hjust = 0.5, vjust = -0.25, group = 4L
        ),
        comfort_givoni_label_path_entry(
            "evaporative_cooling", "EVAPORATIVE COOLING",
            rbind(
                comfort_givoni_point(evap_left, 0),
                comfort_givoni_point(base + 21, 0)
            ),
            hjust = 0.68, vjust = -0.35, group = 5L
        ),
        comfort_givoni_label_path_entry(
            "mass_cooling", "MASS COOLING",
            rbind(
                comfort_givoni_point(base + 17,
                    min(max_comfort_gkg, hum30(base + 17))),
                comfort_givoni_point(base + 17,
                    min(max_comfort_gkg, hum20(base)))
            ),
            hjust = 0.48, vjust = -0.25, group = 6L
        ),
        comfort_givoni_label_path_entry(
            "mass_cooling_night_ventilation",
            "MASS COOLING &\nNIGHT VENTILATION",
            rbind(
                comfort_givoni_point(base + 24,
                    min(max_comfort_gkg, hum20(base + 24))),
                comfort_givoni_point(base + 24,
                    min(max_comfort_gkg, hum20(base)))
            ),
            hjust = 0.5, vjust = -0.25, group = 7L
        ),
        comfort_givoni_label_path_entry(
            "winter", "WINTER",
            rbind(
                comfort_givoni_point(base - 2, min(hum80(base - 2), 16)),
                comfort_givoni_point(base - 2, hum20(base - 2))
            ),
            hjust = 0.5, vjust = -0.25, group = 8L
        ),
        comfort_givoni_label_path_entry(
            "air_conditioning", "AIR-CONDITIONING",
            rbind(
                comfort_givoni_point(air_label_x, max_comfort_gkg),
                comfort_givoni_point(air_label_x, 0)
            ),
            hjust = 0.5, vjust = -0.25, group = 9L
        ),
        comfort_givoni_label_path_entry(
            "humidification", "HUMIDIFICATION",
            rbind(
                comfort_givoni_point(base - 12, 0),
                comfort_givoni_point(base, 0)
            ),
            hjust = 0.5, vjust = -0.35, group = 10L
        )
    )
    out <- do.call(rbind, paths)
    row.names(out) <- NULL
    out
}

comfort_givoni_label_point_specs <- function(base, pressure_pa, tdb_max_si,
                                             hum_min_gkg = 0) {
    max_comfort_gkg <- min(16,
        comfort_givoni_hum_gkg(base + 5, 80, pressure_pa)
    )
    heating_x <- base - 18
    heating_hum_gkg <- mean(c(
        hum_min_gkg,
        comfort_givoni_hum_gkg(heating_x, 100, pressure_pa)
    ))
    new_data_frame(list(
        zone = c(
            "comfort", "heating", "air_conditioning_dehumidification"
        ),
        label = c(
            "COMFORT\nZONE", "HEATING",
            "AIR-CONDITIONING &\nDEHUMIDIFICATION"
        ),
        tdb_si = c(
            base + 3.5, base - 18, base + 19
        ),
        hum_gkg = c(
            max_comfort_gkg * 0.55, heating_hum_gkg, max_comfort_gkg + 6
        ),
        angle = c(0, 270, 0),
        hjust = c(0.5, 0.5, 0.5),
        vjust = c(0.5, 0.5, 0.5)
    ))
}

comfort_givoni_label_data <- function(strategy, label_type, units, pres,
                                      mollier, tdb_lim, hum_lim) {
    label_type <- match.arg(label_type, c("path", "point"))
    strategy <- comfort_check_givoni_strategy(strategy)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    base <- comfort_givoni_base_temp(strategy)
    tdb_max_si <- comfort_to_si_temp(lim$tdb[[2L]], units)
    hum_min_gkg <- narrow_hum(lim$hum[[1L]], units) * 1000

    if (label_type == "path") {
        labels <- comfort_givoni_label_path_specs(
            base, pressure_pa, tdb_max_si, hum_min_gkg
        )
        out <- new_data_frame(list(
            tdb = comfort_from_si_temp(labels$tdb_si, units),
            humratio = labels$humratio,
            zone = labels$zone,
            label = labels$label,
            angle = labels$angle,
            hjust = labels$hjust,
            vjust = labels$vjust,
            group = labels$group
        ))
        out <- comfort_givoni_clip_humratio(out, lim$hum, units)
        return(psychro_output_xy(out, out$tdb, out$humratio, mollier))
    }

    labels <- comfort_givoni_label_point_specs(
        base, pressure_pa, tdb_max_si, hum_min_gkg
    )
    out <- new_data_frame(list(
        tdb = comfort_from_si_temp(labels$tdb_si, units),
        humratio = labels$hum_gkg / 1000,
        zone = labels$zone,
        label = labels$label,
        angle = labels$angle,
        hjust = labels$hjust,
        vjust = labels$vjust,
        group = seq_len(nrow(labels))
    ))
    out <- comfort_givoni_clip_humratio(out, lim$hum, units)
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_givoni_clip_humratio <- function(data, hum_lim, units) {
    hum_lim <- narrow_hum(hum_lim, units)
    data$humratio <- pmin(pmax(data$humratio, hum_lim[[1L]]), hum_lim[[2L]])
    data
}

comfort_givoni_mean_outdoor_label_angle <- function(mollier) {
    if (isTRUE(mollier)) 0 else 270
}

comfort_givoni_mean_outdoor_label_vjust <- function(mollier) {
    if (isTRUE(mollier)) 1.25 else -0.25
}

comfort_givoni_mean_outdoor_marker <- function(mean_si, pressure_pa,
                                               hum_lim_narrow) {
    hum_sat <- comfort_givoni_humratio(mean_si, 100, pressure_pa)
    if (!is.finite(hum_sat)) {
        return(NULL)
    }
    hum_extension <- max(0.0015, diff(hum_lim_narrow) * 0.08)
    hum_top <- min(hum_lim_narrow[[2L]], hum_sat + hum_extension)
    if (!is.finite(hum_top) || hum_top <= hum_lim_narrow[[1L]]) {
        return(NULL)
    }
    hum_label <- min(hum_top, hum_sat + hum_extension * 0.65)
    hum_label <- pmax(hum_lim_narrow[[1L]], hum_label)

    list(saturation = hum_sat, top = hum_top, label = hum_label)
}

comfort_givoni_mean_outdoor_data <- function(strategy, units, pres, mollier,
                                             tdb_lim, hum_lim) {
    strategy <- comfort_check_givoni_strategy(strategy)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    mean_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- narrow_hum(lim$hum, units)
    marker <- comfort_givoni_mean_outdoor_marker(
        mean_si, pressure_pa, hum_lim_narrow
    )
    if (is.null(marker)) {
        return(comfort_empty_contour())
    }
    out <- new_data_frame(list(
        tdb = comfort_from_si_temp(c(mean_si, mean_si), units),
        humratio = c(hum_lim_narrow[[1L]], marker$top),
        level = mean_si,
        group = 1L,
        metric = "givoni_mean_outdoor"
    ))
    psychro_output_xy(out, out$tdb, out$humratio, mollier)
}

comfort_givoni_mean_outdoor_label_data <- function(strategy, units, pres,
                                                   mollier, tdb_lim,
                                                   hum_lim) {
    strategy <- comfort_check_givoni_strategy(strategy)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    pressure_pa <- comfort_pressure_pa(pres, units)
    mean_si <- comfort_to_si_temp(strategy$mean_outdoor, strategy$units)
    hum_lim_narrow <- narrow_hum(lim$hum, units)
    marker <- comfort_givoni_mean_outdoor_marker(
        mean_si, pressure_pa, hum_lim_narrow
    )
    if (is.null(marker)) {
        return(comfort_givoni_empty_label())
    }

    label_temp <- comfort_from_si_temp(mean_si, units)
    unit_label <- if (units == "IP") "\u00b0F" else "\u00b0C"
    out <- new_data_frame(list(
        tdb = comfort_from_si_temp(mean_si, units),
        humratio = marker$label,
        zone = "mean_outdoor",
        label = sprintf("%.1f %s", label_temp, unit_label),
        angle = comfort_givoni_mean_outdoor_label_angle(mollier),
        hjust = 0.5,
        vjust = comfort_givoni_mean_outdoor_label_vjust(mollier),
        group = 1L
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
        adaptive = "acceptability",
        heat_index = "heat_index"
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
        ),
        heat_index = comfort_heat_index(
            tdb = tdb, rh = rh, solar_exposure = p$solar_exposure,
            units = units, limit_inputs = p$limit_inputs,
            round_output = p$round_output
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
