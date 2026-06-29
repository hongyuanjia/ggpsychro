#' @include comfort-core.R comfort-pmv.R comfort-adaptive.R comfort-heat-index.R
NULL

# Public constructors and calculation entry points. These stay separate from
# ggplot2 layer assembly so numeric API behavior can be reviewed independently.

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

    # ISO 7730 maps PMV to predicted percentage dissatisfied with an empirical
    # even-power curve, so warm and cool deviations are treated symmetrically.
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
    exposure <- x$solar_exposure
    # Missing exposure follows the vectorized calculator convention and returns
    # NA, but finite values outside Marsh's 0..1 scale are input errors.
    bad_exposure <- is.finite(exposure) & (exposure < 0 | exposure > 1)
    if (any(bad_exposure)) {
        stop("`solar_exposure` must be from 0 to 1.", call. = FALSE)
    }
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
    range <- comfort_check_ordered_breaks(range, "`range`", n_min = 2L)
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
    breaks <- comfort_check_ordered_breaks(breaks, "`breaks`", n_min = 4L)
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
