#' @include comfort-core.R comfort-calc.R
NULL

# Public model and standard constructors keep layer defaults separate from the
# vectorized calculators in comfort-calc.R.

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
#'     geom_comfort_pmv(
#'         model = comfort_model_pmv(met = 1.4, clo = 0.5),
#'         n = c(45, 30)
#'     )
#'
#' # Draw SET as the filled comfort metric.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_set(
#'         model = comfort_model_set(v = 0.2),
#'         n = c(45, 30)
#'     )
#'
#' # Draw the adaptive acceptability region.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_adaptive(
#'         t_running = 22,
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
comfort_model_pmv <- function(
    tr = NULL,
    vr = 0.1,
    met = 1.2,
    clo = 0.5,
    wme = 0,
    model = "7730-2005",
    limit_inputs = FALSE,
    round_output = FALSE
) {
    model <- match.arg(model, "7730-2005")
    # Layer model objects hold fixed environmental assumptions for a whole
    # contour/grid evaluation; vectorized point inputs belong in comfort_pmv().
    tr <- comfort__check_scalar_finite(tr, "`tr`", allow_null = TRUE)
    vr <- comfort__check_scalar_finite(vr, "`vr`")
    met <- comfort__check_scalar_finite(met, "`met`")
    clo <- comfort__check_scalar_finite(clo, "`clo`")
    wme <- comfort__check_scalar_finite(wme, "`wme`")
    limit_inputs <- comfort__check_flag(limit_inputs, "`limit_inputs`")
    round_output <- comfort__check_flag(round_output, "`round_output`")
    comfort__model(
        "pmv",
        list(
            tr = tr,
            vr = vr,
            met = met,
            clo = clo,
            wme = wme,
            model = model,
            limit_inputs = limit_inputs,
            round_output = round_output
        )
    )
}

#' @rdname comfort_model_pmv
#' @export
comfort_model_set <- function(
    tr = NULL,
    v = 0.1,
    met = 1.2,
    clo = 0.5,
    wme = 0,
    limit_inputs = FALSE,
    body_surface_area = 1.8258,
    p_atm = NULL,
    position = c("standing", "sitting"),
    round_output = FALSE
) {
    position <- match.arg(position)
    tr <- comfort__check_scalar_finite(tr, "`tr`", allow_null = TRUE)
    v <- comfort__check_scalar_finite(v, "`v`")
    met <- comfort__check_scalar_finite(met, "`met`")
    clo <- comfort__check_scalar_finite(clo, "`clo`")
    wme <- comfort__check_scalar_finite(wme, "`wme`")
    body_surface_area <- comfort__check_scalar_finite(
        body_surface_area,
        "`body_surface_area`"
    )
    p_atm <- comfort__check_scalar_finite(p_atm, "`p_atm`", allow_null = TRUE)
    limit_inputs <- comfort__check_flag(limit_inputs, "`limit_inputs`")
    round_output <- comfort__check_flag(round_output, "`round_output`")
    comfort__model(
        "set",
        list(
            tr = tr,
            v = v,
            met = met,
            clo = clo,
            wme = wme,
            limit_inputs = limit_inputs,
            body_surface_area = body_surface_area,
            p_atm = p_atm,
            position = position,
            round_output = round_output
        )
    )
}

#' @rdname comfort_model_pmv
#' @param t_running Running mean outdoor temperature.
#' @param standard Adaptive comfort standard.
#' @param category Adaptive comfort category.
#' @export
comfort_model_adaptive <- function(
    t_running,
    tr = NULL,
    v = 0.1,
    standard = c("ashrae55", "en16798"),
    category = NULL,
    limit_inputs = TRUE,
    round_output = FALSE
) {
    standard <- match.arg(standard)
    t_running <- comfort__check_scalar_finite(t_running, "`t_running`")
    tr <- comfort__check_scalar_finite(tr, "`tr`", allow_null = TRUE)
    v <- comfort__check_scalar_finite(v, "`v`")
    limit_inputs <- comfort__check_flag(limit_inputs, "`limit_inputs`")
    round_output <- comfort__check_flag(round_output, "`round_output`")
    comfort__model(
        "adaptive",
        list(
            t_running = t_running,
            tr = tr,
            v = v,
            standard = standard,
            category = category,
            limit_inputs = limit_inputs,
            round_output = round_output
        )
    )
}

#' @rdname comfort_model_pmv
#' @export
comfort_model_heat_index <- function(
    solar_exposure = 0,
    limit_inputs = TRUE,
    round_output = FALSE
) {
    solar_exposure <- comfort__check_scalar_finite(
        solar_exposure,
        "`solar_exposure`"
    )
    if (solar_exposure < 0 || solar_exposure > 1) {
        stop(
            "`solar_exposure` must be a single finite value from 0 to 1.",
            call. = FALSE
        )
    }
    limit_inputs <- comfort__check_flag(limit_inputs, "`limit_inputs`")
    round_output <- comfort__check_flag(round_output, "`round_output`")
    comfort__model(
        "heat_index",
        list(
            solar_exposure = solar_exposure,
            limit_inputs = limit_inputs,
            round_output = round_output
        )
    )
}

#' PMV-based comfort standards
#'
#' These helpers describe static PMV-based comfort zones. They are distinct from
#' adaptive comfort models such as [comfort_model_adaptive()], which use running
#' mean outdoor temperature and produce operative-temperature bands.
#'
#' @param edition Standard edition. Currently `"2017"` for ASHRAE 55 and
#'   `"2007"` for EN 15251.
#' @param range PMV comfort interval for ASHRAE 55.
#' @param breaks PMV boundaries for EN 15251 comfort bands.
#'
#' @return A comfort standard object.
#'
#' @examples
#' # Create the ASHRAE 55 PMV comfort interval.
#' comfort_pmv_ashrae55()
#'
#' # Create the EN 15251 PMV comfort bands.
#' comfort_pmv_en15251()
#'
#' # Draw the ASHRAE 55 comfort zone.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(
#'         standard = comfort_pmv_ashrae55(),
#'         bands = FALSE,
#'         contours = FALSE,
#'         n = 80
#'     )
#'
#' # Draw the EN 15251 comfort bands.
#' ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
#'     geom_comfort_pmv(
#'         standard = comfort_pmv_en15251(),
#'         bands = FALSE,
#'         contours = FALSE,
#'         n = 80
#'     )
#'
#' @export
comfort_pmv_ashrae55 <- function(edition = "2017", range = c(-0.5, 0.5)) {
    # Keep editions explicit in the API while leaving room for later standards.
    edition <- as.character(edition)
    if (length(edition) != 1L || is.na(edition) || edition != "2017") {
        stop('`edition` must be "2017".', call. = FALSE)
    }
    range <- comfort__check_ordered_breaks(range, "`range`", n_min = 2L)
    if (length(range) != 2L) {
        stop("`range` must contain exactly two PMV boundaries.", call. = FALSE)
    }
    comfort__standard(
        paste0("ashrae55_", edition),
        breaks = range,
        fills = "#5BD96A",
        alphas = 0.58
    )
}

#' @rdname comfort_pmv_ashrae55
#' @export
comfort_pmv_en15251 <- function(
    edition = "2007",
    breaks = c(-0.7, -0.2, 0.2, 0.7)
) {
    # Keep editions explicit in the API while leaving room for later standards.
    edition <- as.character(edition)
    if (length(edition) != 1L || is.na(edition) || edition != "2007") {
        stop('`edition` must be "2007".', call. = FALSE)
    }
    breaks <- comfort__check_ordered_breaks(breaks, "`breaks`", n_min = 4L)
    if (length(breaks) != 4L) {
        stop("`breaks` must contain four PMV boundaries.", call. = FALSE)
    }
    comfort__standard(
        paste0("en15251_", edition),
        breaks = breaks,
        fills = c("#9BE89D", "#39D84A", "#9BE89D"),
        alphas = c(0.34, 0.58, 0.34)
    )
}
