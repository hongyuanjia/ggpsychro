#' @include comfort-core.R
NULL

# Adaptive comfort helpers keep the standard-specific equations separate from
# the plotting/stat layer code that consumes them.
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
