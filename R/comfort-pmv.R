#' @include comfort-core.R
NULL

# PMV/SET scalar formulas, native bridges, and root tracing. The R fallback
# paths are intentionally kept beside the native wrappers for parity tests.
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

    # ISO 7730 PMV is evaluated in SI heat-balance units; vapor pressure,
    # clothing insulation, and metabolic rate are converted before iteration.
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

    # Clothing surface temperature is implicit because radiation and convection
    # both depend on it, so solve the fixed point before evaluating heat losses.
    i <- 0L
    while (abs(xn - xf) > 0.00015 && i < 150L) {
        xf <- (xf + xn) / 2
        hcn <- 2.38 * abs(100 * xf - taa)^0.25
        hc <- max(hcf, hcn)
        xn <- (p5 + p4 * hc - p2 * xf^4) / (100 + p3 * hc)
        i <- i + 1L
    }

    tcl <- 100 * xn - 273
    # Fanger heat-loss terms: skin diffusion, sweating, latent/dry respiration,
    # radiation, and convection. Their residual is scaled into the PMV vote.
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

    # SET uses the Gagge two-node model: first simulate skin/core state in the
    # actual environment, then solve the standard environment with equal strain.
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

    # Transient two-node simulation. Each minute updates clothing temperature,
    # sensible exchange, stored heat, blood flow, sweating, and shivering.
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
        # Thermoregulation signals drive vasodilation/constriction, regulatory
        # sweating, and shivering before the next minute is integrated.
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
    # Newton iteration for the standard effective temperature whose sensible
    # and evaporative skin losses match the simulated actual environment.
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

comfort_pmv_curve_data <- function(model, levels, n, units, pres, mollier,
                                   tdb_lim, hum_lim,
                                   label = c("none", "sensation", "boundary",
                                       "comfort"),
                                   label_hjust = NULL, label_vjust = NULL,
                                   reverse = FALSE) {
    label <- match.arg(label)
    out <- comfort_pmv_curve_base_data(
        model, levels, n, units, pres, tdb_lim, hum_lim
    )
    if (!nrow(out)) {
        return(comfort_empty_pmv_curve())
    }
    out$label <- vapply(out$level, comfort_pmv_curve_label, character(1L),
        label = label)
    out$hjust <- comfort_pmv_curve_hjust(label, label_hjust)
    out$vjust <- vapply(out$level, comfort_pmv_curve_vjust, numeric(1L),
        label = label, override = label_vjust, mollier = mollier)

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

comfort_pmv_curve_base_data <- function(model, levels, n, units, pres,
                                        tdb_lim, hum_lim) {
    levels <- comfort_check_breaks(levels, "`levels`", n_min = 1L)
    n <- comfort_pmv_curve_n(n)
    model <- comfort_pmv_curve_model(model)
    lim <- comfort_grid_limits(units, tdb_lim, hum_lim)
    # Constant-PMV curves are traced by solving dry-bulb roots on humidity-ratio
    # samples, then adding saturation-boundary roots so curves close cleanly.
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
            metric = "pmv"
        ))
    }
    curves <- curves[!vapply(curves, is.null, logical(1L))]
    if (!length(curves)) {
        return(comfort_empty_pmv_curve_base())
    }
    out <- do.call(rbind, curves)
    row.names(out) <- NULL
    out
}

comfort_empty_pmv_curve_base <- function() {
    new_data_frame(list(
        tdb = numeric(), humratio = numeric(),
        level = numeric(), value = numeric(), group = integer(),
        linetype = character(), metric = character()
    ))
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
    curves <- comfort_pmv_curve_base_data(
        model, levels, n, units, pres, tdb_lim, hum_lim
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
    # Saturation roots are shared by the humidity sampling grid and by each band
    # edge, avoiding duplicate solves on the curved upper boundary.
    saturation_roots <- comfort_pmv_rootband_saturation_roots(
        model, breaks, lim$tdb, lim$hum, units, pres, n[[1L]]
    )
    humratio <- comfort_pmv_rootband_humratio(
        model, breaks, n[[1L]], units, pres, lim$tdb, lim$hum,
        saturation_roots = saturation_roots
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

    # Build one root vector per PMV break on the shared humidity grid. When a
    # break intersects saturation exactly, overwrite the row with that root.
    roots <- lapply(breaks, function(level) {
        roots <- comfort_pmv_curve_root_vector(
            model, level, humratio, lim$tdb, units, pres
        )
        sat_roots <- saturation_roots[[as.character(level)]]
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

    # Bands are assembled row-wise between adjacent PMV roots, then contiguous
    # rows are stitched into polygons for ggplot.
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

comfort_pmv_rootband_saturation_roots <- function(model, breaks, tdb_lim,
                                                  hum_lim, units, pres, n) {
    roots <- lapply(breaks, function(level) {
        comfort_pmv_curve_saturation_roots(
            model, level, tdb_lim, hum_lim, units, pres, n
        )
    })
    names(roots) <- as.character(breaks)
    roots
}

comfort_pmv_rootband_humratio <- function(model, breaks, n, units, pres,
                                          tdb_lim, hum_lim,
                                          saturation_roots = NULL) {
    hum <- seq(
        narrow_hum(hum_lim[[1L]], units),
        narrow_hum(hum_lim[[2L]], units),
        length.out = n
    )
    sat <- psychro_saturation_humratio(tdb_lim, units, pres)
    hum <- c(hum, sat[is.finite(sat)])
    if (is.null(saturation_roots)) {
        saturation_roots <- comfort_pmv_rootband_saturation_roots(
            model, breaks, tdb_lim, hum_lim, units, pres, n
        )
    }
    for (level in breaks) {
        roots <- saturation_roots[[as.character(level)]]
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
    # Root tracing at saturation can differ from endpoint PMV by a few ulps;
    # keep those edge rows so filled bands do not show visible holes.
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
    # The C root tracer assumes scalar fixed parameters and no input clipping;
    # vectorized or limited models fall back to the R implementation.
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
    # At a fixed humidity ratio, valid dry-bulb temperatures start at the
    # dew-point line and end at the chart limit; roots are bracketed there.
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
    # A fixed 44-step bisection is enough to reach double-precision chart
    # accuracy without depending on optimizers that allocate per row.
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
    # Trace roots along the saturation curve so PMV contours and filled bands
    # can close against the psychrometric chart boundary instead of stopping.
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
