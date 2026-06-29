#' @include psychro-state.R stat-psychro-bin.R
NULL

# Shared data-shaping and validation helpers for comfort layers. Model-specific
# equations live in the PMV, adaptive, heat-index, and Givoni modules.
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

comfort_check_ordered_breaks <- function(x, name, n_min = 2L) {
    x <- as.numeric(x)
    if (length(x) < n_min || any(!is.finite(x)) || any(diff(x) <= 0)) {
        stop(name, " must contain finite strictly increasing PMV boundaries.",
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

comfort_p_sat_torr <- function(tdb) {
    exp(18.6686 - 4030.183 / (tdb + 235))
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

comfort_stat_context <- function(data, units, pres) {
    list(
        units = comfort_stat_units(data, units),
        pres = comfort_stat_pressure(data, pres)
    )
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
    # Grid consumers need different sampling locations: nodes for isoband
    # topology, centers for tile values and label placement.
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
        # Evaluate just inside saturation so psychrolib RH conversion remains
        # finite while contours still trace the visible saturation boundary.
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
        # Cell centers can lie above saturation even when part of the tile is
        # visible; resample near the valid saturated edge instead of dropping it.
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
comfort_band_data <- function(model, metric, levels, n, units, pres, mollier,
                              tdb_lim, hum_lim) {
    # Filled bands are generated on node grids so isoband can preserve polygon
    # topology across adjacent cells.
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
    # For partially clipped tiles, choose a representative point inside the
    # valid psychrometric domain so the color reflects the visible fragment.
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
        # PMV curves are root-traced because grid isolines can miss steep
        # segments near saturation; other metrics use the cheaper grid path.
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
        # Root-traced PMV contours already return curve vertices; the common
        # label code below can treat them like isoband isolines.
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
    # Normalize isoband's path representation to the columns expected by
    # ggplot stats and psychrometric coordinate transforms.
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

    # Layer stats operate in chart coordinates, so dispatch here converts fixed
    # model parameters to the calculator API while preserving unrounded outputs.
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
