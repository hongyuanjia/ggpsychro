# Focused psychrometric chart, coordinate, guide, and stat tests.

test_that("Comfort path labels use the internal textpath renderer", {
    base <- ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 35))

    expect_gt(
        count_textpath_shapes(
            base + geom_comfort_pmv(bands = FALSE, n = 80)
        ),
        0L
    )
    expect_gt(
        count_textpath_shapes(
            base +
                geom_comfort_pmv(
                    standard = comfort_pmv_ashrae55(),
                    bands = FALSE,
                    contours = FALSE,
                    n = 80
                )
        ),
        0L
    )
    expect_gt(
        count_textpath_shapes(
            base + comfort_layer__contour(label = TRUE, n = c(30, 20))
        ),
        0L
    )
})
test_that("Native textpath helpers handle edge-case label placement", {
    path <- tempfile(fileext = ".pdf")
    grDevices::pdf(path)
    on.exit({
        grDevices::dev.off()
        unlink(path)
    })

    gp <- grid::gpar(fontsize = 10)
    expr <- textpath__measure(expression(alpha + beta), gp, vjust = 0.5)
    multiline <- textpath__measure_chr("a\nb", gp, vjust = 0.5)

    expect_equal(expr$n_label, 1L)
    expect_equal(length(expr$piece_label), 1L)
    expect_equal(length(multiline$piece_label), 1L)

    line_path <- util__new_data_frame(list(
        x = c(0, 1),
        y = c(0, 0),
        id = c(1L, 1L)
    ))
    measured <- list(
        piece_label = c("A", "B"),
        piece_id = c(1L, 1L),
        # Piece midpoints are in label arclength units; the label is longer
        # than the one-inch path so remove_long branches are exercised directly.
        piece_mid = c(0.5, 1.5),
        piece_width = c(1, 1),
        label_width = 2,
        label_offset = 0,
        n_label = 1L
    )

    kept <- textpath__place(
        line_path,
        measured,
        hjust = 0.5,
        upright = TRUE,
        remove_long = FALSE
    )
    dropped <- textpath__place(
        line_path,
        measured,
        hjust = 0.5,
        upright = TRUE,
        remove_long = TRUE
    )

    expect_equal(nrow(kept), 2L)
    expect_true(all(is.finite(kept$x)))
    expect_equal(nrow(dropped), 0L)
})
test_that("Native textpath grobs expand through grid makeContent", {
    path <- tempfile(fileext = ".pdf")
    grDevices::pdf(path)
    on.exit({
        grDevices::dev.off()
        unlink(path)
    })

    grid::grid.newpage()
    grob <- textpath__grob(
        label = "ABC",
        x = c(0.1, 0.9),
        y = c(0.5, 0.5),
        id = c(1L, 1L),
        hjust = 0.5,
        vjust = 0.5,
        upright = TRUE,
        remove_long = FALSE,
        gp_text = grid::gpar(fontsize = 10),
        gp_path = grid::gpar(col = "black"),
        text_only = FALSE,
        gap = TRUE,
        padding = grid::unit(0, "pt"),
        name = "test-textpath"
    )
    forced <- grid::grid.force(grob)
    children <- as.list(forced$children)

    expect_true(any(vapply(children, inherits, logical(1L), "polyline")))
    expect_true(any(vapply(children, inherits, logical(1L), "text")))

    dropped <- textpath__grob(
        label = "A very very long label",
        x = c(0.1, 0.2),
        y = c(0.5, 0.5),
        id = c(1L, 1L),
        hjust = 0.5,
        vjust = 0.5,
        upright = TRUE,
        remove_long = TRUE,
        gp_text = grid::gpar(fontsize = 10),
        gp_path = grid::gpar(col = "black"),
        text_only = FALSE,
        name = "drop-textpath"
    )

    expect_true(inherits(grid::grid.force(dropped), "null"))
})
test_that("Native textpath gap removal keeps only visible path intervals", {
    path <- util__new_data_frame(list(
        x = c(0, 1, 2),
        y = c(0, 0, 0),
        id = c(1L, 1L, 1L)
    ))
    placed <- util__new_data_frame(list(
        label = 1L,
        left = 0.8,
        right = 1.2
    ))

    gap_path <- textpath__gap_path(path, placed, padding = 0)

    expect_equal(range(gap_path$x), c(0, 2))
    expect_false(any(gap_path$x > 0.8 & gap_path$x < 1.2))
    expect_equal(length(unique(gap_path$id)), 2L)
})
