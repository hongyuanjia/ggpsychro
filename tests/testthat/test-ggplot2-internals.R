# Dedicated ggplot2-internal checks are CI-only because they intentionally lock
# down private upstream behavior. CRAN and ordinary local runs skip them because
# they do not set the GitHub Actions CI environment.
testthat::skip_if_not(
    identical(Sys.getenv("CI"), "true") ||
        identical(Sys.getenv("GITHUB_ACTIONS"), "true"),
    "ggplot2 internal compatibility checks only run in CI."
)

test_that("ggplot2 build internals are available with expected signatures", {
    expect_silent(ggplot2__check_build_internals())

    specs <- ggplot2__build_internal_specs()
    internals <- ggplot2__build_internals(refresh = TRUE)

    expect_named(internals, names(specs))
    expect_true(all(vapply(internals, is.function, logical(1L))))
    expect_length(ggplot2__build_internal_problems(internals, specs), 0L)
})

test_that("ggplot2 build internal signature checks report actionable problems", {
    specs <- list(needed = c("x", "y"), absent = "z")
    internals <- list(needed = function(x) NULL, absent = NULL)
    problems <- ggplot2__build_internal_problems(internals, specs)
    problem_text <- paste(problems, collapse = "\n")

    expect_match(problem_text, "`needed\\(\\)` has incompatible arguments")
    expect_match(problem_text, "expected x, y")
    expect_match(problem_text, "found x")
    expect_match(problem_text, "y")
    expect_match(problem_text, "missing `absent\\(\\)`")
})

test_that("ggplot2 by-layer helper keeps paired layer and data semantics", {
    layers <- list(list(offset = 1), list(offset = 10))
    data <- list(data.frame(x = 1:2), data.frame(x = 3:4))

    out <- ggplot2__by_layer(
        function(l, d) {
            d$x <- d$x + l$offset
            d
        },
        layers,
        data,
        "testing layer/data pairing"
    )

    expect_equal(out[[1L]]$x, c(2, 3))
    expect_equal(out[[2L]]$x, c(13, 14))
})

test_that("ggplot2 data exposure helpers preserve ignored AsIs columns", {
    data <- data.frame(x = 1:2)
    data$raw <- I(c("a", "b"))

    ignored <- ggplot2__ignore_data(list(data))[[1L]]
    expect_named(ignored, c("x", ".ignored"))
    expect_named(ignored$.ignored, "raw")

    exposed <- ggplot2__expose_data(list(ignored))[[1L]]
    expect_named(exposed, c("x", "raw"))
    expect_s3_class(exposed$raw, "AsIs")
    expect_equal(as.character(exposed$raw), c("a", "b"))
})

test_that("ggplot2 label, theme, and built-object helpers keep expected shape", {
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
        ggplot2::geom_point() +
        ggplot2::labs(title = "Fuel economy")
    built <- ggplot2::ggplot_build(plot)

    labels <- ggplot2__setup_plot_labels(
        built$plot,
        built$plot$layers,
        built$data
    )
    expect_s3_class(labels, "ggplot2::labels")
    expect_equal(labels$x, "wt")
    expect_equal(labels$y, "mpg")
    expect_equal(labels$title, "Fuel economy")

    theme <- ggplot2__plot_theme(built$plot)
    expect_s3_class(theme, "theme")
    expect_true("panel.background" %in% names(theme))

    rebuilt <- ggplot2__class_ggplot_built(
        data = built$data,
        layout = built$layout,
        plot = built$plot
    )
    expect_s3_class(rebuilt, "ggplot2::ggplot_built")
    expect_identical(rebuilt$data, built$data)
    expect_identical(rebuilt$layout, built$layout)
    expect_identical(rebuilt$plot, built$plot)
})

test_that("ggplot2 view-scale helper returns trained scale ranges", {
    scale <- ggplot2::scale_x_continuous()
    scale$train(c(0, 10))

    view <- ggplot2__view_scales_from_scale(
        scale,
        coord_limits = c(2, 8),
        expand = FALSE
    )

    expect_named(view, c("x", "x.sec", "x.range"))
    expect_equal(view$x$continuous_range, c(2, 8))
    expect_equal(view$x.range, c(2, 8))
    expect_true(is.function(view$x$get_breaks))
})
