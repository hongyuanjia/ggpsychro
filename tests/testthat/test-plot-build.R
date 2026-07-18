# Focused psychrometric chart, coordinate, guide, and stat tests.

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
