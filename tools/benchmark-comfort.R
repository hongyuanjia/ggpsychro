# Run with:
# Rscript tools/benchmark-comfort.R

# Load the source checkout when pkgload is available; otherwise benchmark the
# installed package so the script is still usable outside the repository.
bench__load_package <- function() {
    if (
        requireNamespace("pkgload", quietly = TRUE) &&
            file.exists("DESCRIPTION")
    ) {
        pkgload::load_all(quiet = TRUE, export_all = FALSE)
    } else {
        library(ggpsychro)
    }
    invisible(NULL)
}

# Time plot build and grob construction under a temporary graphics device so
# grid text metrics do not create Rplots.pdf in the working directory.
bench__time_plot <- function(plot, operation, iterations = 3L) {
    operation <- match.arg(operation, c("build", "grob"))
    times <- numeric(iterations)
    path <- tempfile(fileext = ".pdf")
    grDevices::pdf(path, width = 8, height = 5)
    on.exit(
        {
            grDevices::dev.off()
            unlink(path)
        },
        add = TRUE
    )

    for (i in seq_len(iterations)) {
        gc()
        times[[i]] <- system.time({
            if (operation == "build") {
                ggplot2::ggplot_build(plot)
            } else {
                ggplot2::ggplotGrob(plot)
            }
        })[["elapsed"]]
    }

    stats::median(times)
}

# Run the benchmark and clean only the Rplots.pdf created by this script.
bench__main <- function() {
    had_rplots <- file.exists("Rplots.pdf")
    on.exit(
        {
            if (!had_rplots && file.exists("Rplots.pdf")) {
                unlink("Rplots.pdf")
            }
        },
        add = TRUE
    )

    bench__load_package()

    cases <- list(
        pmv_default = ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
            geom_comfort_pmv(),
        pmv_explore = ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
            geom_comfort_pmv(n = c(45, 30)),
        set_default = ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
            geom_comfort_set(),
        heat_index_default = ggpsychro(
            tdb_lim = c(25, 45),
            hum_lim = c(0, 32)
        ) +
            geom_comfort_heat_index()
    )

    out <- do.call(
        rbind,
        lapply(names(cases), function(name) {
            plot <- cases[[name]]
            data.frame(
                case = name,
                build_s = bench__time_plot(plot, "build"),
                grob_s = bench__time_plot(plot, "grob"),
                row.names = NULL
            )
        })
    )

    print(out)
    invisible(out)
}

bench__main()
