# Comfort overlays

Comfort overlay layers evaluate thermal comfort models on the current
psychrometric panel. They inherit the chart units, pressure, limits, and
Mollier orientation from
[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md),
so comfort regions can be composed with the same `+` workflow as other
ggplot layers.

## PMV overlays

The default comfort model is ISO 7730 PMV.
[`geom_comfort_overlay()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
draws filled PMV bands,
[`scale_fill_comfort_pmv()`](https://hongyuanjia.github.io/ggpsychro/reference/scale_fill_comfort_pmv.md)
applies a PMV-centered diverging palette, and
[`geom_comfort_pmv_lines()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
adds labelled PMV curves.

``` r

ggpsychro(tdb_lim = c(5, 40), hum_lim = c(0, 24)) +
    psychro_preset("minimal") +
    geom_comfort_overlay(n = c(70, 48), gap = 0) +
    scale_fill_comfort_pmv(name = "PMV") +
    geom_comfort_pmv_lines(levels = seq(-3, 3, by = 0.5), n = 140)
```

![Psychrometric chart with filled PMV bands and labelled PMV curve
lines.](comfort-overlays_files/figure-html/pmv-overlay-1.png)

## Standard comfort zones

For PMV-based standards,
[`geom_comfort_standard_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
draws the filled zone, PMV boundary curves, and labels from a standard
helper.

``` r

ggpsychro(tdb_lim = c(5, 35), hum_lim = c(0, 24)) +
    psychro_preset("minimal") +
    geom_comfort_standard_zone(comfort_standard_ashrae55_2017(), n = 140)
```

![Psychrometric chart with the PMV-based ASHRAE 55 2017 comfort
zone.](comfort-overlays_files/figure-html/ashrae55-zone-1.png)

``` r

ggpsychro(tdb_lim = c(5, 35), hum_lim = c(0, 24)) +
    psychro_preset("minimal") +
    geom_comfort_standard_zone(comfort_standard_en15251_2007(), n = 140)
```

![Psychrometric chart with PMV-based EN 15251 2007 comfort
bands.](comfort-overlays_files/figure-html/en15251-zone-1.png)

## SET and adaptive models

Create reusable model objects with `comfort_model_*()` helpers. SET
overlays default to the `"set"` metric, while adaptive models default to
`"acceptability"`.

``` r

set_model <- comfort_model_set()

ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
    psychro_preset("minimal") +
    geom_comfort_overlay(model = set_model, n = c(40, 24)) +
    geom_comfort_contour(
        model = set_model,
        metric = "set",
        breaks = c(22, 24, 26),
        n = c(40, 24),
        colour = "#4A4A4A"
    )
```

![Psychrometric chart with a SET comfort overlay and labelled SET
contour lines.](comfort-overlays_files/figure-html/set-overlay-1.png)

``` r

adaptive_model <- comfort_model_adaptive(t_running = 20)

ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20)) +
    psychro_preset("minimal") +
    geom_comfort_overlay(model = adaptive_model, n = c(40, 24)) +
    geom_comfort_zone(
        model = adaptive_model,
        fill = NA,
        colour = "#4A4A4A"
    )
```

![Psychrometric chart with an adaptive comfort acceptability overlay and
boundary
zone.](comfort-overlays_files/figure-html/adaptive-overlay-1.png)

## State metrics

[`stat_comfort_state()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
evaluates the model at supplied psychrometric states. The computed
comfort fields can be used with `after_stat()`.

``` r

states <- data.frame(
    tdb = c(22, 24, 26, 28),
    relhum = c(45, 50, 55, 60)
)

ggpsychro(states, tdb_lim = c(15, 32), hum_lim = c(0, 22)) +
    psychro_preset("minimal") +
    stat_comfort_state(
        aes(tdb = tdb, relhum = relhum, colour = after_stat(pmv)),
        size = 3
    ) +
    scale_colour_gradient2(
        "PMV",
        low = "#3B5FFF",
        mid = "#4A4A4A",
        high = "#FF3B30",
        midpoint = 0
    )
```

![Psychrometric chart with measured state points coloured by computed
PMV.](comfort-overlays_files/figure-html/state-metrics-1.png)

## Mollier and IP charts

Comfort layers follow the parent chart’s coordinate system. Use
`mollier = TRUE` for Mollier orientation or `units = "IP"` for IP
inputs.

``` r

ggpsychro(tdb_lim = c(15, 30), hum_lim = c(0, 20), mollier = TRUE) +
    psychro_preset("minimal") +
    geom_comfort_overlay(n = c(50, 30)) +
    scale_fill_comfort_pmv(name = "PMV") +
    geom_comfort_pmv_lines(levels = seq(-2, 2, by = 1), n = 100)
```

![Mollier psychrometric chart with a PMV comfort overlay and labelled
PMV
lines.](comfort-overlays_files/figure-html/mollier-comfort-overlay-1.png)

Use manual psychrometric zones when the desired region is a
project-specific operating envelope rather than a thermal comfort model.
See [Zones and
processes](https://hongyuanjia.github.io/ggpsychro/articles/zones-and-processes.md)
for those layers.
