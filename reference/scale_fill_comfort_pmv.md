# Comfort PMV fill scale

A diverging blue-white-red fill scale centered on PMV 0.

## Usage

``` r
scale_fill_comfort_pmv(
  ...,
  limits = c(-3, 3),
  low = "#3B5FFF",
  mid = "#F7F7F7",
  high = "#FF3B30",
  midpoint = 0,
  oob = scales::squish
)
```

## Arguments

- ...:

  Passed to
  [`ggplot2::scale_fill_gradient2()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

- limits:

  Scale limits.

- low, mid, high:

  Endpoint and midpoint colours.

- midpoint:

  Scale midpoint.

- oob:

  Out-of-bounds handler.

## Value

A ggplot2 fill scale.

## Examples

``` r
# Use the default PMV colour scale.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_overlay(n = c(45, 30)) +
    scale_fill_comfort_pmv(name = "PMV")


# Focus the legend on the usual comfort range.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_overlay(n = c(45, 30)) +
    scale_fill_comfort_pmv(limits = c(-1.5, 1.5), name = "PMV")


# Use a custom diverging palette.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_overlay(n = c(45, 30)) +
    scale_fill_comfort_pmv(
        low = "#2166AC",
        mid = "white",
        high = "#B2182B",
        name = "PMV"
    )

```
