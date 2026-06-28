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
