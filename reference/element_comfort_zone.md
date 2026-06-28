# Comfort zone style element

`element_comfort_zone()` creates a small style object for comfort
strategy zones. It is used by
[`geom_comfort_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
through the `zone_style` argument to override Marsh-style defaults for
individual zones.

## Usage

``` r
element_comfort_zone(
  fill = ggplot2::waiver(),
  colour = ggplot2::waiver(),
  linewidth = ggplot2::waiver(),
  linetype = ggplot2::waiver(),
  alpha = ggplot2::waiver(),
  linejoin = ggplot2::waiver(),
  color = NULL
)
```

## Arguments

- fill, colour, color, linewidth, linetype, alpha, linejoin:

  Zone drawing properties. Values left as
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  inherit the layer default.

## Value

A comfort zone style element.
