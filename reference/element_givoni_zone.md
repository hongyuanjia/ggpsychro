# Comfort zone style element

`element_givoni_zone()` creates a small style object for comfort
strategy zones. It is used by
[`geom_comfort_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_givoni.md)
through the `zone_style` argument to override the default Givoni-Milne
zone styles.

## Usage

``` r
element_givoni_zone(
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

## Examples

``` r
# Fill and outline the comfort zone with custom colours.
ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
    geom_comfort_givoni(
        labels = FALSE,
        zone_style = list(
            comfort = element_givoni_zone(
                fill = "#6FCF97",
                colour = "#1B7F4A",
                alpha = 0.35
            )
        )
    )


# Emphasize the air-conditioning region with a light fill.
ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
    geom_comfort_givoni(
        labels = FALSE,
        zone_style = list(
            air_conditioning = element_givoni_zone(
                fill = "#7BC8F6",
                colour = "#1B5E8C",
                alpha = 0.18,
                linetype = "solid"
            )
        )
    )


# Restyle a line-only region without filling it.
ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
    geom_comfort_givoni(
        labels = FALSE,
        zone_style = list(
            winter = element_givoni_zone(
                colour = "#C44536",
                linewidth = 1.2,
                linetype = "dashed"
            )
        )
    )

```
