# Psychrometric coordinates

Psychrometric coordinates

## Usage

``` r
coord_psychro(
  tdb_lim = NULL,
  hum_lim = NULL,
  altitude = NULL,
  units = NULL,
  mollier = NULL,
  expand = FALSE,
  default = TRUE,
  clip = "on"
)
```

## Arguments

- tdb_lim:

  A numeric vector of length-2 indicating the dry-bulb temperature
  limits. Should be in range `[-50, 100]` degree_C \[SI\] or
  `[-58, 212]` degree_F \[IP\]. If `NULL`, trained data ranges will be
  used when available, otherwise a default display range will be used.
  Default: `NULL`.

- hum_lim:

  A numeric vector of length-2 indicating the humidity ratio limits.
  Should be in range `[0, 60]` g_H20 kg_Air-1 \[SI\] or `[0, 420]`
  gr_H20 lb_Air-1 \[IP\]. If `NULL`, trained data ranges will be used
  when available, otherwise a default display range will be used.
  Default: `NULL`.

- altitude:

  A single number of altitude in m \[SI\] or ft \[IP\]. If `NULL`,
  inherits the altitude from the parent
  [`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
  plot.

- units:

  Unit system, either `"SI"` or `"IP"`. If `NULL`, inherits the unit
  system from the parent
  [`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
  plot.

- mollier:

  If `TRUE`, use Mollier chart coordinates. If `NULL`, inherits the
  chart type from the parent
  [`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
  plot.

- expand:

  If `TRUE`, add a small expansion factor to the limits. Defaults to
  `FALSE` for psychrometric charts.

- default:

  Is this the default coordinate system? Defaults to `TRUE` so replacing
  the coordinate system created by
  [`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
  does not emit a ggplot2 replacement message.

- clip:

  Should drawing be clipped to the extent of the plot panel? A setting
  of `"on"` (the default) means yes, and a setting of `"off"` means no.
  In most cases, the default of `"on"` should not be changed, as setting
  `clip = "off"` can cause unexpected results. It allows drawing of data
  points anywhere on the plot, including in the plot margins. If limits
  are set via `xlim` and `ylim` and some data points fall outside those
  limits, then those data points may show up in places such as the axes,
  the legend, the plot title, or the plot margins.

## Value

A ggplot2 coordinate system object for psychrometric charts.

## Details

`coord_psychro()` is normally used with a
[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
plot. When `altitude`, `units`, or `mollier` is `NULL`, the value is
inherited from the parent plot. Supply these arguments explicitly when
using the coordinate system outside that path.

## Examples

``` r
ggpsychro() +
    coord_psychro(tdb_lim = c(10, 35), hum_lim = c(0, 25))


ggpsychro(units = "IP", altitude = 1000) +
    coord_psychro(
        tdb_lim = c(50, 100),
        hum_lim = c(0, 140),
        units = "IP",
        altitude = 1000
    )


ggpsychro(mollier = TRUE) +
    coord_psychro(
        tdb_lim = c(0, 50),
        hum_lim = c(0, 30),
        mollier = TRUE
    )
```
