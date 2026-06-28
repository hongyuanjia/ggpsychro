# Create a ggpsychro plot

This function is the equivalent of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
in ggplot2. It takes care of setting up the plot object along with
creating the layout for the plot based on the graph and the
specification passed in. Alternatively a layout can be prepared in
advance using `create_layout` and passed as the data argument. See
*Details* for a description of all available layouts.

## Usage

``` r
ggpsychro(
  data = NULL,
  mapping = aes(),
  tdb_lim = NULL,
  hum_lim = NULL,
  altitude = 0L,
  units = "SI",
  mollier = FALSE
)
```

## Arguments

- data:

  Default dataset to use for plot. If not already a data.frame, will be
  converted to one by
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html).
  If not specified, must be supplied in each layer added to the plot.

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

- tdb_lim:

  A numeric vector of length-2 indicating the dry-bulb temperature
  limits. Should be in range `[-50, 100]` degree_C \[SI\] or
  `[-58, 212]` degree_F \[IP\]. If `NULL`, trained data ranges will be
  used when available, otherwise a default display range will be used.
  Default: `NULL`.

- hum_lim:

  A numeric vector of length-2 indicating the humidity ratio limits.
  Should be in range `[0, 60]` g_H20 kg_Air-1 \[SI\] or `[0, 350]`
  gr_H20 lb_Air-1 \[IP\]. If `NULL`, trained data ranges will be used
  when available, otherwise a default display range will be used.
  Default: `NULL`.

- altitude:

  A single number of altitude in m \[SI\] or ft \[IP\]. Default: `0`.

- units:

  A string indicating the system of units chosen. Should be either
  `"SI"` or `"IP"`.

- mollier:

  If `TRUE`, a Mollier chart will be created instead of a psychrometric
  chart. Default: `FALSE`.

## Value

An object of class `gg` onto which layers, scales, etc. can be added.

## Author

Hongyuan Jia

## Examples

``` r
ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50))


ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), mollier = TRUE)


ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), units = "IP", altitude = 100)

```
