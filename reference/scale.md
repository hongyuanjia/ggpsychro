# Transformation object for psychrometric chart

Transformation object for psychrometric chart

## Usage

``` r
scale_drybulb_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)

scale_humratio_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)

scale_relhum_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)

scale_wetbulb_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)

scale_vappres_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)

scale_specvol_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)

scale_enthalpy_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  trans = waiver(),
  transform = waiver(),
  guide = waiver(),
  ...
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  `waiver()`, the default, the name of the scale is taken from the first
  mapping used for that aesthetic. If `NULL`, the legend title will be
  omitted.

- breaks:

  One of:

  - `NULL` for no breaks

  - `waiver()` for the default breaks computed by the [transformation
    object](https://scales.r-lib.org/reference/new_transform.html)

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output (e.g., a function returned by
    [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
    Note that for position scales, limits are provided after scale
    expansion. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- minor_breaks:

  One of:

  - `NULL` for no minor breaks

  - `waiver()` for the default breaks (none for discrete, one minor
    break between each major break for continuous)

  - A numeric vector of positions

  - A function that given the limits returns a vector of minor breaks.
    Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. When the function has two arguments, it will be
    given the limits and major break positions.

- n.breaks:

  An integer guiding the number of major breaks. The algorithm may
  choose a slightly different number to ensure nice break labels. Will
  only have an effect if `breaks = waiver()`. Use `NULL` to use the
  default number of breaks given by the transformation.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - `waiver()` for the default labels computed by the transformation
    object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- limits:

  One of:

  - `NULL` to use the default scale range

  - A numeric vector of length two providing limits of the scale. Use
    `NA` to refer to the existing minimum or maximum

  - A function that accepts the existing (automatic) limits and returns
    new limits. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. Note that setting limits on positional scales
    will **remove** data outside of the limits. If the purpose is to
    zoom, use the limit argument in the coordinate system (see
    [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- trans, transform:

  A transformation object or transformer name passed to the underlying
  ggplot2 continuous scale. The default
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  keeps the psychrometric scale in chart display units.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- ...:

  Other arguments passed on to `scale_(x|y)_continuous()`

## Value

A ggplot2 continuous scale object.

## Examples

``` r
# Configure dry-bulb and humidity-ratio axes.
ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 25)) +
    scale_drybulb_continuous(breaks = seq(0, 35, by = 5)) +
    scale_humratio_continuous(breaks = seq(0, 25, by = 5))


# Configure relative-humidity and wet-bulb grid breaks.
ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 25)) +
    scale_relhum_continuous(n.breaks = 8) +
    scale_wetbulb_continuous(
        breaks = seq(10, 30, by = 5),
        minor_breaks = NULL
    )


# Configure vapor-pressure, specific-volume, and enthalpy grids.
ggpsychro(tdb_lim = c(0, 35), hum_lim = c(0, 25)) +
    scale_vappres_continuous(
        breaks = seq(1000, 4000, by = 1000),
        limits = c(1000, 4500)
    ) +
    scale_specvol_continuous(
        breaks = seq(0.80, 0.95, by = 0.05),
        limits = c(0.80, 0.95)
    ) +
    scale_enthalpy_continuous(
        breaks = seq(20000, 80000, by = 20000),
        limits = c(20000, 80000)
    )

```
