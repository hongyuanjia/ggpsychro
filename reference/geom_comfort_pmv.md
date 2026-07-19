# Draw PMV comfort layers

`geom_comfort_pmv()` draws filled PMV bands, PMV contour lines and
labels, plus optional PMV-based standard zones.

## Usage

``` r
geom_comfort_pmv(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  model = comfort_model_pmv(),
  standard = NULL,
  bands = TRUE,
  contours = TRUE,
  labels = TRUE,
  contour_levels = seq(-3, 3, by = 0.5),
  band_levels = NULL,
  n = NULL,
  band_render = c("band", "tile"),
  band_method = c("auto", "root", "isoband"),
  alpha = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    `position_jitter()`. This method allows for passing extra arguments
    to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use `position_jitter()`, give the position as
    `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- model:

  A comfort model object.

- standard:

  PMV-based standard object.

- bands, contours, labels:

  Single logical values controlling whether `geom_comfort_pmv()` draws
  filled PMV bands, PMV contour lines, and text labels. PMV standard
  zones still draw when `standard` is supplied.

- contour_levels:

  PMV contour levels for `geom_comfort_pmv()`.

- band_levels:

  Number of PMV filled bands, or a numeric vector of PMV band breaks for
  `geom_comfort_pmv()`.

- n:

  Grid resolution in dry-bulb and humidity-ratio directions. If `NULL`,
  PMV bands use `c(360, 220)` and PMV curves use `360`.

- band_render:

  Band rendering mode. `"band"` draws filled polygon regions from
  continuous band boundaries; `"tile"` draws sampled grid cells
  directly.

- band_method:

  Boundary construction method for `band_render = "band"`. `"auto"` uses
  root-traced boundaries for PMV and isobands for other metrics;
  `"root"` forces PMV root-traced boundaries; `"isoband"` uses gridded
  isobands.

- alpha:

  Layer transparency. PMV standards keep their own defaults unless
  `alpha` is supplied.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A list of ggplot additions.

## Details

`n` trades drawing smoothness for build time. For PMV bands, supply one
value to use the same dry-bulb and humidity-ratio resolution, or two
values for separate directions. PMV contour curves and standard-zone
boundaries use the first value because they trace roots along one
sampling direction. Smaller values such as `n = c(45, 30)` are useful
for exploratory work; larger values produce smoother publication
graphics.

`band_render = "band"` draws filled polygon bands from continuous
boundaries. `band_render = "tile"` draws sampled grid cells directly.
With `band_render = "band"`, `band_method = "auto"` uses PMV root
tracing for smoother PMV boundaries; `band_method = "isoband"` uses
gridded isobands when a cheaper sampled approximation is preferred.

## Examples

``` r
# Draw PMV comfort bands, contours, and labels.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_pmv(n = c(45, 30)) +
    scale_fill_comfort_pmv(name = "PMV")


# Draw labelled PMV contour lines.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_pmv(
        bands = FALSE,
        contours = TRUE,
        labels = TRUE,
        contour_levels = c(-1, 0, 1),
        n = 80
    )


# Draw sampled PMV values as grid tiles.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_pmv(band_render = "tile", contours = FALSE, n = c(45, 30))


# Draw a PMV-based comfort standard zone.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_pmv(
        standard = comfort_pmv_ashrae55(),
        bands = FALSE,
        contours = FALSE,
        n = 80
    )

```
