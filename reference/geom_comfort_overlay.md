# Comfort overlays for psychrometric charts

`geom_comfort_overlay()` samples a psychrometric panel on a dry-bulb by
humidity-ratio grid and draws the selected comfort metric as filled
contour bands by default. The default model is ISO 7730 PMV.
`geom_comfort_contour()` draws PMV contours with root-traced curves by
default, and uses the same grid as the overlay for other metrics.
`geom_comfort_zone()` draws a comfort region, and `stat_comfort_state()`
evaluates comfort fields at supplied states.

## Usage

``` r
geom_comfort_overlay(
  mapping = NULL,
  data = NULL,
  stat = NULL,
  position = "identity",
  ...,
  model = comfort_model_pmv(),
  metric = NULL,
  n = NULL,
  method = c("auto", "rootband", "isoband", "tile"),
  levels = NULL,
  gap = 0,
  alpha = 0.55,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_comfort_heat_index(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  model = comfort_model_heat_index(),
  n = c(160, 100),
  alpha = 0.55,
  show_labels = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_comfort_contour(
  mapping = NULL,
  data = NULL,
  stat = StatComfortContour,
  position = "identity",
  ...,
  model = comfort_model_pmv(),
  metric = NULL,
  breaks = NULL,
  n = NULL,
  contour_method = c("auto", "root", "isoband"),
  label = FALSE,
  label_size = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_comfort_zone(
  mapping = NULL,
  data = NULL,
  stat = StatComfortZone,
  position = "identity",
  ...,
  model = comfort_model_pmv(),
  metric = NULL,
  range = NULL,
  n = NULL,
  gap = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_comfort_pmv_lines(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  model = comfort_model_pmv(),
  levels = seq(-3, 3, by = 0.5),
  n = 360,
  label_sensation = TRUE,
  label_axis = TRUE,
  axis_label_hjust = ggplot2::waiver(),
  axis_label_vjust = ggplot2::waiver(),
  sensation_label_hjust = 0.5,
  sensation_label_vjust = 0.5,
  axis_label_size = NULL,
  sensation_label_size = NULL,
  padding = grid::unit(1, "pt"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_comfort_standard_zone(
  standard = comfort_standard_ashrae55_2017(),
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  model = comfort_model_pmv(),
  n = 360,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_comfort_givoni(
  strategy = comfort_strategy_givoni(),
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  alpha = 0.55,
  show_labels = TRUE,
  show_pmv = FALSE,
  pmv_model = comfort_model_pmv(),
  zone_alpha = 0.2,
  zone_style = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_comfort_state(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  model = comfort_model_pmv(),
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

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    `stat_count()`, give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

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

- metric:

  Comfort metric to draw. Defaults to `"pmv"` for PMV, `"set"` for SET,
  and `"acceptability"` for adaptive comfort.

- n:

  Grid resolution in dry-bulb and humidity-ratio directions. If `NULL`,
  a model-specific default is used.

- method:

  Overlay drawing method. `"auto"` uses root-traced filled bands for PMV
  and isobands for other metrics; `"rootband"` forces PMV root-traced
  bands; `"isoband"` draws filled contour bands; `"tile"` keeps the
  rectangular tile fallback.

- levels:

  Number of filled contour bands, or a numeric vector of band breaks.
  Ignored for `method = "tile"`.

- gap:

  Relative gap between generated tiles for `method = "tile"`.

- alpha:

  Overlay transparency. Defaults to `0.55` so psychrometric chart grid
  and relative-humidity curves remain visible beneath the comfort
  overlay. For `geom_comfort_standard_zone()`, an `alpha` supplied
  through `...` overrides the standard-specific defaults.

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

- show_labels:

  If `TRUE`, draw overlay labels.

- breaks:

  Contour break values.

- contour_method:

  Contour drawing method. `"auto"` uses root-traced curves for PMV and
  isobands for other metrics.

- label:

  A single logical value. If `TRUE`, label contour lines with their
  level values.

- label_size:

  Text size for contour labels. Defaults to `2.8`.

- range:

  Comfort value interval for PMV and SET zones.

- label_sensation:

  If `TRUE`, label integer PMV curves with thermal sensation text.

- label_axis:

  If `TRUE`, label each PMV curve near the x-axis.

- axis_label_hjust, axis_label_vjust:

  Position adjustment for PMV numeric labels near the x-axis. By
  default,
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  computes a shared baseline above the x-axis and offsets labels above
  their PMV lines.

- sensation_label_hjust, sensation_label_vjust:

  Position and vertical adjustment for thermal sensation labels.

- axis_label_size, sensation_label_size:

  Text size for PMV numeric and thermal sensation labels.

- padding:

  Gap padding around labels, passed as a grid unit.

- standard:

  A PMV-based comfort standard object.

- strategy:

  A Givoni bioclimatic strategy object.

- show_pmv:

  If `TRUE`, draw the PMV comfort background under the Givoni strategy
  outlines.

- pmv_model:

  PMV model used when `show_pmv = TRUE`.

- zone_alpha:

  Alpha for the filled Givoni comfort zone. Other Givoni strategy
  regions are drawn as outlines.

- zone_style:

  Optional named list of per-zone style overrides for
  `geom_comfort_givoni()`. Names must match Givoni zone ids such as
  `"comfort"`, `"winter"`, or `"air_conditioning"`. Values can be
  created with
  [`element_comfort_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/element_comfort_zone.md),
  [`ggplot2::element_polygon()`](https://ggplot2.tidyverse.org/reference/element.html),
  or ordinary named lists with fields `fill`, `colour`/`color`,
  `linewidth`, `linetype`, `alpha`, and `linejoin`.

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    `geom_point()`, give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

## Examples

``` r
# Draw filled PMV comfort bands.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_overlay(n = c(45, 30)) +
    scale_fill_comfort_pmv(name = "PMV")


# Draw PMV contour lines.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_contour(
        breaks = c(-1, 0, 1),
        n = 50
    )


# Draw the neutral PMV comfort zone.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_zone(
        range = c(-0.5, 0.5),
        n = c(45, 30),
        alpha = 0.3
    )


# Evaluate PMV at supplied state points.
states <- data.frame(
    tdb = c(24, 28, 31),
    relhum = c(45, 55, 65)
)
ggpsychro(states, tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    stat_comfort_state(
        aes(tdb = tdb, relhum = relhum, colour = after_stat(pmv)),
        size = 3
    )

```
