# Draw psychrometric zones

`stat_psychro_zone()` samples psychrometric boundary curves and returns
polygon coordinates. `geom_psychro_zone()` draws those polygons.

## Usage

``` r
geom_psychro_zone(
  mapping = NULL,
  data = NULL,
  stat = "psychro_zone",
  position = "identity",
  ...,
  type = "dbt-rh",
  n = 100L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_psychro_zone(
  mapping = NULL,
  data = NULL,
  geom = "polygon",
  position = "identity",
  ...,
  type = "dbt-rh",
  n = 100L,
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

- type:

  A zone type.

- n:

  Number of samples used for computed zone boundaries.

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

## Details

Supported zone types are:

- `"dbt-rh"`: `tdb_min`, `tdb_max`, `relhum_min`, and `relhum_max`

- `"enthalpy-rh"`: `enthalpy_min`, `enthalpy_max`, `relhum_min`, and
  `relhum_max`

- `"specvol-rh"` or `"volume-rh"`: `specvol_min`, `specvol_max`,
  `relhum_min`, and `relhum_max`

- `"dbt-wmax"`: `tdb_min`, `tdb_max`, `humratio_max`, and optional
  `humratio_min`

- `"xy-points"`: `tdb`, `humratio`, and optional `group`

`relhum` values are supplied in percent. `humratio` values are supplied
in chart display units: g/kg in SI and gr/lb in IP.

## Examples

``` r
comfort <- data.frame(
    name = c("cool", "warm"),
    tdb_min = c(20, 24),
    tdb_max = c(26, 32),
    relhum_min = c(35, 45),
    relhum_max = c(60, 75)
)

ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
    geom_psychro_zone(
        aes(tdb_min = tdb_min, tdb_max = tdb_max,
            relhum_min = relhum_min, relhum_max = relhum_max,
            fill = name),
        data = comfort,
        type = "dbt-rh",
        alpha = 0.28,
        colour = NA
    )


# Dry-bulb range with humidity-ratio limits.
humidity_cap <- data.frame(
    tdb_min = 10,
    tdb_max = 34,
    humratio_min = 4,
    humratio_max = 12
)
ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
    geom_psychro_zone(
        aes(tdb_min = tdb_min, tdb_max = tdb_max,
            humratio_min = humratio_min, humratio_max = humratio_max),
        data = humidity_cap,
        type = "dbt-wmax",
        alpha = 0.25
    )


# Property-bounded zones.
enthalpy_zone <- data.frame(
    enthalpy_min = 40000,
    enthalpy_max = 65000,
    relhum_min = 30,
    relhum_max = 80
)
ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
    geom_psychro_zone(
        aes(enthalpy_min = enthalpy_min, enthalpy_max = enthalpy_max,
            relhum_min = relhum_min, relhum_max = relhum_max),
        data = enthalpy_zone,
        type = "enthalpy-rh",
        alpha = 0.25
    )


# Draw an already-specified polygon in chart display units.
polygon_zone <- data.frame(
    tdb = c(20, 26, 28),
    humratio = c(6, 8, 6)
)
ggpsychro(tdb_lim = c(0, 40), hum_lim = c(0, 25)) +
    geom_psychro_zone(
        aes(tdb = tdb, humratio = humratio),
        data = polygon_zone,
        type = "xy-points",
        alpha = 0.25
    )
```
