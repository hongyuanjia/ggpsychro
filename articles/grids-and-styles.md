# Chart grids and styles

ggpsychro draws the dry-bulb grid, humidity-ratio grid, saturation
curve, and psychrometric reference grids through
[`coord_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/coord_psychro.md).
The default reference grids are drawn without labels. Add
`geom_grid_*()` helpers when a grid should be explicit and labelled.

## Presets

[`psychro_preset()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_preset.md)
applies a named collection of scales, labelled reference grids, and a
matching theme. The built-in presets are `"ashrae"` and `"minimal"`.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    psychro_preset("ashrae")
```

![ASHRAE-style psychrometric chart with labelled reference
grids.](grids-and-styles_files/figure-html/ashrae-preset-1.png)

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 40)) +
    psychro_preset("minimal")
```

![Minimal psychrometric chart with selected labelled reference
grids.](grids-and-styles_files/figure-html/minimal-preset-1.png)

Use `labels = FALSE` when you want the preset’s grid visibility and
theme but no grid labels.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    psychro_preset("ashrae", labels = FALSE)
```

![ASHRAE-style psychrometric chart with reference grid labels
hidden.](grids-and-styles_files/figure-html/preset-no-labels-1.png)

## Reference grid helpers

The reference grid helpers are:

| Helper | Property |
|----|----|
| [`geom_grid_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md) | Relative humidity |
| [`geom_grid_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md) | Wet-bulb temperature |
| [`geom_grid_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md) | Vapor pressure |
| [`geom_grid_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md) | Specific volume |
| [`geom_grid_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md) | Enthalpy |

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 40)) +
    geom_grid_relhum() +
    geom_grid_wetbulb() +
    geom_grid_vappres() +
    geom_grid_specvol() +
    geom_grid_enthalpy()
```

![Psychrometric chart with relative humidity, wet-bulb, vapor pressure,
specific volume, and enthalpy
grids.](grids-and-styles_files/figure-html/all-grids-1.png)

These helpers do not add data layers. They update the psychrometric
coordinate metadata so the panel background can draw or hide the
selected grid.

## Breaks, labels, and style

Each reference grid has a matching `scale_*_continuous()` function. Use
these scales to customize breaks, minor breaks, limits, and labels.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 40)) +
    geom_grid_relhum(linewidth = 0.8, color = "black", label.size = 5) +
    scale_relhum_continuous(breaks = seq(25, 75, by = 25), minor_breaks = NULL) +
    geom_grid_wetbulb(color = "black", linewidth = 0.6, label = FALSE) +
    scale_wetbulb_continuous(breaks = seq(5, 30, by = 5), minor_breaks = NULL) +
    geom_grid_vappres(show = FALSE) +
    scale_specvol_continuous(labels = NULL) +
    geom_grid_enthalpy()
```

![Psychrometric chart with custom grid breaks, labels, and line
styles.](grids-and-styles_files/figure-html/custom-grid-1.png)

Line style arguments go directly to
[`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html).
Label style arguments use a `label.` prefix, such as `label.size`,
`label.color`, and `label.vjust`.

## Psychrometric protractor

[`geom_psychro_protractor()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_protractor.md)
adds the ASHRAE-style sensible heat ratio and heat-moisture-ratio
protractor in the chart mask area. The parent chart decides the
orientation: regular charts place it in the upper-left mask area, while
Mollier charts rotate the same protractor into the lower-right mask
area.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    psychro_preset("ashrae") +
    geom_psychro_protractor()
```

![ASHRAE-style psychrometric chart with a sensible heat ratio and
heat-moisture-ratio protractor in the upper-left mask
area.](grids-and-styles_files/figure-html/psychro-protractor-1.png)

Use `scale` to resize the whole protractor and a length-2 `margin` to
adjust the horizontal and vertical mask-area offsets. Tick breaks and
labels are controlled by
[`guide_psychro_protractor()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_protractor.md).

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30), mollier = TRUE) +
    psychro_preset("minimal") +
    geom_psychro_protractor(
        scale = 0.85,
        margin = c(0.06, 0.12),
        guide = guide_psychro_protractor(
            shr_breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
            shr_minor_breaks = seq(0.1, 0.9, by = 0.1),
            ratio_labels = NULL
        )
    )
```

![Mollier chart with a smaller psychrometric protractor in the
lower-right mask area and only sensible heat ratio labels
shown.](grids-and-styles_files/figure-html/psychro-protractor-custom-1.png)

## Theme elements

ggpsychro adds psychrometric theme elements for panel masks and
reference grids. Use these with regular
[`theme()`](https://hongyuanjia.github.io/ggpsychro/reference/theme.md)
calls.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    geom_grid_relhum(label = FALSE) +
    theme(
        psychro.panel.grid.saturation = element_line(
            color = "black", linetype = 2
        ),
        psychro.panel.mask = element_polygon(fill = "gray90", color = NA)
    )
```

![Psychrometric chart with custom saturation line and gray panel mask
theme
elements.](grids-and-styles_files/figure-html/theme-elements-1.png)
