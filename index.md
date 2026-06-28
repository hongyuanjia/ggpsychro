# ggpsychro

> ‘ggplot2’ extension for making psychrometric charts.

## Features

- ggplot2-native psychrometric charts with SI/IP units and Mollier
  orientation.
- Preset grid and style helpers for common psychrometric chart layouts.
- ASHRAE-style psychrometric protractor for sensible heat ratio and
  heat-moisture-ratio guides in the chart mask area.
- Psychrometric stats for relative humidity, wet-bulb temperature, vapor
  pressure, specific volume, enthalpy, and humidity ratio.
- State points, process lines, and filled operating zones for HVAC
  workflows.
- Thermal comfort overlays for PMV, SET, adaptive comfort, and PMV-based
  ASHRAE 55 / EN 15251 comfort zones.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_github("hongyuanjia/ggpsychro")
```

## Example

The creation of a psychrometric chart starts with
[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md).
The result is a ggplot object, so grids, stats, geoms, scales, and
themes can be added with the same `+` workflow used by ggplot2.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    psychro_preset("minimal")
```

![Minimal psychrometric chart with dry-bulb temperature, humidity ratio,
and reference grids.](reference/figures/README-ggpsychro-1.png)

Comfort overlays can be added as regular layers. The PMV overlay
combines root-traced filled bands, PMV isolines, and text labels on the
same psychrometric chart.

``` r

ggpsychro(tdb_lim = c(5, 40), hum_lim = c(0, 24)) +
    psychro_preset("minimal") +
    geom_comfort_overlay(n = c(70, 48), gap = 0) +
    scale_fill_comfort_pmv(name = "PMV") +
    geom_comfort_pmv_lines(levels = seq(-3, 3, by = 0.5), n = 140)
```

![Psychrometric chart with a PMV comfort overlay, filled blue-white-red
PMV bands, and labelled PMV contour
lines.](reference/figures/README-comfort-overlay-1.png)

## Learn more

The longer examples live on the pkgdown site:

- [Get
  started](https://hongyuanjia.github.io/ggpsychro/articles/ggpsychro.html) -
  build basic charts, choose limits and units, use Mollier orientation,
  and add ggplot layers.
- [Chart grids and
  styles](https://hongyuanjia.github.io/ggpsychro/articles/grids-and-styles.html) -
  tune dry-bulb, humidity-ratio, relative-humidity, wet-bulb, enthalpy,
  and specific-volume grid guides, plus the psychrometric protractor.
- [Plotting psychrometric
  data](https://hongyuanjia.github.io/ggpsychro/articles/plotting-data.html) -
  plot points, bins, and overlays from weather or measured state data.
- [Comfort
  overlays](https://hongyuanjia.github.io/ggpsychro/articles/comfort-overlays.html) -
  draw PMV, SET, adaptive comfort, and PMV-based standard comfort
  overlays.
- [Zones and
  processes](https://hongyuanjia.github.io/ggpsychro/articles/zones-and-processes.html) -
  draw manual comfort regions, operating limits, state points, and HVAC
  process lines.

## Author

Hongyuan Jia

## License

The project is released under the terms of MIT License.

Copyright © 2019-2026 Hongyuan Jia

## Contribute

Please note that the ‘ggpsychro’ project is released with a [Contributor
Code of
Conduct](https://hongyuanjia.github.io/ggpsychro/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
