
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpsychro

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/ggpsychro?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/ggpsychro)
[![Travis build
status](https://travis-ci.com/hongyuanjia/ggpsychro.svg?branch=master)](https://travis-ci.com/hongyuanjia/ggpsychro)
[![Codecov test
coverage](https://codecov.io/gh/hongyuanjia/ggpsychro/branch/master/graph/badge.svg)](https://codecov.io/gh/hongyuanjia/ggpsychro?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggpsychro)](https://CRAN.R-project.org/package=ggpsychro)
<!-- badges: end -->

> ‘ggplot2’ extension for making pyschrometric charts.

## Installation

<!-- You can install the released version of ggpsychro from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("ggpsychro") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hongyuanjia/ggpsychro")
```

## Example

### The basic

Similar with ggplot2, the creation of a psychrometric chart using
ggpsychro starts with function `ggpsychro()`. You can specify the range
of dry-bulb temperature and humidity ratio using `tab_lim` and `hum_lim`
and the unit system using `units`

``` r
ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50), units = "IP")
```

<img src="man/figures/README-ggpsychro-1.png" width="60%" style="display: block; margin: auto;" />

You can also create a mollier chart by setting `mollier` to `TRUE`.

``` r
ggpsychro(mollier = TRUE)
```

<img src="man/figures/README-mollier-1.png" width="60%" style="display: block; margin: auto;" />

By default, only dry-bulb temperature, humidity ratio and the saturation
line are plotted.

The style of the saturation and area above it can be further styled
using `sat_style` and `mask_style`.

``` r
ggpsychro(mask_style = list(fill = "gray"), sat_style = list(color = "black", linetype = 2))
```

<img src="man/figures/README-style-1.png" width="60%" style="display: block; margin: auto;" />

### Grid

ggpsychro introduces new ggplot geoms to add 5 more grids onto the base
plot:

| Geom                 | Type                 |
| -------------------- | -------------------- |
| `geom_grid_relhum`   | Relative humidity    |
| `geom_grid_wetbulb`  | Wet-bulb temperature |
| `geom_grid_vappres`  | Vaper pressure       |
| `geom_grid_specvol`  | Specific volume      |
| `geom_grid_enthalpy` | Enthalpy             |

All of the new geoms accept a `step` argument to specify the interval
between each value.

``` r
ggpsychro() +
    geom_grid_relhum(step = 20) +
    geom_grid_wetbulb(step = 15) +
    geom_grid_vappres(step = 1000) +
    geom_grid_specvol(step = 0.05) +
    geom_grid_enthalpy(step = 50)
```

<img src="man/figures/README-grid-1.png" width="60%" style="display: block; margin: auto;" />

Every geom comes together with an

### Stat

ggpsychro provides 5 new ggplot stats to use together with other common
ggplot2 geoms::

| Stat            | Type                 |
| --------------- | -------------------- |
| `stat_relhum`   | Relative humidity    |
| `stat_wetbulb`  | Wet-bulb temperature |
| `stat_vappres`  | Vaper pressure       |
| `stat_specvol`  | Specific volume      |
| `stat_enthalpy` | Enthalpy             |

This makes it quick easy to add new elements.

Working together with ggplot2 orignal geoms is as simple as change
`stat` to your variable name of interest.

``` r
library(ggplot2)
library(eplusr) # for reading EPW data
epw <- read_epw(file.path(eplus_config(8.8)$dir, "WeatherData/USA_CO_Golden-NREL.724666_TMY3.epw"))
data <- epw$data()

ggpsychro(epw$data()) +
    geom_grid_relhum(step = 10) +
    geom_point(aes(dry_bulb_temperature, relative_humidity), stat = "rel_hum", alpha = 0.1) +
    facet_wrap(~month, labeller = as_labeller(function (x) paste0("Month: ", x)))
```

<img src="man/figures/README-stat-1-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 30)) +
    # 18 wet-bulb line with dry-bulb from 20 - 30
    geom_line(aes(x = 20:30, y = 18), stat = "wet_bulb") +
    # temperature range 24-28 with relative humidity from 40%-60%
    geom_ribbon(aes(x = 24:28, ymin = 40, ymax = 60), stat = "rel_hum", fill = "skyblue", color = "black")
```

<img src="man/figures/README-stat-2-1.png" width="60%" style="display: block; margin: auto;" />

## Author

Hongyuan Jia

## License

The project is released under the terms of MIT License.

Copyright © 2019 Hongyuan Jia

## Contribute

Please note that the ‘epwshiftr’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
