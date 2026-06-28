# Get started

ggpsychro creates psychrometric charts with the same additive grammar
used by ggplot2. Start with
[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md),
choose the chart limits and units, and then add grids, scales, stats,
geoms, and themes with `+`.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30))
```

![Default psychrometric chart with dry-bulb temperature and humidity
ratio axes.](ggpsychro_files/figure-html/basic-chart-1.png)

## Chart limits and units

`tdb_lim` controls the dry-bulb temperature range. `hum_lim` controls
the displayed humidity ratio range. SI units are used by default.

``` r

ggpsychro(tdb_lim = c(10, 35), hum_lim = c(0, 25), units = "SI")
```

![SI psychrometric chart limited to 10 to 35 degrees C and 0 to 25 g per
kg humidity ratio.](ggpsychro_files/figure-html/si-chart-1.png)

Use `units = "IP"` for IP charts. The dry-bulb limits are in degree F
and the humidity-ratio limits are in grains per pound of dry air.

``` r

ggpsychro(tdb_lim = c(32, 122), hum_lim = c(0, 220), units = "IP")
```

![IP psychrometric chart limited to 32 to 122 degrees F and 0 to 220
grains per pound humidity
ratio.](ggpsychro_files/figure-html/ip-chart-1.png)

## Mollier charts

Set `mollier = TRUE` to swap the chart into Mollier orientation.

``` r

ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30), mollier = TRUE)
```

![Mollier chart with humidity ratio on the x axis and dry-bulb
temperature on the y
axis.](ggpsychro_files/figure-html/mollier-chart-1.png)

## Explicit coordinates

[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
sets up the psychrometric coordinate system for you. If you are building
up a plot in smaller pieces,
[`coord_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/coord_psychro.md)
can also be added explicitly.

``` r

ggpsychro() +
    coord_psychro(tdb_lim = c(10, 35), hum_lim = c(0, 25), units = "SI")
```

![Psychrometric chart created by adding coord_psychro
explicitly.](ggpsychro_files/figure-html/coord-psychro-1.png)

## Add ggplot layers

Because the result is a ggplot object, regular ggplot layers work as
usual.

``` r

states <- data.frame(
    dry_bulb_temperature = c(18, 23, 28, 31),
    humidity_ratio = c(8, 9.5, 10.5, 13)
)

ggpsychro(states, aes(dry_bulb_temperature, humidity_ratio),
    tdb_lim = c(0, 40), hum_lim = c(0, 25)
) +
    psychro_preset("minimal") +
    geom_point(color = "#0f766e", size = 2)
```

![Minimal psychrometric chart with four state points added as a scatter
layer.](ggpsychro_files/figure-html/add-points-1.png)

For data recorded as relative humidity, wet-bulb temperature, vapor
pressure, specific volume, or enthalpy, see [Plotting psychrometric
data](https://hongyuanjia.github.io/ggpsychro/articles/plotting-data.md).
