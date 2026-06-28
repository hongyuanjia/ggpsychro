# Givoni bioclimatic strategy

`comfort_strategy_givoni()` stores the fixed inputs used by
[`geom_comfort_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md).
The strategy geometry follows Marsh's Givoni Bioclimatic Chart overlay:
the mean outdoor temperature shifts the base comfort zone, and zones are
drawn in dry-bulb/relative-humidity space before conversion to humidity
ratio.

## Usage

``` r
comfort_strategy_givoni(mean_outdoor = 19, units = c("SI", "IP"))
```

## Arguments

- mean_outdoor:

  Mean outdoor temperature.

- units:

  Unit system for `mean_outdoor`, `"SI"` or `"IP"`.

## Value

A Givoni comfort strategy object.
