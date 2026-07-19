# Givoni-Milne strategy overlay

`comfort_strategy_givoni()` stores the fixed inputs used by
[`geom_comfort_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_givoni.md).
The adaptive variant shifts the base comfort zone from a mean outdoor
temperature. The fixed variant anchors the comfort zone to the common
Givoni/Milne 1979 bounds: 20 to 25.5 degrees C dry-bulb temperature and
20% to 80% relative humidity with the hot-humid corner clipped. Strategy
zones are drawn in dry-bulb/relative-humidity space before conversion to
humidity ratio.

## Usage

``` r
comfort_strategy_givoni(
  mean_outdoor = 19,
  units = c("SI", "IP"),
  variant = c("adaptive", "fixed"),
  tdb_range = NULL,
  relhum_range = c(20, 80)
)
```

## Arguments

- mean_outdoor:

  Mean or running-mean outdoor temperature used to adapt the base
  comfort zone when `variant = "adaptive"`. It is ignored when
  `variant = "fixed"` and can be `NULL` for that variant.

- units:

  Unit system for `mean_outdoor`, `"SI"` or `"IP"`.

- variant:

  Givoni-Milne strategy variant. `"adaptive"` shifts the comfort anchor
  from `mean_outdoor`; `"fixed"` uses the fixed 1979 comfort anchor.

- tdb_range:

  Optional dry-bulb comfort-anchor range used when `variant = "fixed"`.
  Values use `units`. When `NULL`, the fixed variant uses 20 to 25.5
  degrees C.

- relhum_range:

  Relative-humidity comfort-anchor range in percent.

## Value

A Givoni-Milne comfort strategy object.

## Details

This overlay is a climate-screening and design-strategy aid. It should
not be interpreted as a comfort-standard compliance method or as a
substitute for building energy/thermal simulation. In particular, the
high-mass and night ventilation regions indicate potential strategy
ranges; using them to count comfort hours requires daily temperature
profiles, nighttime conditions, and building assumptions that are
outside this layer.

## References

Andrew Marsh, Psychrometric Chart,
<https://andrewmarsh.com/software/psychro-chart-web/>

Herb S, Wolk S, Reinhart C. Beyond the bioclimatic chart: An automated
simulation-based method for the assessment of natural ventilation and
passive design potential. Building and Environment, 269, 112362.
[doi:10.1016/j.buildenv.2024.112362](https://doi.org/10.1016/j.buildenv.2024.112362)

## Examples

``` r
# Create an adaptive Givoni-Milne strategy for a warm outdoor mean.
comfort_strategy_givoni(mean_outdoor = 22)
#> $mean_outdoor
#> [1] 22
#> 
#> $units
#> [1] "SI"
#> 
#> $variant
#> [1] "adaptive"
#> 
#> $tdb_range
#> NULL
#> 
#> $relhum_range
#> [1] 20 80
#> 
#> attr(,"class")
#> [1] "PsyComfortGivoniStrategy" "list"                    

# Use the fixed Givoni/Milne 1979 comfort anchor instead.
comfort_strategy_givoni(variant = "fixed", mean_outdoor = NULL)
#> $mean_outdoor
#> [1] NA
#> 
#> $units
#> [1] "SI"
#> 
#> $variant
#> [1] "fixed"
#> 
#> $tdb_range
#> NULL
#> 
#> $relhum_range
#> [1] 20 80
#> 
#> attr(,"class")
#> [1] "PsyComfortGivoniStrategy" "list"                    

# Or provide project-specific comfort anchor ranges.
comfort_strategy_givoni(
    variant = "fixed",
    mean_outdoor = NULL,
    tdb_range = c(22, 27),
    relhum_range = c(30, 70)
)
#> $mean_outdoor
#> [1] NA
#> 
#> $units
#> [1] "SI"
#> 
#> $variant
#> [1] "fixed"
#> 
#> $tdb_range
#> [1] 22 27
#> 
#> $relhum_range
#> [1] 30 70
#> 
#> attr(,"class")
#> [1] "PsyComfortGivoniStrategy" "list"                    

# Draw the Givoni strategy overlay for that outdoor mean.
ggpsychro(tdb_lim = c(5, 45), hum_lim = c(0, 30)) +
    geom_comfort_givoni(
        strategy = comfort_strategy_givoni(mean_outdoor = 22),
        labels = FALSE
    )

```
