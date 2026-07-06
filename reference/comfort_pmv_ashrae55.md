# PMV-based comfort standards

These helpers describe static PMV-based comfort zones. They are distinct
from adaptive comfort models such as
[`comfort_model_adaptive()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_model_pmv.md),
which use running mean outdoor temperature and produce
operative-temperature bands.

## Usage

``` r
comfort_pmv_ashrae55(edition = "2017", range = c(-0.5, 0.5))

comfort_pmv_en15251(edition = "2007", breaks = c(-0.7, -0.2, 0.2, 0.7))
```

## Arguments

- edition:

  Standard edition. Currently `"2017"` for ASHRAE 55 and `"2007"` for EN
  15251.

- range:

  PMV comfort interval for ASHRAE 55.

- breaks:

  PMV boundaries for EN 15251 comfort bands.

## Value

A comfort standard object.

## Examples

``` r
# Create the ASHRAE 55 PMV comfort interval.
comfort_pmv_ashrae55()
#> $name
#> [1] "ashrae55_2017"
#> 
#> $breaks
#> [1] -0.5  0.5
#> 
#> $fills
#> [1] "#5BD96A"
#> 
#> $alphas
#> [1] 0.58
#> 
#> attr(,"class")
#> [1] "PsyComfortStandard" "list"              

# Create the EN 15251 PMV comfort bands.
comfort_pmv_en15251()
#> $name
#> [1] "en15251_2007"
#> 
#> $breaks
#> [1] -0.7 -0.2  0.2  0.7
#> 
#> $fills
#> [1] "#9BE89D" "#39D84A" "#9BE89D"
#> 
#> $alphas
#> [1] 0.34 0.58 0.34
#> 
#> attr(,"class")
#> [1] "PsyComfortStandard" "list"              

# Draw the ASHRAE 55 comfort zone.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_pmv(
        standard = comfort_pmv_ashrae55(),
        bands = FALSE,
        contours = FALSE,
        n = 80
    )


# Draw the EN 15251 comfort bands.
ggpsychro(tdb_lim = c(15, 35), hum_lim = c(0, 24)) +
    geom_comfort_pmv(
        standard = comfort_pmv_en15251(),
        bands = FALSE,
        contours = FALSE,
        n = 80
    )

```
