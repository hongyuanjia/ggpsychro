# PMV-based comfort standards

These helpers describe static PMV-based comfort zones. They are distinct
from adaptive comfort models such as
[`comfort_model_adaptive()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_model_pmv.md),
which use running mean outdoor temperature and produce
operative-temperature bands.

## Usage

``` r
comfort_standard_ashrae55_2017(range = c(-0.5, 0.5))

comfort_standard_en15251_2007(breaks = c(-0.7, -0.2, 0.2, 0.7))
```

## Arguments

- range:

  PMV comfort interval for ASHRAE 55.

- breaks:

  PMV boundaries for EN 15251 comfort bands.

## Value

A comfort standard object.

## Examples

``` r
# Create the ASHRAE 55 PMV comfort interval.
comfort_standard_ashrae55_2017()
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
comfort_standard_en15251_2007()
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
```
