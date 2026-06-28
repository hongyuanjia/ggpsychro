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
