# Thermal comfort calculations

These functions evaluate common comfort models without requiring Python
at runtime. Relative humidity is always supplied in percent. Temperature
and air-speed inputs follow `units`: SI uses degree C and m/s; IP uses
degree F and ft/s.

## Usage

``` r
comfort_pmv(
  tdb,
  tr = tdb,
  vr = 0.1,
  rh,
  met = 1.2,
  clo = 0.5,
  wme = 0,
  units = c("SI", "IP"),
  limit_inputs = TRUE,
  round_output = TRUE
)

comfort_set(
  tdb,
  tr = tdb,
  v = 0.1,
  rh,
  met = 1.2,
  clo = 0.5,
  wme = 0,
  units = c("SI", "IP"),
  limit_inputs = TRUE,
  round_output = TRUE,
  body_surface_area = 1.8258,
  p_atm = 101325,
  position = c("standing", "sitting")
)

comfort_adaptive(
  tdb,
  tr = tdb,
  t_running,
  v = 0.1,
  standard = c("ashrae55", "en16798"),
  category = NULL,
  units = c("SI", "IP"),
  limit_inputs = TRUE,
  round_output = TRUE
)
```

## Arguments

- tdb:

  Dry-bulb air temperature.

- tr:

  Mean radiant temperature. Defaults to `tdb`.

- vr:

  Relative air speed for PMV.

- rh:

  Relative humidity in percent.

- met:

  Metabolic rate in met.

- clo:

  Clothing insulation in clo.

- wme:

  External work in met.

- units:

  Unit system, `"SI"` or `"IP"`.

- limit_inputs:

  If `TRUE`, values outside the model applicability range are returned
  as `NA`.

- round_output:

  If `TRUE`, round outputs like `pythermalcomfort`.

- v:

  Air speed for SET and adaptive comfort.

- body_surface_area:

  Body surface area in square meters for SET.

- p_atm:

  Atmospheric pressure in Pa.

- position:

  Body position, `"standing"` or `"sitting"`.

- t_running:

  Running mean outdoor temperature for adaptive comfort.

- standard:

  Adaptive comfort standard, either `"ashrae55"` or `"en16798"`.

- category:

  Comfort category. For ASHRAE 55 use `"80"` or `"90"`; for EN 16798 use
  `"I"`, `"II"`, or `"III"`.

## Value

A data frame with model outputs.

## Examples

``` r
comfort_pmv(25, rh = 50, met = 1.4, clo = 0.5)
#>    pmv ppd     tsv
#> 1 0.41 8.5 Neutral
comfort_set(25, rh = 50)
#>    set
#> 1 24.3
comfort_adaptive(25, t_running = 20)
#>   standard tmp_cmf tmp_cmf_80_low tmp_cmf_80_up tmp_cmf_90_low tmp_cmf_90_up
#> 1 ashrae55      24           20.5          27.5           21.5          26.5
#>   acceptability_80 acceptability_90 lower upper acceptability
#> 1             TRUE             TRUE  20.5  27.5          TRUE
```
