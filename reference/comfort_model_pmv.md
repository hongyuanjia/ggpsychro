# Comfort model objects

Model objects capture the fixed inputs used by comfort layers. They can
be reused across overlays, contours, zones, and point states.

## Usage

``` r
comfort_model_pmv(
  tr = NULL,
  vr = 0.1,
  met = 1.2,
  clo = 0.5,
  wme = 0,
  model = "7730-2005",
  limit_inputs = FALSE,
  round_output = FALSE
)

comfort_model_set(
  tr = NULL,
  v = 0.1,
  met = 1.2,
  clo = 0.5,
  wme = 0,
  limit_inputs = FALSE,
  body_surface_area = 1.8258,
  p_atm = NULL,
  position = c("standing", "sitting"),
  round_output = FALSE
)

comfort_model_adaptive(
  t_running,
  tr = NULL,
  v = 0.1,
  standard = c("ashrae55", "en16798"),
  category = NULL,
  limit_inputs = TRUE,
  round_output = FALSE
)
```

## Arguments

- tr:

  Mean radiant temperature. Defaults to `tdb`.

- vr:

  Relative air speed for PMV.

- met:

  Metabolic rate in met.

- clo:

  Clothing insulation in clo.

- wme:

  External work in met.

- model:

  PMV model/version label. Currently `"7730-2005"` is implemented.

- limit_inputs:

  If `TRUE`, values outside the model applicability range are returned
  as `NA`.

- round_output:

  If `TRUE`, round model outputs. Comfort plot layers use unrounded
  values by default so contours and zones remain smooth.

- v:

  Air speed for SET and adaptive comfort.

- body_surface_area:

  Body surface area in square meters for SET.

- p_atm:

  Atmospheric pressure in Pa.

- position:

  Body position, `"standing"` or `"sitting"`.

- t_running:

  Running mean outdoor temperature.

- standard:

  Adaptive comfort standard.

- category:

  Adaptive comfort category.

## Value

A comfort model object.
