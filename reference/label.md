# Label wet-bulb temperature

Format numbers as main variables on the psychrometric chart.

## Usage

``` r
label_drybulb(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

label_humratio(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

label_relhum(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

label_wetbulb(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

label_vappres(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

label_specvol(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

label_enthalpy(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

drybulb_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

humratio_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

relhum_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

wetbulb_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

vappres_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

specvol_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)

enthalpy_format(
  x,
  accuracy = NULL,
  scale = 1,
  units,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  parse = FALSE,
  ...
)
```

## Arguments

- x:

  A numeric vector

- accuracy:

  A number to round to. Use (e.g.) `0.01` to show 2 decimal places of
  precision. If `NULL`, the default, uses a heuristic that should ensure
  breaks have the minimum number of digits needed to show the difference
  between adjacent values.

  Applied to rescaled data.

- scale:

  A scaling factor: `x` will be multiplied by `scale` before formatting.
  This is useful if the underlying data is very small or very large.

- units:

  A single string indicating the unit system to use. Should be either
  `"SI"` or `"IP"`

- big.mark:

  Character used between every 3 digits to separate thousands. The
  default (`NULL`) retrieves the setting from the [number
  options](https://scales.r-lib.org/reference/number_options.html).

- decimal.mark:

  The character to be used to indicate the numeric decimal point. The
  default (`NULL`) retrieves the setting from the [number
  options](https://scales.r-lib.org/reference/number_options.html).

- trim:

  Logical, if `FALSE`, values are right-justified to a common width (see
  [`base::format()`](https://rdrr.io/r/base/format.html)).

- parse:

  If `TRUE`, the labels will be parsed into expressions and displayed as
  described in [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html).
  Default: `FALSE`.

- ...:

  Other arguments passed on to
  [`base::format()`](https://rdrr.io/r/base/format.html).

## Value

A labelling function that formats numeric breaks.

## Examples

``` r
demo_scale(10:50, labels = label_drybulb(units = "SI", parse = TRUE))

demo_scale(10:50, labels = label_drybulb(units = "IP", parse = TRUE))


demo_scale(10:20, labels = label_humratio(scale = 0.001, units = "SI", parse = TRUE))

demo_scale(10:20, labels = label_humratio(scale = 0.007, units = "IP", parse = TRUE))


demo_scale(10:50, labels = label_relhum(units = "SI"))

demo_scale(10:50, labels = label_relhum(units = "IP"))


demo_scale(10:50, labels = label_wetbulb(units = "SI", parse = TRUE))

demo_scale(10:50, labels = label_wetbulb(units = "IP", parse = TRUE))


demo_scale(10:50, labels = label_specvol(units = "SI", parse = TRUE))

demo_scale(10:50, labels = label_specvol(units = "IP", parse = TRUE))


demo_scale(10:50, labels = label_vappres(units = "SI"))

demo_scale(10:50, labels = label_vappres(units = "IP"))


demo_scale(seq(1000, 2000), labels = label_enthalpy(units = "SI", parse = TRUE))

demo_scale(seq(1000, 2000), labels = label_enthalpy(units = "IP", parse = TRUE))


demo_scale(10:50, labels = drybulb_format(units = "SI", parse = TRUE))

demo_scale(10:20, labels = humratio_format(scale = 0.001, units = "SI", parse = TRUE))

demo_scale(10:50, labels = relhum_format(units = "SI"))

demo_scale(10:50, labels = wetbulb_format(units = "SI", parse = TRUE))

demo_scale(10:50, labels = specvol_format(units = "SI", parse = TRUE))

demo_scale(10:50, labels = vappres_format(units = "SI"))

demo_scale(seq(1000, 2000), labels = enthalpy_format(units = "SI", parse = TRUE))

```
