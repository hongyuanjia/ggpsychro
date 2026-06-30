# Create transformation objects for psychrometric chart

Create transformation objects for psychrometric chart

## Usage

``` r
drybulb_trans(units = "SI")

humratio_trans(units = "SI")

relhum_trans(units = "SI")

wetbulb_trans(units = "SI")

vappres_trans(units = "SI")

specvol_trans(units = "SI")

enthalpy_trans(units = "SI")
```

## Arguments

- units:

  A string indicating the system of units chosen. Should be either
  `"SI"` or `"IP"`.

## Value

A
[`scales::trans_new()`](https://scales.r-lib.org/reference/new_transform.html)
transformation object.

## Examples

``` r
plot(drybulb_trans("SI"), xlim = c(0, 5))

plot(humratio_trans("SI"), xlim = c(0, 1000))

plot(relhum_trans("SI"), xlim = c(0, 100))

plot(wetbulb_trans("SI"), xlim = c(-50, 40))

plot(vappres_trans("SI"), xlim = c(1000, 4000))

plot(specvol_trans("SI"), xlim = c(0.8, 1))

plot(enthalpy_trans("SI"), xlim = c(1000, 2000))
```
