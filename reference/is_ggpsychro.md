# Test for ggpsychro plots

Test for ggpsychro plots

## Usage

``` r
is_ggpsychro(x)
```

## Arguments

- x:

  An object to test

## Value

A single logical value.

## Examples

``` r
is_ggpsychro(ggpsychro())
#> [1] TRUE
is_ggpsychro(ggplot2::ggplot())
#> [1] FALSE
```
