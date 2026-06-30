# Reports whether x is a ggplot object

Reports whether x is a ggplot object

## Usage

``` r
is.ggpsychro(x)
```

## Arguments

- x:

  An object to test

## Value

A single logical value.

## Examples

``` r
is.ggpsychro(ggpsychro())
#> [1] TRUE
is.ggpsychro(ggplot2::ggplot())
#> [1] FALSE
```
