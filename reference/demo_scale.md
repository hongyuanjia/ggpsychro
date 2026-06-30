# Demonstrate scales functions with ggplot2 code

This function generates ggplot2 code needed to use scales functions for
real code.

## Usage

``` r
demo_scale(x, ...)
```

## Arguments

- x:

  A vector of data

- ...:

  Other arguments pass to scale functions

## Value

A ggplot object demonstrating the supplied scale settings.

## Examples

``` r
demo_scale(0:10, labels = scales::label_number())

```
