# Demonstrate psychrometric label and scale functions

This helper builds a compact ggplot2 scale preview for label and scale
functions.

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
