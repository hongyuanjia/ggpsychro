# Polygon theme element for psychrometric chart panels

`element_polygon()` is used by ggpsychro-specific theme elements such as
`psychro.panel.background` and `psychro.panel.mask`.

## Usage

``` r
element_polygon(
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  color = NULL,
  inherit.blank = FALSE
)
```

## Arguments

- fill:

  Fill colour.

- colour, color:

  Border colour. `color` is an alias for `colour`.

- size:

  Border width.

- linetype:

  Border linetype.

- inherit.blank:

  Whether this element inherits from blank elements.

## Value

A ggplot2 theme element.
