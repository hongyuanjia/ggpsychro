# Add a psychrometric protractor

`geom_psychro_protractor()` adds a heat-moisture ratio and sensible heat
ratio reference protractor to the mask area of a
[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
plot. The orientation is determined by the parent chart: regular
psychrometric charts place the protractor in the upper-left mask area,
while Mollier charts rotate the same protractor geometry into the
lower-right mask area. `guide_psychro_protractor()` configures the
protractor ticks and labels.

## Usage

``` r
geom_psychro_protractor(
  ...,
  show = TRUE,
  label = TRUE,
  annotation = TRUE,
  scale = 1,
  radius = 0.24,
  margin = 0.08,
  guide = guide_psychro_protractor()
)

guide_psychro_protractor(
  shr_breaks = waiver(),
  shr_minor_breaks = waiver(),
  shr_labels = waiver(),
  ratio_breaks = waiver(),
  ratio_minor_breaks = waiver(),
  ratio_labels = waiver(),
  check_overlap = TRUE
)
```

## Arguments

- ...:

  Line style settings passed to
  [`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html),
  such as `colour`, `color`, `linewidth`, `size`, and `linetype`. Label
  style settings can be supplied with `label.*` names, such as
  `label.colour`, `label.color`, `label.size`, `label.alpha`,
  `label.family`, and `label.fontface`.

- show:

  A single logical value. If `FALSE`, hide the protractor.

- label:

  A single logical value. If `FALSE`, hide protractor labels.

- annotation:

  A single logical value, character vector, or expression vector. If
  `FALSE`, hide the helper formula text. Character and expression
  vectors must have length 2 and are used as the helper formula labels.

- scale:

  A single positive number for overall protractor scaling. It multiplies
  `radius`, protractor line width, and label text size; `margin` is not
  scaled.

- radius:

  A single number in panel coordinates controlling the protractor
  radius.

- margin:

  A single number or length-2 numeric vector in panel coordinates
  controlling the distance from the mask-area edge. When length 2, the
  values are horizontal and vertical margins, respectively.

- guide:

  A protractor guide created by `guide_psychro_protractor()`.

- shr_breaks, shr_minor_breaks:

  Numeric vectors controlling the labelled and unlabelled ticks for the
  sensible heat ratio axis. Use `NULL` to hide that tick tier.

- shr_labels, ratio_labels:

  A character vector, expression vector, function, `NULL`, or `waiver()`
  controlling labels for the corresponding major breaks. `waiver()` uses
  the default numeric labels, and `NULL` hides the labels.

- ratio_breaks, ratio_minor_breaks:

  Numeric vectors controlling the labelled and unlabelled ticks for the
  heat-moisture-ratio axis (`dh/dW`). Use `waiver()` for the
  ASHRAE-style defaults, or `NULL` to hide that tick tier.

- check_overlap:

  A single logical value. If `TRUE`, overlapping protractor labels are
  dropped by the grid text grob.

## Value

A ggplot addition.

## Examples

``` r
ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    geom_psychro_protractor()


ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30), mollier = TRUE) +
    geom_psychro_protractor()
```
